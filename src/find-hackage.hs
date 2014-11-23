{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

import Control.Applicative
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Trans.Control

import Data.Proxy
import Data.Word

import Web.Apiary.Heroku
import Network.Wai.Handler.Warp (run)

import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.IORef
import qualified Update
import Control.Concurrent.Lifted

import Network.HTTP.Date
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP

import Data.Apiary.Extension
import System.PosixCompat.Time

import qualified Data.Aeson as JSON
import Control.Lens hiding ((??))
import qualified Text.Show.ByteString as L
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.Aeson.Lens as JSON
import qualified Data.HashMap.Strict as H

import qualified Text.XML.Pugi as Pugi
import qualified Text.XML.Pugi.Mutable as XML

import Data.Time
import System.Locale
import qualified Data.Trie as Trie
import qualified StorePackage

import Web.Apiary.Helics
import System.Environment (getArgs)

#if DEVELOP
import Web.Apiary.Develop
#else
import Web.Apiary
#endif

data Elasticsearch = Elasticsearch HTTP.Request HTTP.Manager
instance Extension Elasticsearch

initElasticsearch :: Has Heroku es => Initializer IO es (Elasticsearch ': es)
initElasticsearch = initializerBracket $ \e m -> do
    url <- liftIO $ fmap (StorePackage.parseUrl' . T.unpack) <$> getHerokuEnv "BONSAI_URL" e
    case url of
        Just (Just r) -> HTTP.withManager HTTP.tlsManagerSettings $ \mgr -> m $ Elasticsearch r mgr
        _ -> fail "initElasticsearch: failed."

rawElasticsearchQuery :: (MonadHas Elasticsearch m, MonadIO m)
                      => (HTTP.Request -> HTTP.Request) -> m JSON.Value
rawElasticsearchQuery reqmod = do
    Elasticsearch req mgr <- getExt (Proxy :: Proxy Elasticsearch)
    r <- maybe (fail "Elasticsearch: json decode failed.") return .
        JSON.decode . HTTP.responseBody =<< liftIO (HTTP.httpLbs (reqmod req) mgr)
    return (r::JSON.Value)

searchPackage' :: (Has Elasticsearch exts, Has Arguments exts, MonadIO m)
               => Word -> T.Text -> ActionT exts prms m JSON.Value
searchPackage' skip query = do
    let body = JSON.object
            [ "query" JSON..= JSON.object
                [ "query_string" JSON..= JSON.object
                    [ "query"  JSON..= query
                    , "fields" JSON..= [ "name^5", "synopsis", "description" :: T.Text
                                       , "ngram.name", "ngram.synopsis", "ngram.description"
                                       ]
                    , "default_operator"             JSON..= ("AND" :: T.Text)
                    ]
                ]
            , "from" JSON..= skip
            ]
    p <- elasticRequestUrl "/package/_search" 
    rawElasticsearchQuery
        (\r -> r { HTTP.path        = p
                 , HTTP.requestBody = HTTP.RequestBodyLBS $ JSON.encode body
                 })

searchPackage :: (Has Elasticsearch exts, Has Arguments exts, MonadIO m, MonadBaseControl IO m)
              => Word -> T.Text -> ActionT exts prms m JSON.Value
searchPackage skip query =
    searchPackage' skip query
    `catch` (\(HTTP.StatusCodeException st h _) -> do
        guard (st == status400)
        contentType "text/plain"
        status st
        reset
        maybe (return ()) bytes $ lookup "X-Response-Body-Start" h
        stop)

esJSONToAPIJSON :: JSON.Value -> Maybe JSON.Value
esJSONToAPIJSON r = r ^? JSON.key "hits" . to (\o -> JSON.object
    [ "result" JSON..= (o ^. JSON.key "hits" . JSON._Array . folded . to eachResult)
    , "count"  JSON..= (o ^? JSON.key "total")
    , "pages"  JSON..= (o ^? JSON.key "total" . JSON._Double . to (ceiling . (/ 10)) :: Maybe Int)
    ])
  where
    eachResult o = maybe [] (:[]) $ do
        score <- o ^? JSON.key "_score"
        src   <- o ^? JSON.key "_source" . JSON._Object
        return $ H.insert "score" score (H.delete "ngram" src)

esJSONToAtom :: UTCTime -> Word -> S.ByteString -> JSON.Value -> Maybe Pugi.Document
esJSONToAtom t skip q r = do
    total <- r ^? JSON.key "hits" . JSON.key "total" . JSON._Integral
    let query = r ^. JSON.key "hits" . JSON.key "hits" . JSON._Array . folded . to eachResult
    createAtom
        (S8.pack $ formatTime defaultTimeLocale "%FT%TZ" t)
        "urn:uuid:1234D3D3-DE37-4512-AF9B-E5C0C9BBE246"
        total skip (succ $ skip `quot` 10) q query
  where
    eachResult o = maybe [] (:[]) $ do
        name <- o ^? JSON.key "_source" . JSON.key "name"     . JSON._String
        desc <- o ^? JSON.key "_source" . JSON.key "synopsis" . JSON._String
        return (T.encodeUtf8 name, T.encodeUtf8 desc)

createAtom :: S.ByteString -> S.ByteString -> Int -> Word -> Word -> S.ByteString
           -> [(S.ByteString, S.ByteString)] -> Maybe Pugi.Document
createAtom upd feedId total startI startP terms items = XML.create $ \doc -> do
    feed <- XML.appendElement "feed" doc
    XML.appendAttrs [("xmlns", "http://www.w3.org/2005/Atom"), ("xmlns:opensearch", "http://a9.com/-/spec/opensearch/1.1/")] feed
    _ <- XML.appendElement "title"   feed >>= XML.appendPCData "find-hackage search result"
    _ <- XML.appendElement "link"    feed >>= XML.appendAttr "href" "http://find-hackage.herokuapp.com"
    _ <- XML.appendElement "updated" feed >>= XML.appendPCData upd
    _ <- XML.appendElement "author"  feed >>= XML.appendElement "name" >>= XML.appendPCData "find-hackage"
    _ <- XML.appendElement "id"      feed >>= XML.appendPCData feedId
    _ <- XML.appendElement "opensearch:totalResults" feed >>= XML.appendPCData (L.toStrict $ L.show total)
    _ <- XML.appendElement "opensearch:startIndex"   feed >>= XML.appendPCData (L.toStrict $ L.show startI)
    _ <- XML.appendElement "opensearch:itemsPerPage" feed >>= XML.appendPCData "10"
    _ <- XML.appendElement "opensearch:Query"        feed >>=
        XML.appendAttrs [("role", "request"), ("searchTerms", terms), ("startPage", L.toStrict $ L.show startP)]
    forM_ items $ \i -> do
        item <- XML.appendElement "item" feed
        _ <- XML.appendElement "title"       item >>= XML.appendPCData (i ^. _1)
        _ <- XML.appendElement "link"        item >>= XML.appendPCData ("http://hackage.haskell.org/package/" `S.append` (i ^. _1))
        _ <- XML.appendElement "description" item >>= XML.appendPCData (i ^. _2)
        return ()

sentinel :: (MonadHas Elasticsearch m, MonadHas Arguments m, MonadIO m, MonadBaseControl IO m)
         => IORef (HTTPDate, Either SomeException Int) -> IORef Candidates -> Elasticsearch -> m ()
sentinel lastUpdated cands (Elasticsearch req mgr) = loop where
  loop = do
      (upd, _) <- liftIO $ readIORef lastUpdated
      p <- elasticRequestUrl "/package/_bulk"
      updateAction <-
        do new <- liftIO $ Update.updater (Just upd) req { HTTP.path = p } mgr
           nc  <- initCandidates
           liftIO $ writeIORef cands nc
           return $ \t -> liftIO $ writeIORef lastUpdated (t, Right $ length new)
        `catch` \(e :: SomeException) -> do
           return $ \t -> liftIO $ writeIORef lastUpdated (t, Left e)

      t <- liftIO $ epochTimeToHTTPDate <$> epochTime
      updateAction t
      liftIO $ threadDelay (10 * 60 * 10^(6::Int))
      loop

data Candidates = Candidates
    { candNames      :: Trie.Trie Int
    , candLicenses   :: Trie.Trie Int
    , candCategories :: Trie.Trie Int
    } deriving Show

initCandidates :: (MonadHas Elasticsearch m, MonadHas Arguments m, MonadIO m) => m Candidates
initCandidates = do
    let aggr f = f JSON..= JSON.object ["terms" JSON..= JSON.object ["field" JSON..= f, "size" JSON..= (0::Int)]]
        body   = JSON.object ["aggs" JSON..= JSON.object [aggr "raw.name", aggr "raw.license", aggr "raw.category"]]
    p  <- elasticRequestUrl "/package/_search" 
    nv <- rawElasticsearchQuery
        (\r -> r { HTTP.path        = p
                 , HTTP.requestBody = HTTP.RequestBodyLBS $ JSON.encode body
                 })
    let spToPlus ' ' = '+'
        spToPlus c   = c
        bucket f = JSON.key "aggregations" . JSON.key f . JSON.key "buckets" . JSON._Array . folded . to (\o -> do
            ky  <- o ^? JSON.key "key" . JSON._String . to (T.map spToPlus . T.toLower)
            cnt <- o ^? JSON.key "doc_count" . JSON._Integral
            return (T.encodeUtf8 ky, cnt :: Int)
            ) . traverse
    return $ Candidates
        { candNames      = Trie.fromList (nv ^.. bucket "raw.name")
        , candLicenses   = Trie.fromList (nv ^.. bucket "raw.license")
        , candCategories = Trie.fromList (nv ^.. bucket "raw.category")
        }

newtype Arguments = Arguments 
    { argIndexName :: S.ByteString
    }
instance Extension Arguments

elasticRequestUrl :: (MonadHas Arguments m, Monad m) => S.ByteString -> m S.ByteString
elasticRequestUrl url = getExt Proxy >>= \args ->
    return ('/' `S8.cons` argIndexName args `S8.append` url)

initArguments :: Initializer' IO Arguments
initArguments = initializer' $ getArgs >>= \case
    [idx] -> return $ Arguments (S8.pack idx)
    _     -> fail "find-hackage INDEX"

main :: IO ()
main = runHerokuWith run (initElasticsearch +> initHerokuHelics def {appName = "find-hackage"} +> initArguments) def $ do
    lastUpdated <- liftIO $ newIORef (defaultHTTPDate, Right 0 :: Either SomeException Int)
    cands <- liftIO . newIORef =<< initCandidates
    _ <- getExt (Proxy :: Proxy Elasticsearch) >>= fork . sentinel lastUpdated cands

    [capture|/updated|] . method GET . document "database status api" . action $ do
        (date, st) <- liftIO $ readIORef lastUpdated
        contentType "application/json"
        lazyBytes . JSON.encode . JSON.object $
            ("date" JSON..= T.decodeUtf8 (formatHTTPDate date)) : case st of
                Right c -> [ "count" JSON..= c,          "error" JSON..= JSON.Null ]
                Left  a -> [ "count" JSON..= (-1 ::Int), "error" JSON..= (show a)  ]

    [capture|/search|] . method GET
        . document "search api"
        . ([key|q|] ?? "query string" =: pText)
        . ([key|skip|] ?? "skip count" =?!: (0 :: Word)) $ do

            accept "application/json" . action $ do
                query <- param [key|q|]
                skip  <- param [key|skip|]
                r <- searchPackage skip query
                case esJSONToAPIJSON r of
                    Just json -> lazyBytes $ JSON.encode json
                    Nothing   -> do
                        status status500
                        lazyBytes "{\"error\":\"convert database json to response json failed.\"}"

            accept "application/atom+xml" . action $ do
                query <- param [key|q|]
                skip  <- param [key|skip|]
                r <- searchPackage skip query
                time <- liftIO getCurrentTime
                case esJSONToAtom time skip (T.encodeUtf8 query) r of
                    Just doc -> lazyBytes $ Pugi.pretty def doc
                    Nothing  -> do
                        status status500
                        contentType "text/plain"
                        bytes "convert database json to reponse atom failed."

    [capture|/suggestions|] . method GET . document "suggestions api"
        . ([key|q|] ?? "suggestions string" =: pText) . accept "application/x-suggestions+json" . action $ do
            query <- param [key|q|]
            let keywords = [ "name", "version", "license", "copyright", "maintainer", "author", "stability"
                           , "homepage", "bugReports", "synopsis", "description", "category", "hasLibrary"
                           , "executable", "deprecated", "hasExecutable", "inFavourOf"
                           , "ngram.name", "ngram.synopsis", "ngram.description"
                           ]
            cvals <- liftIO $ readIORef cands
            let getCands  str = map T.decodeUtf8 . Trie.keys . Trie.submap (T.encodeUtf8 str)
                boolCands str = filter (T.toLower str `T.isPrefixOf`) ["true", "false"]
                cs = case T.words query of
                    [] -> []
                    ws -> case T.break (== ':') $ last ws of
                        (w, "") -> filter (w `T.isPrefixOf`) (map (`T.snoc` ':') keywords) ++ getCands query (candNames cvals)
                        ("license",    pfx) -> map ("license:"  `T.append`) $ getCands (T.toLower $ T.tail pfx) (candLicenses   cvals)
                        ("name",       pfx) -> map ("name:"     `T.append`) $ getCands (T.toLower $ T.tail pfx) (candNames      cvals)
                        ("ngram.name", pfx) -> map ("name:"     `T.append`) $ getCands (T.toLower $ T.tail pfx) (candNames      cvals)
                        ("category",   pfx) -> map ("category:" `T.append`) $ getCands (T.toLower $ T.tail pfx) (candCategories cvals)
                        ("hasLibrary",    pfx) -> map ("hasLibrary:" `T.append`) $ boolCands (T.tail pfx)
                        ("hasExecutable", pfx) -> map ("hasExecutable:" `T.append`) $ boolCands (T.tail pfx)
                        ("deprecated",    pfx) -> map ("deprecated:" `T.append`) $ boolCands (T.tail pfx)
                        _                   -> []

            lazyBytes . JSON.encode $ [JSON.toJSON query, JSON.toJSON $ take 10 cs]

    pkgCount <- elasticRequestUrl "/package/_count"
    [capture|packages|] . method GET . document "get package count" . action $ do
        contentType "application/json"
        r <- rawElasticsearchQuery
            (\r -> r { HTTP.path        = pkgCount
                     , HTTP.requestBody = HTTP.RequestBodyBS "{\"query\": {\"match_all\": {}}}"
                     })
        case r ^? JSON.key "count" . JSON._Integer of
            Nothing -> status status500 >> lazyBytes "{\"error\":\"convert database json to response json failed.\"}"
            Just n  -> showing n

    root . method GET . action $ file "static/main.html" Nothing

    [capture|/opensearch.xml|] . method GET . action $ do
        contentType "application/opensearchdescription+xml"
        file' "static/opensearch.xml" Nothing

    [capture|/nop|] . action $ bytes "nop"

    [capture|**path|] . method GET . action $ do
        p <- param [key|path|]
        file (joinPath $ "static" : map T.unpack p) Nothing

    [capture|/api/documentation|] . method GET . action $
        defaultDocumentationAction def { documentGoogleAnalytics = Just "UA-48784415-5" }
