{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Update where

import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Maybe
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as H
import Text.XML.Pugi
import qualified Data.ByteString.Char8 as S
import Network.HTTP.Conduit as HTTP
import Network.HTTP.Types
import qualified Data.Aeson as JSON
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy as L
import qualified StorePackage
import Data.List
import Data.Function
import Network.HTTP.Date

import qualified Distribution.PackageDescription               as D
import qualified Distribution.PackageDescription.Configuration as D
import qualified Distribution.PackageDescription.Parse         as D

import Data.Time
import System.Locale

data Recent = Recent
    { recentName    :: S.ByteString
    , recentVersion :: S.ByteString
    , recentTime    :: UTCTime
    } deriving (Show, Ord, Eq)

parseRecents :: Document -> [Recent]
parseRecents = map maximum . groupBy ((==) `on` recentName) . sort .
    mapMaybe getElem . nodeSetToList . selectNodes [xpath|/rss/channel/item|]
  where
    getElem (Left n) = do
        (pkg, v) <- S.break (== ' ') . text <$> child "title" n
        pub      <- child "pubDate" n >>= parseTime defaultTimeLocale rfc822DateFormat . S.unpack . text
        guard . not $ S.null v
        return $ Recent pkg (S.tail v) pub
    getElem _ = Nothing

newtype Version = Version { unVersion :: S.ByteString }
instance JSON.FromJSON Version where
    parseJSON (JSON.Object o) = o JSON..: "fields" >>= \case
        (JSON.Object o') -> o' JSON..: "version" >>= \case
            v:_ -> return $ Version (T.encodeUtf8 v)
            _   -> mzero
        _ -> mzero
    parseJSON _ = mzero

checkStatusP :: (Int -> Bool) -> Status -> RequestHeaders -> CookieJar -> Maybe SomeException
checkStatusP p s@(Status sci _) hs j =
    if p sci
    then Nothing
    else Just $ toException $ StatusCodeException s hs j

getStoredVersion :: MonadIO m => Request -> S.ByteString -> Manager -> m (Maybe S.ByteString)
getStoredVersion req pkg mgr = do
    res <- httpLbs req
        { HTTP.path   = "/find_hackage/packages/" `S.append` pkg 
        , queryString = "?fields=version"
        , checkStatus = checkStatusP (\c -> (200 <= c && c < 300) || c == 404)
        } mgr 
    if responseStatus res == status404
        then return Nothing
        else return . fmap unVersion $ JSON.decode (responseBody res)

isNewer :: MonadIO m => Request -> Recent -> Manager -> m Bool
isNewer req (Recent pkg v _) mgr = maybe True (v /=) `liftM` getStoredVersion req pkg mgr

fetchCabalFile :: MonadIO m => Request -> Manager -> Recent -> m (Maybe D.PackageDescription)
fetchCabalFile req mgr (Recent pkg ver _) = do
    res <- httpLbs req { HTTP.path = S.concat ["/package/", pkg, "-", ver, "/", pkg, ".cabal"]  } mgr
    return $ case D.parsePackageDescription (TL.unpack . TL.decodeUtf8 $ responseBody res) of
        D.ParseOk _ pd -> Just (D.flattenPackageDescription pd)
        _              -> Nothing

updater :: (Functor m, MonadThrow m, MonadIO m) => Maybe HTTPDate -> Request -> Manager -> m [(D.PackageDescription, UTCTime)]
updater snc baseReq mgr = do
    hackage <- parseUrl "http://hackage.haskell.org/packages/recent.rss"
    rss     <- flip httpLbs mgr $ case snc of
        Nothing -> hackage
        Just t  -> hackage { requestHeaders = ("If-Modified-Since", formatHTTPDate t) : requestHeaders hackage 
                           , checkStatus    = checkStatusP (\c -> (200 <= c && c < 300) || c == 304)
                           }
    if responseStatus rss == status304
        then return []
        else case parse def $ (L.toStrict $ responseBody rss) of
            Right doc -> do
                upds <- filterM (\r -> isNewer baseReq r mgr) $ parseRecents doc
                pds  <- catMaybes `liftM` mapM (\r -> fmap (,recentTime r) <$> fetchCabalFile hackage mgr r) upds
                CL.sourceList pds $$ StorePackage.sinkStoreElasticsearch 100 H.empty baseReq mgr
                return pds
            Left _    -> return []
