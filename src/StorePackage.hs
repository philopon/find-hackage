{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE LambdaCase #-}

module StorePackage where

import qualified Codec.Archive.Tar                             as Tar

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource

import           Data.Aeson                                    ((.:), (.=))
import qualified Data.Aeson                                    as JSON
import qualified Data.ByteString.Char8                         as S
import qualified Data.ByteString.Lazy.Char8                    as L
import           Data.Conduit
import qualified Data.Conduit.List                             as CL
import qualified Data.Conduit.Zlib                             as ZLib
import           Data.Function                                 (fix, on)
import qualified Data.HashMap.Strict                           as H
import           Data.List
import           Data.Maybe
import qualified Data.Text                                     as T
import qualified Data.Text.Encoding                            as T
import qualified Data.Vector                                   as V

import qualified Distribution.Package                          as D
import qualified Distribution.PackageDescription               as D
import qualified Distribution.PackageDescription.Configuration as D
import qualified Distribution.PackageDescription.Parse         as D
import qualified Distribution.Text                             as D

import           Network.HTTP.Client.TLS
import           Network.HTTP.Conduit

import           System.Environment

import Numeric

untar :: MonadThrow m => Conduit S.ByteString m Tar.Entry
untar = do
    s <- CL.consume
    loop $ Tar.read (L.fromChunks s)
  where
    loop (Tar.Next e es) = yield e >> loop es
    loop  Tar.Done       = return ()
    loop (Tar.Fail e)    = throwM e

isCabalFile :: Tar.Entry -> Bool
isCabalFile e = ".cabal" `isSuffixOf` Tar.entryPath e

normalFileContent :: Tar.EntryContent -> Maybe L.ByteString
normalFileContent (Tar.NormalFile c _) = Just c
normalFileContent _ = Nothing

resultMaybe :: D.ParseResult a -> Maybe a
resultMaybe (D.ParseOk _ a) = Just a
resultMaybe _ = Nothing

entryToPackageDescription :: Monad m => Conduit Tar.Entry m D.PackageDescription
entryToPackageDescription =
    CL.filter isCabalFile =$
    CL.mapMaybe (normalFileContent . Tar.entryContent) =$
    CL.mapMaybe (resultMaybe . D.parsePackageDescription . L.unpack) =$
    CL.map D.flattenPackageDescription =$
    CL.groupBy ((==) `on` (D.pkgName . D.package)) =$
    CL.map (maximumBy (compare `on` (D.pkgVersion . D.package)))

pdToValue :: D.PackageDescription -> Maybe [T.Text] -> JSON.Value
pdToValue p dpr = JSON.object
    [ "name"          .= name
    , "version"       .= (D.display . D.pkgVersion . D.package) p
    , "license"       .= (D.display . D.license) p
    , "copyright"     .= D.copyright p
    , "maintainer"    .= D.maintainer p
    , "author"        .= D.author p
    , "stability"     .= D.stability p
    , "homepage"      .= D.homepage p
    , "bugReports"    .= D.bugReports p
    , "synopsis"      .= D.synopsis p
    , "description"   .= (unescapeHtml . D.description) p
    , "category"      .= (splitCategory . D.category) p
    , "hasLibrary"    .= (isJust . D.library) p
    , "hasExecutable" .= (not . null . D.executables) p
    , "executables"   .= (map D.exeName . D.executables) p
    , "deprecated"    .= isJust dpr
    , "inFavourOf"    .= maybe [] id dpr
    , "ngram"         .= JSON.object
        [ "description" JSON..= (filter (`notElem` " \t\n\r") . unescapeHtml . D.description) p
        , "synopsis"    JSON..= (filter (`notElem` " \t\n\r") . unescapeHtml . D.synopsis) p
        , "name"        JSON..= name
        ]
    , "raw"           .= JSON.object
        [ "name"     .= name
        , "license"  .= (D.display . D.license) p
        , "category" .= (splitCategory . D.category) p
        ]
    ]
  where
    splitCategory = map T.strip . T.splitOn "," . T.pack
    name = (D.display . D.pkgName    . D.package) p

unescapeHtml :: String -> String
unescapeHtml = loop
  where
    loop []       = []
    loop ('&':cs) = unescape cs
    loop (c:cs)   = c : loop cs

    entities = H.fromList [("lt", '<'), ("gt", '>'), ("amp", '&'), ("quot", '"')]

    unescape cs = case break (== ';') cs of
        (_, [])      -> []
        ('#':x:hex, _:cs') | x `elem` ['x', 'X'] -> case readHex hex of
            [(n, [])] -> toEnum n : loop cs'
            _         -> loop cs'
        ('#':dec, _:cs') -> case reads dec of
            [(n, [])] -> toEnum n : loop cs'
            _         -> loop cs'
        (ent, _:cs') -> case H.lookup ent entities of
            Just c  -> c : loop cs'
            Nothing -> loop cs'

parseUrl' :: MonadThrow m => String -> m Request
parseUrl' s | '@' `notElem` s = parseUrl s
parseUrl' s0 = do
    let (proto, s1) = T.breakOnEnd "://" (T.pack s0)
        (user,  s2) = T.breakOnEnd ":" s1
        (pass,  s3) = T.breakOnEnd "@" s2
    req <- parseUrl . concat $ map T.unpack [proto, s3]
    return $ applyBasicAuth (T.encodeUtf8 $ T.init user) (T.encodeUtf8 $ T.init pass) req

sinkStoreElasticsearch :: (MonadThrow m, MonadIO m)
                       => Int -> H.HashMap T.Text [T.Text] -> Request -> Manager
                       -> Consumer D.PackageDescription m ()
sinkStoreElasticsearch cs dpr req' mgr = do
    let req = req' { method = "POST" }
    fix $ \loop -> do
        chunk <- CL.take cs
        let cmd i = JSON.object ["index" .= JSON.object ["_id" .= (i :: String)]]
            body = L.unlines . map JSON.encode $ concatMap (\p ->
                let pkg = D.display . D.pkgName $ D.package p in
                [ cmd pkg
                , pdToValue p (H.lookup (T.pack pkg) dpr)
                ]) chunk
        unless (null chunk) $ do
            _ <- liftIO $ httpLbs req { requestBody = RequestBodyLBS body } mgr
            loop

newtype Deprecateds = Deprecateds { unDeprecateds :: H.HashMap T.Text [T.Text] }
instance JSON.FromJSON Deprecateds where
    parseJSON (JSON.Array a) = Deprecateds . H.fromList <$> mapM parseElem (mapMaybe fromObject (V.toList a))
      where
        fromObject (JSON.Object o) = Just o
        fromObject _ = Nothing
        parseElem o = (,) <$> o .: "deprecated-package" <*> o .: "in-favour-of"
    parseJSON _ = mzero

getDeprecateds :: (MonadThrow m, MonadIO m) => Manager -> m (H.HashMap T.Text [T.Text])
getDeprecateds mgr = do
    req <- parseUrl "http://hackage.haskell.org/packages/deprecated"
    res <- httpLbs req { requestHeaders = ("Accept", "application/json") : requestHeaders req } mgr
    maybe (fail "getDeprecateds: decode json failed.") return $
        unDeprecateds <$> (JSON.decode $ responseBody res)

storeElasticsearch :: (MonadThrow m, MonadIO m, MonadResource m) => String -> Manager -> m ()
storeElasticsearch url mgr = do
    dpr   <- getDeprecateds mgr
    index <- parseUrl "http://hackage.haskell.org/packages/index.tar.gz"
    src   <- http index mgr
    req   <- parseUrl' url
    responseBody src $$+-
        ZLib.ungzip =$ untar =$ entryToPackageDescription =$
        sinkStoreElasticsearch 500 dpr req { path = "/find_hackage/package/_bulk" } mgr

main :: IO ()
main = withManagerSettings tlsManagerSettings $ \mgr -> liftIO getArgs >>= \case
    [url] -> storeElasticsearch url mgr
    _     -> liftIO $ putStrLn "USAGE: main ELASTICSEARCH_URL"
