module Tongues.Translate where

import Tongues.Prelude
import qualified Prelude as P
import qualified Web.Google.Translate as G
import qualified Data.Text.IO as TIO
import Data.Char (isDigit)
import qualified Data.Text as Text
import Network.HTTP.Client as C
import Network.HTTP.Client.TLS as TLS
import System.Envy (FromEnv(fromEnv))
import qualified System.Envy as Envy


cleanSrt :: Text -> Text
cleanSrt = Text.unlines . fmap (\t -> "<div>" <> t <> "</div>") . filter f . Text.lines
  where
    f t = case Text.uncons t of
      Nothing -> True
      Just (c, _) -> not $ isDigit c

cleanSrtFile :: FilePath -> FilePath -> IO ()
cleanSrtFile hin hout = TIO.readFile hin >>= TIO.writeFile hout . cleanSrt
  

translateFile :: FilePath -> G.Lang -> FilePath -> IO ()
translateFile hin targetLang hout = do
  t <- TIO.readFile hin
  r <- translate targetLang t
  case r of
    Left e -> putText e
    Right t' -> TIO.writeFile hout t'

translate :: G.Lang -> Text -> IO (Either Text Text)
translate targetLang t = do
  env <- getEnv
  r <- C.newManager TLS.tlsManagerSettings >>= \mgr ->
    G.translate mgr (googleApiKey env) Nothing tgt body
  case r of
    Right G.TranslationResponse { G.translations = xs } -> do
      return . Right . Text.concat . fmap getTxt $ xs
    Left e -> do
      return . Left $ show e
  where
    getTxt G.Translation { G.translatedText = G.TranslatedText txt } = txt
    tgt = G.Target targetLang
    body = G.Body t


data TranslateConfig = TranslateConfig
  { googleApiKey :: G.Key
  } deriving (Eq, Ord, Show, Generic)

instance FromEnv TranslateConfig where
  fromEnv _ = TranslateConfig . G.Key
    <$> Envy.env "TONGUES_GOOGLE_API_KEY"

getEnv :: IO TranslateConfig
getEnv = Envy.decodeEnv >>= either P.error return

main :: IO ()
main = do
  env <- getEnv
  r <- C.newManager TLS.tlsManagerSettings >>= \mgr ->
    G.translate mgr (googleApiKey env) (Just srcLang) trgLang (G.Body "Hello")
  case r of
    Right G.TranslationResponse { G.translations = xs } -> do
      forM_ xs $ \G.Translation { G.translatedText = G.TranslatedText txt } ->
        putText txt
    Left e -> do
      print e
  where
    srcLang = G.Source G.English
    trgLang = G.Target G.Russian


