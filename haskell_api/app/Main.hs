{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Generics
import GHC.IO.Encoding
import Control.Lens
import Control.Monad (forM_)
import Control.Concurrent.Async
import Network.Wreq
import Data.Text (Text)
import Data.Aeson
import qualified Data.Text.IO as T
import GHC.IO.Encoding (utf8)
import System.Win32.Console


data TranslateRequest = TranslateRequest {
        q :: Text,
        source :: Text,
        target :: Text,
        format :: Text
    } 
    deriving (Generic, Show)

data TranslateResponse = TranslateResponse {
        translatedText :: Text
    } 
    deriving (Generic, Show)

data Language = Language {
        code :: Text,
        name :: Text
    } 
    deriving (Generic, Show)

instance ToJSON TranslateRequest

instance FromJSON TranslateResponse

instance FromJSON Language

main :: IO ()
main = do
    setLocaleEncoding utf8
    setConsoleOutputCP 65001

    langs <- getLanguages
    results <- forConcurrently langs $ \lang -> do
    -- forM_ langs $ \lang -> do
        result <- translateText "Haskell is an interesting programming language" "en" (code lang)
        -- T.putStrLn ((name lang) <> " : " <> result)
        pure ((name lang) <> " : " <> result)
    mapM_ (T.putStrLn) (results)


-- Get languages from API
-- Method: GET
-- Endpoint: https://translate.argosopentech.com/languages
getLanguages :: IO [Language]
getLanguages = do
    rsp <- asJSON =<< get "https://translate.argosopentech.com/languages"
    pure (rsp ^. responseBody)


-- Get languages from API
-- Method: POST
-- Endpoint: https://translate.argosopentech.com/languages
translateText :: Text -> Text -> Text -> IO Text
translateText text sourceLang targetLang = do
    rsp <- asJSON =<< post "https://translate.argosopentech.com/translate" (toJSON (TranslateRequest {
        q = text,
        source = sourceLang,
        target = targetLang,
        format = "text"  
    }))
    pure (translatedText (rsp ^. responseBody))

