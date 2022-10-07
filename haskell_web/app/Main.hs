{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.IORef
import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid
import Data.IORef (atomicModifyIORef')

data Note = Note {
        author :: Text,
        content :: Text
    }

newtype ServerState = ServerState {
        notes :: IORef [Note]
    }

type Server a = SpockM () () ServerState a

app :: Server ()
-- app = get root (text "Hello!")
-- app = get root (html "<h1>Hello!</h1>")
app = do
    get root $ do 
        listNote <- getState >>= (liftIO . readIORef . notes)
        lucid $ do
            h1_ "Hello!"
            p_ "How are you today?"
            ul_ $ forM_ listNote $ \note -> li_ $ do
                toHtml (author note)
                ": "
                toHtml (content note)
            h2_ "New Note"
            form_ [method_ "post"] $ do
                label_ $ do
                    "Author: "
                    input_ [name_ "author"]
                label_ $ do
                    "Content: "
                    textarea_ [name_ "content"] ""
                input_ [type_ "submit", value_ "Add Note"]
                
    post root $ do
        author <- param' "author"
        content <- param' "content"
        notesRef <- notes <$> getState
        liftIO $ atomicModifyIORef' notesRef $ \notes ->
            (notes <> [Note author content], ())
        redirect "/"

main :: IO ()
main = do
    st <- ServerState <$> newIORef [
            Note "Alice" "Must not forget to walk the dog", 
            Note "Bob" "Must. Eat. Pizza!!"
        ]

    cfg <- defaultSpockCfg () PCNoDatabase st
    runSpock 8080 (spock cfg app)
