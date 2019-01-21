{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- For Database
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Lib (app) where

import Web.Spock
import Web.Spock.Config
import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Generics
import Control.Monad.IO.Class

-- For Database
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Database.Persist        hiding (get) -- To avoid a naming clash with Web.Spock.get
import qualified Database.Persist        as P         -- We'll be using P.get later for GET /people/<id>.
import           Database.Persist.Postgresql hiding (get)
import           Database.Persist.TH
-- Kanji Data Type --
data Kanji = Kanji {
    literal :: Text, -- literal character
    grade :: Int, -- grade of the kanji
    strokes :: Int, -- strokes needed to write kanji
    jaOn :: Text, -- "onyomi" of the kanji
    jaKun :: Text, -- "kunyomi" of the kanji
    def :: Text, -- definitions of the kanji
    nanori :: Text -- readings of the kanji used in names
} deriving (Generic, Show)

instance ToJSON Kanji

instance FromJSON Kanji

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

app :: Api
app = do
        get "kanji" $ do
           json . show $ "kanji"

