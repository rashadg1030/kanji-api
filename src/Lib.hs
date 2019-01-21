{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib (app) where

import Web.Spock
import Web.Spock.Config
import Data.Aeson hiding (json)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import GHC.Generics
import Text.XML.HXT.Core hiding (app)
import Text.Pretty.Simple (pPrint) 
import Control.Monad.IO.Class

-- Kanji Data Type --
data Kanji = Kanji {
    literal :: Text, -- literal character
    grade :: Int, -- grade of the kanji
    strokes :: Int, -- strokes needed to write kanji
    jaOn :: [Text], -- "onyomi" of the kanji
    jaKun :: [Text], -- "kunyomi" of the kanji
    def :: [Text], -- definitions of the kanji
    nanori :: [Text] -- readings of the kanji used in names
} deriving (Generic, Show)

instance ToJSON Kanji

instance FromJSON Kanji

type Api = SpockM () () () ()

type ApiAction a = SpockAction () () () a

app :: Api
app = do
        get "kanji" $ do
           t <- liftIO runParser
           json . show $ t

