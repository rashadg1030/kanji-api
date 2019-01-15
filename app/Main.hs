module Main where

import Lib
import Web.Spock
import Web.Spock.Config


main :: IO ()
main = do
    spockCfg <- defaultSpockCfg () PCNoDatabase ()
    runSpock 8080 (spock spockCfg app)
