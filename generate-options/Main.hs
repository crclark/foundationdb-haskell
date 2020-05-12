{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

import Generate
import Parse

import Data.Foldable (for_)
import Options.Generic (type (<?>), Generic, getRecord, ParseRecord(..), unHelpful)
import System.IO (writeFile)
import Text.XML.Light.Proc (onlyElems)
import Text.XML.Light.Types
import Text.XML.Light.Input (parseXMLDoc)

data ProgramOptions = ProgramOptions {
  file :: FilePath <?> "Path to fdb.options"
} deriving (Generic, Show)

instance ParseRecord ProgramOptions

main :: IO ()
main = do
  fdbPath <- unHelpful . file <$> getRecord "fdb.options to Haskell generator"
  raw <- readFile fdbPath
  case parseXMLDoc raw of
    Nothing -> error "Not a valid XML file."
    Just contents -> do
      let eOptTys = mapM toFdbOptionType $ onlyElems $ elContent contents
      case eOptTys of
        Left err -> error $ "parse error: " ++ err
        Right optTys -> for_ optTys $ \optTy -> do
          let fileName = (optionType optTy) ++ ".hs"
          writeFile fileName $ generateOptionsModule optTy
