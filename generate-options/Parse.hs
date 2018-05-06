{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Parse where

import Control.Applicative ((<|>))
import Control.Error
import Text.XML.Light.Proc (onlyElems)
import Text.XML.Light.Types


data FdbOptionType = FdbOptionType {
  optionType :: String
  , options :: [FdbOption]
} deriving Show

data FdbOption = FdbOption {
  optionName :: String
  , optionCode :: Int
  , optionParam :: Maybe FdbParam
  , optionDescription :: String
} deriving Show

data FdbParam = FdbParam {
  paramDescription :: String
  , paramType :: FdbParamType
} deriving Show

data FdbParamType = FdbBytes | FdbString | FdbFlag | FdbInt
  deriving Show

type ParseError = String

get :: [Attr] -> String -> Either ParseError String
get attrs k =
  note ("Attr not found: " ++ k) $
    lookup k $ map (\Attr{..} -> (qName attrKey, attrVal)) attrs

parseParamType :: String -> Either ParseError FdbParamType
parseParamType "Bytes" = Right FdbBytes
parseParamType "String" = Right FdbString
parseParamType "Int" = Right FdbInt
parseParamType x = Left $ "unknown param type: " ++ x

getFdbParam :: [Attr] -> Either ParseError (Maybe FdbParam)
getFdbParam attrs = do
  let mParamTypeStr = hush (get attrs "paramType")
  let mParamDescription = hush (get attrs "paramDescription")
  case (mParamTypeStr, mParamDescription) of
    (Just paramTypeStr, Just paramDescription) -> do
      paramType <- parseParamType paramTypeStr
      return $ Just FdbParam{..}
    (Nothing, Just paramDescription) ->
      return $ Just $ FdbParam paramDescription FdbFlag
    (Just x, Nothing) ->
      Left $ "param type without description: " ++ x
    (Nothing, Nothing) ->
      return Nothing

toFdbOption :: Element -> Either ParseError FdbOption
toFdbOption Element{..} = do
  let paramName = qName elName
  assertErr ("Element not an Option: " ++ paramName)
            (paramName == "Option")
  optionName <- get elAttribs "name"
  optionCode <- get elAttribs "code" >>=
                readErr ("Couldn't parse code in " ++ paramName)
  optionParam <- getFdbParam elAttribs
  optionDescription <- get elAttribs "description" <|> pure ""
  return FdbOption{..}

toFdbOptionType :: Element -> Either ParseError FdbOptionType
toFdbOptionType elm = do
  assertErr ("top-level element not an option type")
            (qName (elName elm) == "Scope")
  optionType <- attrVal <$>
                headErr "unnamed top element" (elAttribs elm)
  options <- mapM toFdbOption (onlyElems $ elContent elm)
  return FdbOptionType{..}
