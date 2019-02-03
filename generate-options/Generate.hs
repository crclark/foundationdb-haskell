{-# LANGUAGE RecordWildCards #-}

module Generate (generateOptionsModule) where

import Parse

import Data.Char (toLower, toUpper)
import Language.Haskell.Exts.Build
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy p xs =
  let s = takeWhile (not . p) xs
      l = length s
      in s : splitBy p (drop (l+1) xs)

lowerCamelCase :: String -> String
lowerCamelCase [] = []
lowerCamelCase str =
  let (x:xs) = splitBy (== '_') str
      in map toLower x ++ concatMap capitalize xs

ty :: String -> Type ()
ty str = TyCon () (UnQual () (Ident () str))

con :: String -> [Type ()] -> QualConDecl ()
con conName params =
  QualConDecl () Nothing Nothing (ConDecl () (Ident () conName) params)

optName :: FdbOptionType -> String -> String
optName FdbOptionType{..} suffix = capitalize optionType ++ suffix

stringOptName :: FdbOptionType -> String
stringOptName o = optName o "String"

intOptName :: FdbOptionType -> String
intOptName o = optName o "Int"

bytesOptName :: FdbOptionType -> String
bytesOptName o = optName o "Bytes"

flagOptName :: FdbOptionType -> String
flagOptName o = optName o "Flag"

inst :: String -> InstRule ()
inst qn = IRule () Nothing Nothing (IHCon () (UnQual () (Ident () qn)))

optionDataDecl :: FdbOptionType -> Decl ()
optionDataDecl o@FdbOptionType{..} =
  DataDecl ()
           (DataType ())
           Nothing -- context
           (DHead () (Ident () (capitalize optionType)))
           [ con (stringOptName o) [ty "Int", ty "String"]
           , con (intOptName o) [ty "Int", ty "Int"]
           , con (bytesOptName o) [ty "Int", ty "ByteString"]
           , con (flagOptName o) [ty "Int"]]
           [Deriving () Nothing [inst "Show", inst "Read", inst "Eq", inst "Ord"]]

simpleBind :: String -> [Pat ()] -> Exp () -> Decl ()
simpleBind fnName pats rhs =
  FunBind () [Match () (Ident () fnName) pats (UnGuardedRhs () rhs) Nothing]

-- | I don't see any haskell-src-exts option for generating commented code,
-- so we will have to insert these comments "by hand". There's a
-- haskell-src-exts-sc package on hackage, but I don't understand how to use it.
mkComment :: String -> String
mkComment str = "-- | " ++ str ++ "\n"

optionFunctionDecl :: FdbOptionType -> FdbOption -> Decl ()
optionFunctionDecl optType FdbOption{..} =
  case optionParam of

    Nothing ->
      simpleBind (lowerCamelCase optionName)
                 []
                 (app (var (name (flagOptName optType)))
                      (paren $ intE $ fromIntegral optionCode))

    Just (FdbParam _ FdbString) ->
      simpleBind (lowerCamelCase optionName)
                 [PVar () (name "str")]
                 (foldl1 app [ var (name (stringOptName optType))
                             , paren $ intE $ fromIntegral optionCode
                             , var (name "str")])

    Just (FdbParam _ FdbInt) ->
      simpleBind (lowerCamelCase optionName)
                 [PVar () (name "i")]
                 (foldl1 app [ var (name (intOptName optType))
                             , paren $ intE $ fromIntegral optionCode
                             , var (name "i")])

    Just (FdbParam _ FdbBytes) ->
      simpleBind (lowerCamelCase optionName)
                 [PVar () (name "bs")]
                 (foldl1 app [ var (name (bytesOptName optType))
                             , paren $ intE $ fromIntegral optionCode
                             , var (name "bs")])

    Just (FdbParam _ FdbFlag) -> error "impossible case"
{-
modName :: ModuleName ()
modName = ModuleName () "FoundationDB.Options"

exports :: [FdbOptionType] -> ExportSpecList ()
exports opTys =
  ExportSpecList () (concatMap f opTys)

  where f FdbOptionType{..} =
          eabs (capitalize optionType)
          : map (eabs . lowerCamelCase . optionName) options
        eabs x = EAbs () (NoNamespace ()) (Qual () modName (name x))

moduleHead :: [FdbOptionType] -> ModuleHead ()
moduleHead opTys =
  ModuleHead ()
             modName
             Nothing
             (Just (exports opTys))

generateModuleHead :: [FdbOptionType] -> String
generateModuleHead = prettyPrint . moduleHead
-}

generateOption :: FdbOptionType -> FdbOption -> String
generateOption opTy opt@FdbOption{..} =
  mkComment optionDescription
  ++ prettyPrint (optionFunctionDecl opTy opt)
  ++ "\n\n"

generateOptionType :: FdbOptionType -> String
generateOptionType o@FdbOptionType{..} =
  prettyPrint (optionDataDecl o)
  ++ "\n\n"
  ++ concatMap (generateOption o) options

deprecatedPragmas :: [FdbOption] -> [Decl ()]
deprecatedPragmas opts =
  map (\FdbOption{..} ->
        DeprPragmaDecl () [( [name (lowerCamelCase optionName)]
                           , "Deprecated in FDB C API")]) $
  filter ((== "Deprecated") . optionDescription) opts

generateDeprecatedPragmas :: [FdbOptionType] -> String
generateDeprecatedPragmas =
  unlines . map prettyPrint . deprecatedPragmas . concatMap options

-- TODO: figure out a better way to do comments so we don't need to do any
-- manual string concatenation.
generateOptionsModule :: [FdbOptionType] -> String
generateOptionsModule tys =
  "{-# OPTIONS_GHC -fno-warn-missing-signatures #-}\n"
  ++ "-- | NOTE: This file is generated from <https://github.com/apple/foundationdb/blob/master/fdbclient/vexillographer/fdb.options fdb.options>\n"
  ++ "-- by the generate-options executable in this project.\n"
  ++ "-- All documentation on the individual options in this namespace comes\n"
  ++ "-- from FoundationDB's documentation in @fdb.options@.\n"
  ++ "module FoundationDB.Options where\n"
  ++ "import Data.ByteString.Char8 (ByteString)\n\n"
  ++ generateDeprecatedPragmas tys
  ++ unlines (map generateOptionType tys)
