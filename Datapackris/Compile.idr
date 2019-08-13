module Datapackris.Compile

import Data.DList
import Datapackris.Types
import Datapackris.Utils

%default total

public export
data DpkFile : Type where
  MkFile : (fileName : String) -> (content : String) -> DpkFile
  MkCopy : (fileName : String) -> (source : String) -> DpkFile

public export
Compiled : Type
Compiled = List DpkFile

Semigroup Bool where
  a <+> b = a || b
Monoid Bool where
  neutral = False

checkDup : Datapack -> List String
checkDup (MkDatapack xs) = snd $ runWriter $
  for {b = ()} (countBy (==) xs) $ \((type ** MkContent name _), count) =>
    if count > 1
    then tell [show type ++ " " ++ name ++
               " is defined " ++ show count ++ " times"]
    else pure ()

checkDup' : Datapack -> Maybe String
checkDup' (MkDatapack xs) =
  Prelude.Basics.snd $ runWriter {a = ()} $ do
    tell $ Just "hello"
    pure ()

applyFor : (tyC ** Content tyC) -> (tyR ** Reference tyR) -> Bool
applyFor (tyC ** MkContent nameC _) (tyR ** MkRef nameR) =
  tyC == tyR && nameC == nameR

checkRef : Datapack -> List String
checkRef (MkDatapack xs) = snd $ runWriter $
  for {b = List ()} xs $ \(type ** MkContent name cmds) =>
    case type of
      FunctionType => for (getReferences cmds) $ \ref =>
        if any (`applyFor` ref) xs then pure ()
        else tell [
          "couldn't find " ++ show (fst ref) ++ " " ++
          show (getRefName $ snd ref) ++ " in function " ++ name]
      _ => pure []

getTypeDir : ContentType -> String
getTypeDir FunctionType = "function"

getTypeExt : ContentType -> String
getTypeExt FunctionType = ".mcfunction"

getFileName : (type ** Content type) -> Either String String
getFileName (type ** MkContent name _) with (split (== ':') name)
  | [ns, path] = Right $
      "data/" ++
      ns ++ "/" ++
      getTypeDir type ++ "/" ++
      path ++ getTypeExt type
  | _ = Left $ "malformed " ++ show type ++ " name: " ++ name

compileFile : (type ** Content type) -> Either String DpkFile
compileFile x with (x)
  | (FunctionType ** MkContent _ (MkCommands _ cmds)) = case getFileName x of
      Left err => Left err
      Right name => Right $ MkFile name $ unlines $ fromDList cmds

compileFiles : Datapack -> Either (List String) Compiled
compileFiles (MkDatapack xs) = goRight xs
  where goLeft : List (type ** Content type) -> List String
        goLeft (x :: xs) with (compileFile x)
          | Left err = err :: goLeft xs
          | Right _ = goLeft xs
        goLeft [] = []
        goRight xs with (xs)
          | (x :: xs') with (compileFile x)
            | Left _ = Left $ goLeft xs
            | Right file = case goRight xs' of
                errs@(Left _) => errs
                (Right files) => Right $ file :: files
          | [] = Right []

export
compile : DatapackMeta -> DatapackRW _ -> Either (List String) Compiled
compile meta dprw with (snd $ runRW dprw meta)
  | dpk@(MkDatapack xs) = 
      let dupErrs = checkDup dpk
          refErrs = checkRef dpk
          files = compileFiles dpk
       in case files of
            (Left errs) => Left $ dupErrs ++ refErrs ++ errs
            (Right compiled) =>
              let errs = dupErrs ++ refErrs
               in if isCons errs then Left errs else Right compiled

createDirsDo : String -> IO (Either FileError a)
            -> IO (Either FileError a)
createDirsDo name f with (splitLast (== '/') name)
  | ("", _) = f
  | (path, _) = do
      res <- createDirs path
      case res of
        (Left err) => pure $ Left err
        (Right _) => f

exportFile : DpkFile -> IO (Either FileError ())
exportFile (MkFile name content) =
  createDirsDo name $ writeFile name content
exportFile (MkCopy name source) =
  createDirsDo name $ copyFile source name

getName : DpkFile -> String
getName (MkFile name _) = name
getName (MkCopy name _) = name

export
exportCompiled : Compiled -> IO ()
exportCompiled xs = do
  for {b = ()} xs $ \file => do
    res <- exportFile file
    case res of
      (Left err) => do
        putStrLn $ "when writing file " ++ getName file ++ ": " ++ show err
        idris_crash "runtime error"
      (Right _) => pure ()
  putStrLn "write completed"
  pure ()

createMCMeta : DatapackMeta -> DpkFile
createMCMeta meta = MkFile "pack.mcmeta" $ concat [
  "{",
    "\"pack\": {",
      "\"pack_format\":4,",
      "\"description\":\"", getDescription meta, "\"",
    "}",
  "}"
]

export
compileExport : DatapackMeta -> DatapackRW _ -> IO ()
compileExport meta dprw with (compile meta dprw)
  | Left errs = do
      for errs putStrLn
      pure ()
  | Right compiled = do
      createDir $ getDpkName meta
      changeDir $ getDpkName meta
      exportCompiled (createMCMeta meta :: compiled)
