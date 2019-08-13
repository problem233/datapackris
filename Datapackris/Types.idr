module Datapackris.Types

import public Data.DList
import public Control.Monad.RW

%default total

public export
record DatapackMeta where
  constructor MkDatapackMeta
  getDpkName : String
  getNamespace : String
  getDescription : String

public export
data ContentType = FunctionType

export
Eq ContentType where
  FunctionType == FunctionType = True
  _ == _ = False

export
Show ContentType where
  show FunctionType = "function"

public export
record Reference (type : ContentType) where
  constructor MkRef
  getRefName : String

export
Eq (Reference type) where
  (MkRef x) == (MkRef y) = x == y

public export
record Commands where
  constructor MkCommands
  getReferences : List (type ** Reference type)
  getCommands : DList String

export
Eq (DPair ContentType Reference) where
  (tyX ** MkRef nameX) == (tyY ** MkRef nameY) =
    tyX == tyY && nameX == nameY

export
Semigroup Commands where
  (MkCommands refXs xs) <+> (MkCommands refYs ys) =
    MkCommands (union refXs refYs) (xs ++ ys)

export
Monoid Commands where
  neutral = MkCommands [] []

public export
CommandsRW : Type -> Type
CommandsRW = RW DatapackMeta Commands

public export
ContentOf : ContentType -> Type
ContentOf FunctionType = Commands

public export
record Content (type : ContentType) where
  constructor MkContent
  getConName : String
  getContent : ContentOf type

export
Eq (DPair ContentType Content) where
  (tyX ** MkContent nameX _) == (tyY ** MkContent nameY _) =
    tyX == tyY && nameX == nameY

public export
record Datapack where
  constructor MkDatapack
  getContents : List (type ** Content type)

export
Semigroup Datapack where
  (MkDatapack xs) <+> (MkDatapack ys) = MkDatapack $ xs ++ ys

export
Monoid Datapack where
  neutral = MkDatapack []

public export
DatapackRW : Type -> Type
DatapackRW = RW DatapackMeta Datapack
