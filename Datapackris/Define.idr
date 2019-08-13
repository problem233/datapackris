module Datapackris.Define

import Datapackris.Types

%default total

export
defun : String -> CommandsRW () -> DatapackRW (Reference FunctionType)
defun name cmds = MkRW $ \meta =>
  let fullName = getNamespace meta ++ ":" ++ name
   in (MkRef fullName,
       MkDatapack [
         (FunctionType ** (MkContent fullName $ snd $ runRW cmds meta))])
