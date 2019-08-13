module Datapackris.Commands

import Datapackris.Types

%default total

export
cmds : List (type ** Reference type) -> List String -> CommandsRW ()
cmds refs xs = tell (MkCommands refs xs)

export
cmd : String -> CommandsRW ()
cmd = cmds [] . (:: [])

export
say : String -> CommandsRW ()
say = cmd . ("say " ++)

namespace FunctionRef
  export
  function : Reference FunctionType -> CommandsRW ()
  function ref@(MkRef name) = cmds [(FunctionType ** ref)] ["function " ++ name]

namespace FunctionName
  export
  function : String -> CommandsRW ()
  function name = cmds [(FunctionType ** MkRef name)] ["function " ++ name]
