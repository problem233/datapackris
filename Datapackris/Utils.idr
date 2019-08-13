module Datapackris.Utils

%default total

export
countBy : (a -> a -> Bool) -> List a -> List (a, Nat)
countBy (==) = foldl editCount []
  where editCount : List (a, Nat) -> a -> List (a, Nat)
        editCount ((y, count) :: r) x =
          if x == y then (y, S count) :: r else (y, count) :: editCount r x
        editCount [] x = [(x, 1)]

export
unsnoc : List a -> Maybe (List a, a)
unsnoc [] = Nothing
unsnoc (x :: []) = Just ([], x)
unsnoc (x :: xs) = case unsnoc xs of
  (Just (xs', last)) => Just (x :: xs', last)
  Nothing => Nothing

export
splitLast : (Char -> Bool) -> String -> (String, String)
splitLast f s =
  case unsnoc $ split f s of
    (Just (xs, x)) => (concat $ intersperse "/" xs, x)
    Nothing => ("", s)

export
dirAccesible : String -> IO Bool
dirAccesible dir = do
  res <- dirOpen dir
  case res of
    (Left _) => pure False
    (Right dir) => do
      dirClose dir
      pure True

export
createDirs : String -> IO (Either FileError ())
createDirs s = go splited
  where splited : List String
        splited = split (== '/') s
        go (x :: xs) = do
          acc <- dirAccesible x
          if acc then do
            changeDir x
            go xs
          else do
            res <- createDir x
            case res of
              err@(Left _) => pure err
              (Right _) => do
                changeDir x
                go xs
        go [] = do
          changeDir $ concat $ replicate (length splited) "../"
          pure $ Right ()

export
copyFile : String -> String -> IO (Either FileError ())
copyFile from to = do
  res <- readFile from
  case res of
    (Left err) => pure $ Left err
    (Right content) => writeFile to content
