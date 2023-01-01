module App where

import Data.Text as T (pack)
import Types (ToDo (ToDo, isComplete, text))

getTodoItems :: Either String [ToDo] -> [ToDo]
getTodoItems (Left _) = []
getTodoItems (Right []) = []
getTodoItems (Right ts) = ts

add :: String -> [ToDo] -> [ToDo]
add s ts = ts <> [ToDo {isComplete = False, text = T.pack s}]

remove :: Either String Int -> [ToDo] -> [ToDo]
remove (Left _) ts = ts
remove (Right _) [] = []
remove (Right i) (t : ts)
  | i - 1 == 0 = remove (Right $ i - 1) ts
  | otherwise = t : remove (Right $ i - 1) ts

toggle :: Either String Int -> [ToDo] -> [ToDo]
toggle (Left _) ts = ts
toggle (Right _) [] = []
toggle (Right i) (t : ts)
  | i - 1 == 0 = setIsComplete (not $ isComplete t) t : toggle (Right (i - 1)) ts
  | otherwise = t : toggle (Right (i - 1)) ts

checkArgs ::
  Either String Int ->
  [ToDo] ->
  String -> -- action,  remove or toggle
  String -> -- verb,    removing or toggling
  IO ()
checkArgs (Left _) ts a _ = putStrLn $ "Provide an integer to " ++ a
checkArgs (Right i) ts _ v
  | i < 1 || i > length ts =
      putStrLn $ "Provide an integer greater than 0 and less than " ++ show (length ts + 1)
  | otherwise = putStrLn $ v ++ " item #" ++ show i

setIsComplete :: Bool -> ToDo -> ToDo
setIsComplete b toDo = toDo {isComplete = b}