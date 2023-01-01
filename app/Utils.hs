module Utils where

import App (add, checkArgs, remove, toggle)
import Control.Monad (unless)
import Data.Aeson (eitherDecode, encode)
import Data.Aeson.Types (toJSON)
import Data.ByteString.Lazy as B (ByteString, readFile, writeFile)
import Data.Char (toLower)
import Data.Text as T (Text, append, pack)
import qualified Data.Text.IO as TIO
import System.Console.ANSI (clearScreen)
import System.Directory (doesFileExist)
import Text.ANSI (strikethrough)
import Text.Read (readEither)
import Types (ToDo (isComplete, text))

jsonFile :: FilePath
jsonFile = "./assets/list.json"

getJSON :: IO ByteString
getJSON = B.readFile jsonFile

-- create json file if one does not exist, decode json
getToDos :: IO (Either String [ToDo])
getToDos = do
  exists <- doesFileExist jsonFile
  unless exists (Prelude.writeFile jsonFile "[]") -- create empty list
  eitherDecode <$> getJSON

loop :: [ToDo] -> IO ()
loop todos = do
  prompt todos
  printMenu todos
  line <- getLine
  clearScreen
  putStrLn $ "You entered: " ++ line
  putStr "System response: "
  reply line todos
  let ts = updateList line todos
      encodedTodos = jsonEncode ts
  B.writeFile jsonFile encodedTodos -- write list to json file
  let userInput = map toLower <$> maybeHead (words line)
  unless (userInput == Just "quit") (loop ts)

prompt :: [ToDo] -> IO ()
prompt [] = putStrLn "\nThere are no to do items in your list."
prompt ts = do
  putStrLn "\nCurrent to do list:"
  printToDo (zip [1 ..] ts)

printToDo :: [(Int, ToDo)] -> IO ()
printToDo [] = return ()
printToDo ((i, t) : fs) =
  TIO.putStrLn (T.append (T.pack $ show i ++ ". ") (getLabel (isComplete t) (text t))) >> printToDo fs

getLabel :: Bool -> Text -> Text
getLabel b t
  | b = strikethrough t
  | otherwise = t

printMenu :: [ToDo] -> IO ()
printMenu ts = do
  putStrLn "\nCommands:"
  putStrLn "add <String>  - Add to do item"
  unless (null ts) (putStrLn "remove <Int>  - Remove numbered to do item")
  unless (null ts) (putStrLn "toggle <Int>  - Toggle complete status for numbered to do item")
  putStrLn "quit          - Quit\n"

reply :: String -> [ToDo] -> IO ()
reply s ts = do
  if null ts
    then replyLimitedOptions (map toLower <$> maybeHead (words s)) s
    else replyAllOptions (map toLower <$> maybeHead (words s)) s ts

replyLimitedOptions :: Maybe String -> String -> IO ()
replyLimitedOptions (Just "add") s =
  case maybeTail $ words s of
    Just [] -> putStrLn "Provide a string to add"
    Just a -> putStrLn $ "Adding " ++ unwords a
replyLimitedOptions (Just "quit") _ = putStrLn "Buh bye"
replyLimitedOptions (Just s) _ = putStrLn $ "No command `" ++ s ++ "`"
replyLimitedOptions Nothing _ = putStrLn "Enter a command"

replyAllOptions :: Maybe String -> String -> [ToDo] -> IO ()
replyAllOptions (Just "remove") s ts =
  case maybeTail $ words s of
    Just [] -> putStrLn "Provide an integer to remove"
    Just a -> checkArgs (readEither $ unwords a) ts "remove" "Removing"
replyAllOptions (Just "toggle") s ts =
  case maybeTail $ words s of
    Just [] -> putStrLn "Provide an integer to toggle"
    Just a -> checkArgs (readEither $ unwords a) ts "toggle" "Toggling"
replyAllOptions a s _ = replyLimitedOptions a s

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (_ : xs) = Just xs

updateList :: String -> [ToDo] -> [ToDo]
updateList s ts = do
  if null ts
    then updateListLimitedOptions (map toLower <$> maybeHead (words s)) s ts
    else updateListAllOptions (map toLower <$> maybeHead (words s)) s ts

updateListLimitedOptions :: Maybe String -> String -> [ToDo] -> [ToDo]
updateListLimitedOptions (Just "add") s ts = do
  case maybeTail $ words s of
    Just [] -> ts
    Just a -> add (unwords a) ts
updateListLimitedOptions (Just _) _ ts = ts
updateListLimitedOptions Nothing _ ts = ts

updateListAllOptions :: Maybe String -> String -> [ToDo] -> [ToDo]
updateListAllOptions (Just "remove") s ts = updateListItem (maybeTail $ words s) remove ts
updateListAllOptions (Just "toggle") s ts = updateListItem (maybeTail $ words s) toggle ts
updateListAllOptions a s ts = updateListLimitedOptions a s ts

updateListItem ::
  Maybe [String] -> -- tail
  (Either String Int -> [ToDo] -> [ToDo]) -> -- function, remove or toggle
  [ToDo] ->
  [ToDo]
updateListItem (Just []) _ ts = ts
updateListItem (Just a) f ts = f (readEither $ unwords a) ts

jsonEncode :: [ToDo] -> ByteString
jsonEncode ts = encode $ toJSON ts