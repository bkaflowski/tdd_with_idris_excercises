module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) ->
              (items : Vect size String) ->
              DataStore

size : DataStore -> Nat
size (MkData size items) = size

items : (store : DataStore) -> Vect (size store) String
items (MkData size items) = items

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (item :: items2) = item :: addToData items2

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parseCommand : String -> String -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                         False => Nothing
                         True => Just (Get (cast val))
parseCommand "search" pat = Just (Search pat)
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                             case integerToFin pos (size store) of
                                  Nothing => Just ("Out of range\n", store)
                                  Just id => Just (index id store_items ++ "\n", store)

findAllMatches : String -> Integer -> Vect n String -> List (Integer, String)
findAllMatches pat idx [] = []
findAllMatches pat idx (x :: xs) = if isInfixOf pat x then (idx, x) :: findAllMatches pat (idx + 1) xs
                                                  else findAllMatches pat (idx + 1) xs

search : String -> DataStore -> List (Integer, String)
search pat (MkData size items) = findAllMatches pat 0 items

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp
   = case parse inp of
          Nothing => Just ("Invalid command\n", store)
          Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
          Just (Get pos) => getEntry pos store
          Just (Search pat) => Just ("Found: " ++ show (search pat store) ++ "\n", store)
          Just Size => Just ("Size of store: " ++ show (size store) ++ "\n", store)
          Just Quit => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
