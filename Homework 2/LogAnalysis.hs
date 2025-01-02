{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
parseMessage :: String->LogMessage
parseMessage str = case words str of
    ("I":timestamp:msg) ->
        LogMessage Info (read timestamp) (unwords msg)
    ("W":timestamp:msg) ->
        LogMessage Warning (read timestamp) (unwords msg)
    ("E":severity:timestamp:msg)->
        LogMessage (Error (read severity)) (read timestamp) (unwords msg)
    _->Unknown str

--parse all messages
parse :: String -> [LogMessage]
parse n = map parseMessage (lines n)

--filters to get errors with severity of at least 50 from messsages parsed with parse function
filter_errors_with_50_severity :: [LogMessage]->[LogMessage]
filter_errors_with_50_severity [] = []
filter_errors_with_50_severity (msg@(LogMessage (Error x) _ _):xs)
    | x>=50 = msg:filter_errors_with_50_severity xs
    | otherwise = filter_errors_with_50_severity xs
filter_errors_with_50_severity (_:xs) = filter_errors_with_50_severity xs

--form a messageTree out of messages
insert::LogMessage->MessageTree->MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf
insert message@(LogMessage _ timestamp _) (Node left node_message@(LogMessage _ node_timestamp _) right)
    | timestamp<node_timestamp = Node (insert message left) node_message right
    | otherwise = Node left node_message (insert message right)
insert _ (Node _ (Unknown _) _) = Leaf


--build a message tree from all the logMessages
build :: [LogMessage]->MessageTree
--base case
build [] = Leaf
-- recursive case
build (x:list) = insert x (build list)
-- we have LogMessage as Unknown, in this case we simply call the insert function, which will handle this
-- we have LogMessage as LogMessage, in this case too we can simply call the insert function as it handles this 
-- but we have to also look for the furture, which is assigning rest of the logmessages, so when we have the list, we can recursively call build, so we want to call insert <whatever> build(list), in the scenario that there is a singular elment, we insert that, in the case that there is nothing in the list, idk

--Exercise 4
-- Returns a list of LogMessages sorted by timestamp
inOrder :: MessageTree ->[LogMessage]
inOrder Leaf = [] 
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r
-- Aim is to traverse as left, curr, right, we can have inorder left_part ++ [LogMessage] ++ inorder rightpart
-- for base case, we can Leaf, in which case we return []
-- if we have Unknown at the middle, we can return [] which is a empty LogMessage, no this is filtered by insertion


--Get String part from LogMessage
getMessage :: LogMessage->String
getMessage (LogMessage _ _ s) = s
getMessage (Unknown s) = s

--Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lm = map getMessage (inOrder $ build $ filter_errors_with_50_severity lm)

--here even if we go by LogMessage, we need the MessageType to be Error,so if it is error then we take it else we keep going left and right
-- the base case is that if the array is empty then we reuturn empty array
-- we need to put a additonal filter to take only errors
-- pipeline will be as parse [LogMessage]-> build [MessageTree]->inOrder [LogMessage]->whatWentWrong (put out a additonal filter to take only the ones with error and 50) [String]
-- we can implement a foler before build and after parse, where we only take the errors with >=50 and continue the rest of the thing, at the end, all we have to do is take the op from inorder and restructure