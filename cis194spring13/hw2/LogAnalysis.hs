module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = case words s of
    ("E":e:t:rest)      -> LogMessage (Error (read e)) (read t) (unwords rest)
    ("W":t:rest)        -> LogMessage Warning (read t) (unwords rest)
    ("I":t:rest)        -> LogMessage Info (read t) (unwords rest)
    other               -> Unknown (unwords other)

parse :: String -> [LogMessage]
parse s = case lines s of
    []          -> []
    ls          -> map parseMessage ls

compareLogMessage :: LogMessage -> LogMessage -> LogMessage
compareLogMessage left@(LogMessage _ t1 _) right@(LogMessage _ t2 _) =
    if t1 < t2 then left else right
compareLogMessage _ _ = error "bad compare\n"

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ t1 _) (Node left cur@(LogMessage _ t2 _) right) =
    if t1 <= t2
    then Node (insert lm left) cur right
    else Node left cur (insert lm right)
insert _ _ = error "bad insert\n"

build :: [LogMessage] -> MessageTree
build [] = Leaf
build lms = foldl (flip insert) Leaf lms

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left cur right) = inOrder left ++ [cur] ++ inOrder right

listShow :: [LogMessage] -> String
listShow [] = ""
listShow (e:es) = show e ++ "\n" ++ listShow es

isWarning :: LogMessage -> Bool
isWarning (LogMessage Warning _ _) = True
isWarning _ = False

isError :: LogMessage -> Bool
isError lm = case lm of
    (LogMessage (Error _) _ _) -> True
    _                          -> False

hasPriority :: Int -> LogMessage -> Bool
hasPriority n lm = case lm of
    (LogMessage (Error p) _ _) -> p >= n
    _                          -> False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong lms = map show (inOrder (build (filter (\ lm -> isError lm && hasPriority 50 lm) lms)))

main :: IO ()
main = do
    lms <- testParse parse 10000 "error.log"
    putStrLn $ foldl (\ acc s -> acc ++ s ++ "\n") "" (whatWentWrong lms)
