{-# LANGUAGE RecordWildCards #-}
import System.Console.GetOpt
import System.Environment
import Data.List.Split
import Control.Applicative
import Control.Monad
import Control.Arrow
import System.Directory
import System.Locale
import Data.Time
import Data.List (transpose, intercalate)
import Utils
import Common

data Options = Options
    { numDays        :: Int
    , removeItems    :: [Int]
    , todoStr        :: String
    , suppressOutput :: Bool
    , listName       :: String
    } deriving (Show)

defaultOptions :: Options
defaultOptions = Options
    { numDays = 1
    , removeItems = []
    , todoStr = ""
    , suppressOutput = False
    , listName = "main"
    }

toOutputRow :: UTCTime -> TodoItem -> String
toOutputRow now item@(TodoItem {..}) = timeStr ++ " " ++ message
    where timeStr = color ++ formatTime' "%D %T"  dateAdded ++ endColor
          (color, endColor) = if isOld now item then ("\x1b[31m", "\x1b[0m") else ("", "")

options = 
    [ Option "n" ["days"]
        (ReqArg
            (\arg opt -> opt { numDays = read arg })
            "Int")
        "Number of days"
    , Option "r" ["remove"]
        (ReqArg
            (\arg opt -> opt { removeItems = map read . filter (not . null) $ splitOn "," arg})
            "Comma separated list of Ints")
        "Items to remove"
    , Option "s" ["suppress"]
        (NoArg
            (\opt -> opt {suppressOutput = True}))
        "Suppress output"
    , Option "l" ["name"]
        (ReqArg
            (\arg opt -> opt { listName = arg })
            "String")
        "The name of the todo-list to use"
    ]


writeTodoList :: String -> [TodoItem] -> IO ()
writeTodoList name ts = do
    p <- getTodoListPath name
    writeFile p (show ts)

main = do
    (actions, nonOptions, errors) <- getOpt RequireOrder options <$> getArgs

    let Options {..} = foldl (.) id actions $ defaultOptions

    now <- getZonedTime
    let utcNow = zonedTimeToUTC now
    let newMessage = unwords nonOptions

    let newItemL = 
         case newMessage of 
            "" -> []
            _  -> [TodoItem {dateAdded = now, message = newMessage}]

    todoList <- getTodoList listName
 
    let newTodoList = removeAtNs removeItems todoList ++ newItemL
    case suppressOutput of 
        True -> return ()
        False -> putStr $ columnate [map show [0..], map (toOutputRow utcNow) newTodoList]

    writeTodoList listName newTodoList

