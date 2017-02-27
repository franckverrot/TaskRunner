import System.Directory

data Retry = AtMost Int
           | Forever
           deriving (Show)

data LastRun = Success
             | Error
             deriving (Show)

data Status = Maybe LastRun

data Task = Task { name        :: String
                 , retry       :: Retry
                 , targets     :: [Task]
                 , action      :: IO ()
                 , statusCheck :: IO (Maybe LastRun)
                 }

newTask :: Task
newTask = Task { name        = "unnamed task"
               , retry       = Forever
               , targets     = []
               , action      = return ()
               , statusCheck = return Nothing
               }

instance Show Task where
  show task =
    "Task<"
      ++ "name=" ++ (name task)
      ++ ", retry=" ++ (show $ retry task)
      ++ ", targets=" ++ (show $ length $ targets task)
      ++ ">"

printTree :: Task -> IO ()
printTree root = do
  printRecTree 0 root
  where
    printRecTree :: Int -> Task -> IO ()
    printRecTree depth root = do
      putStrLn $ (concat $ replicate depth "  ") ++ (show root)
      mapM_ (printRecTree (depth + 1)) $ targets root

taskName :: Task -> String -> Task
task `taskName` newName =
  task { name = newName }

retryCount :: Task -> Retry -> Task
task `retryCount` count =
  task { retry = count }

addTopLevelTask :: Task -> Task -> Task
task `addTopLevelTask` otherTask =
  task { targets = otherTask : (targets task) }

reportLastRun :: Task -> IO (Maybe LastRun) -> Task
task `reportLastRun` status =
  task { statusCheck = status }

onRun :: Task -> IO () -> Task
task `onRun` action =
  task { action = action }


checkFileExists :: String -> IO (Maybe LastRun)
checkFileExists filename = do
  result <- doesFileExist filename
  putStr $ "  Checking for file " ++ filename ++ "... "
  case result of
    False -> do
               putStrLn "Not found. Let's run the task?"
               return Nothing
    True  -> do
               putStrLn "Found. No need to rerun the task."
               return $ Just Success

config = newTask
           `taskName` "Root level"
           `retryCount` (AtMost 1)
           `addTopLevelTask` subtask1
           `onRun` putStrLn "Running..."

subtask1 = newTask
             `taskName` "subtask1"
             `reportLastRun` (checkFileExists "subtask1.output")
             `addTopLevelTask` subtask2
             `onRun` putStrLn " hello1"

subtask2 = newTask
             `taskName` "subtask2"
             `reportLastRun` (checkFileExists "subtask2.output")
             `onRun` putStrLn "  hello2"

run :: Task -> IO ()
run task = do
  status <- statusCheck task
  case status of
    Just Success -> return ()     -- don't need to run
    Just Error   -> (action task) -- would re-run
    Nothing      -> (action task) -- first run

  mapM_ run $ targets task

main = do
  putStrLn "Config:"
  printTree config

  putStrLn ""
  putStrLn "Run:"
  run config
