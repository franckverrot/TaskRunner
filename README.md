# TaskRunner

Quick experiment in building a task manager.

Only a rough API so far.

```haskell
config = newTask
           `taskName` "Root level"
           `retryCount` (AtMost 1)
           `addTopLevelTask` subtask1
           `onRun` putStrLn "Running..."

subtask1 = newTask
             `taskName` "subtask1"
             `reportLastRun` (checkFileExists "TaskRunner.hs")
             `addTopLevelTask` subtask2
             `onRun` putStrLn " hello1"

subtask2 = newTask
             `taskName` "subtask2"
             `onRun` putStrLn "  hello2"

main = do
  ...
  run config
```

```
λ stack exec runhaskell TaskRunner.hs
Config:
Task<name=Root level, retry=AtMost 1, targets=1>
  Task<name=subtask1, retry=Forever, targets=1>
    Task<name=subtask2, retry=Forever, targets=0>


Run:
Running...
  Checking for file subtask1.output... Found. No need to rerun the task.
  Checking for file subtask2.output... Not found. Let's run the task?
  hello2
```
