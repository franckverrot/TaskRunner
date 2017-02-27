# TaskRunner

Quick experiment in building a task manager.

Only a rough API so far.

```
λ stack exec runhaskell TaskRunner.hs
Config:
Task<name=Root level, retry=AtMost 1, targets=1>
  Task<name=subtask1, retry=Forever, targets=1>
    Task<name=subtask2, retry=Forever, targets=0>

Run:
Running...
  hello2
```
