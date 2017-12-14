Main module for the CoreWars sim

> module CoreWars(start) where
>
> import RedCode
> import System.Environment
> import Data.Array.IO

Start our sim! :D

> start = generateMemory loadFiles

Given a list of files as args, parse these files and create a list of programs
Programs are a list of RedCode instructions

> loadFiles :: IO [Program]
> loadFiles = do
>   files <- getArgs
>   contents <- mapM readFile files
>   let line = map lines contents
>   let programs = map parseProgram line
>   return programs

Next up, given a list of programs, populate memory with them, evenly spaced apart!

> generateMemory programs = do
>   progs <- programs
>   memory <- (newArray (0,8000) (RedCode DAT (Field (DRCT,0)) (Field (DRCT,0)))) :: IO (IOArray Int RedCode)
>   populateMemory memory progs 0 (length progs)
>   return memory

Take a list of programs, and insert them into memory, at 8000 / num programs intervals!

> populateMemory _ [] _ _ = return ()
> populateMemory memory (prog:progs) num total = do
>   insertProgram memory prog ((8000 `quot` total) * num)
>   populateMemory memory progs (num + 1) total

Insery a single program, line by line, into memory, taking into account the looped address space!

> insertProgram _ [] _ = return ()
> insertProgram memory (line:lines) addr = do
>   writeArray memory (addr `mod` 8000) line
>   insertProgram memory lines (addr + 1)
