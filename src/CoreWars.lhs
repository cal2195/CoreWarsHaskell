Main module for the CoreWars sim

> module CoreWars where
>
> import RedCode
> import System.Environment


> loadFiles :: IO [Program]
> loadFiles = do
>   files <- getArgs
>   contents <- mapM readFile files
>   let line = map lines contents
>   let programs = map parseProgram line
>   return programs
