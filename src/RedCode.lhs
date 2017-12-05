This module will hold all the redcode types and programs

> module RedCode(Program, parseProgram) where
> import Data.List.Split

A program is a list of redcode instructions

> type Program = [RedCode]

A redcode instuction has an opcode, and maybe one or two fields

> data RedCode = RedCode OpCode (Maybe Field) (Maybe Field)

The list of opcodes

> data OpCode = DAT | MOV | ADD | SUB | JMP | JMZ | JMN | DJN | CMP | SPL
>             deriving (Read, Show)

The list of addressing modes (Indirect | Immediate | AutoDecrement | Direct)

> data AddrMode = INDR | IMED | ADEC | DRCT

The field type

> data Field = Field (AddrMode, Int)

Take each line split by whitespace and parse it into redcode

> parseInstruction :: String -> Maybe String -> Maybe String -> RedCode
> parseInstruction opcode maybeA maybeB = RedCode (read opcode) (parseField maybeA) (parseField maybeB)

Parse each field if it exists

> parseField :: Maybe String -> Maybe Field
> parseField (Just field) = do
>   let addrMode = head field
>   let (mode, addr) = case addrMode of
>                        '@' -> (INDR, read $ drop 1 field)
>                        '#' -> (IMED, read $ drop 1 field)
>                        '<' -> (ADEC, read $ drop 1 field)
>                        _   -> (DRCT, read $ field)
>   return (Field $ (mode, addr))


Parse a list of strings to a redcode program

> parseProgram :: [String] -> Program
> parseProgram lines = do
>   let tokens = map (splitOn " ") lines
>   (parseTokens tokens)

Take the list of tokens and parse it into redcode.

> parseTokens :: [[String]] -> Program
> parseTokens ([op,a,b]:ops) = parseInstruction op (Just (init a)) (Just b) : parseTokens ops
> parseTokens ([op,a]:ops)   = case op of
>                                "JMP" -> parseInstruction op (Just a) Nothing : parseTokens ops
>                                "DAT" -> parseInstruction op Nothing (Just a) : parseTokens ops
> parseTokens _              = []
