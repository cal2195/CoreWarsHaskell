This file will handle the simulation part for each thread action

> module Sim where

Given a program counter, perform a single tick and return the new program counter

> tick :: IOArray -> Int -> Int
> tick memory cnt = do
>   (RedCode op a b) <- readArray memory cnt
>   case op of
>     DAT -> return cnt
>     MOV -> mov memory a b

Time to calculate that address!

> calcAddr memory cnt (mode, addr) = do
>   case mode of
>     DRCT -> cnt + addr
>     INDR -> cnt + (indrAddr (readArray memory (cnt + addr)))
>     IMED -> addr
>     ADRC -> cnt + (indrAddr (readArray memory (cnt + (autoDec memory addr))))

Decrement the B field value

> autoDec memory cnt = do
>   (RedCode op a (mode, addr)) <- readArray memory cnt
>   writeArray memory cnt (RedCode op a (mode, addr - 1))
>   return (addr - 1)

Get the address at an indirect address

> indrAddr (RedCode _ _ (mode, addr)) = addr

The MOV instruction

> mov memory addr (aMode, aAddr) (bMode, bAddr) = do
>   case aMode of
>     IMED -> writeArray memory (calcAddr (bMode, bAddr)) (RedCode DAT Nothing (Just aAddr))
>     _    -> copy memory (calcAddr (aMode, aAddr)) (calcAddr (bMode, bAddr))
>   return (addr + 1)

Copy a value from one address to another

> copy memory addr1 addr2 = do
>   val <- readArray memory addr1
>   writeArray memory addr2 val
