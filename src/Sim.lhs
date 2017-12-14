> module Sim where
> import RedCode
> import Data.Array.IO

Calculate the address, given a mode and a value relative to the current address!

> calcAddr memory cnt (mode, addr) = do
>   case mode of
>     DRCT -> cnt + addr
>     INDR -> cnt + (indrAddr (readArray memory (cnt + addr)))
>     ADEC -> cnt + (autoDec memory (cnt + addr))

Grab the indirect address from a RedCode instruction!

> indrAddr (RedCode _ _ (Field (mode, addr))) = addr

Decrement the address, save it and then return the new one

> autoDec memory cnt = do
>   (RedCode op a (Field (mode, addr))) <- readArray memory cnt
>   writeArray memory cnt (RedCode op a (Field (mode, addr - 1)))
>   return (addr - 1)

Get the instruction at an address

> getInst memory cnt = do
>   readArray memory cnt

Update an address with a new instuction

> updateInst memory cnt inst = do
>   writeArray memory cnt inst

Get the A field value, regardless of addressing mode

> getAValue memory cnt = do
>   (RedCode op (Field (mode, addr)) b) <- readArray memory cnt
>   return addr

Get the B field value, regardless of addressing mode

> getBValue memory cnt = do
>   (RedCode op a (Field (mode, addr))) <- readArray memory cnt
>   return addr


INSTRUCTIONS

The move instruction, copying an instruction addressed by field A into the address from field B.
If a is Immediate, add a DAT instruction instead!

> mov memory addr (aMode, aAddr) (bMode, bAddr) = do
>   case aMode of
>     IMED -> updateInst memory (calcAddr (bMode, bAddr)) (RedCode DAT (Field (DRCT, 0)) (Field (DRCT,aAddr)))
>     _    -> updateInst memory (calcAddr (bMode, bAddr)) (getInst memory (calcAddr (aMode, aAddr)))

The add instruction, adding two values and saving it in the address referenced by field B.

> add memory addr (aMode, aAddr) (bMode, bAddr) = do
>   a <- case aMode of
>     IMED -> aAddr
>     _    -> getBValue memory (calcAddr memory addr (aMode, aAddr))
>   b <- getBValue memory (calcAddr memory addr (bMode, bAddr))
>   (RedCode op a1 _) <- getInst memory (calcAddr memory addr (bMode, bAddr))
>   updateInst memory (calcAddr memory addr (bMode, bAddr)) (RedCode op a1 (Field (DRCT,a+b)))

I didn't have enough time to complete the rest... :(

For the simulation part, I was going to implement a system similar to the PoorMansConcurrency2.lhs file,
where there was a list of threads, and each thread had a list of tasks (program counters) which would be
executed in a round robin order!
