@echo off
REM arg1 is dir, arg2 is filename *without* extension
happy -i %1/%2.y
runhaskell -iHUnit %1/%2.hs

