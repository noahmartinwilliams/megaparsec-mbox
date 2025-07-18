module Main where

import Data.Either
import Data.Map as M
import Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.MBox.Arc

test01 :: IO ()
test01 = do
    let input = "ARC-Seal: i=1; a=rsa-sha256; t=1739058369; cv=none;\n        d=google.com; s=arc-20240605;\n        b=a+s479KU0OmVVn9PguIoMn8cSBpdPwSP+9ctDs43v19gDolkajjFBIsfoSYpI5q48H\n         lMfFKp1m1v4YVfWBT8ds19sjmzwrrXkVjdI1BvcIaAUPTb4ubghcCOoSqqtDggm13v0r\n         XdPN1qaa28o/UvQDRvFGJmNKxcqiiT5F8IoEyGO6+JQa+KjGEJx18kNgWL1RIoSz7E0S\n         Sp32P8rWNkhm0+KZN2nV4PVDGRVgbM6ucJld27NyXwB/MPsIcdz6gfw1DFUBFmk4Lq1e\n         FSOWtf66b5rqSvTDwFK83/yV86veTC01LpgS4Zqx+p25A+HEQuNCcoz3rR270yIu8xtV\n         Y6Rg=="
        result = runParser mboxArcSeal "" (T.pack input)
    if isRight result
    then do
        let (Right result') = result
        if (M.lookup (T.pack "i") result') == (Just (T.pack "1"))
        then
            putStrLn ("Test 01 succeeded.")
        else
            putStrLn ("Test 01 failed.")
    else
        let (Left err) = result in putStrLn (errorBundlePretty err)

main :: IO ()
main = do
    test01
