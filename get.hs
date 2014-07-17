{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

import System.Environment
import Network

import Network.TigHTTP.Client

main :: IO ()
main = do
	addr : spn : _ <- getArgs
	(pn :: Int) <- readIO spn
	sv <- connectTo addr (PortNumber $ fromIntegral pn)
	httpGet sv >>= print
