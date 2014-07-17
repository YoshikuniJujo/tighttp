{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

import "monads-tf" Control.Monad.Trans
import System.Environment
import Network

import qualified Data.ByteString.Char8 as BSC

import Network.TigHTTP.Client

main :: IO ()
main = do
	addr : spn : _ <- getArgs
	(pn :: Int) <- readIO spn
	sv <- connectTo addr (PortNumber $ fromIntegral pn)
	run sv $ do
		setHost (BSC.pack addr) pn
		httpPost "I am client.\n" >>= liftIO . print
