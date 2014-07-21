{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import System.Environment
import Network
import Network.TigHTTP.Server

import Data.Pipe
import Data.Pipe.List

import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
	(pn :: Int) : _ <- mapM readIO =<< getArgs
	let port = PortNumber $ fromIntegral pn
	socket <- listenOn port
	forever $ do
		(cl, _, _) <- accept socket
		_ <- forkIO $ do
			ret <- getRequest cl
			putResponse cl . response $ LBS.fromChunks [
				"Good afternoon, world!\n",
				"Good night, world!\n" ]
			print =<< runPipe (requestBody ret =$= toList)
		return ()
