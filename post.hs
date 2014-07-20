{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

import "monads-tf" Control.Monad.Trans
import Data.Pipe
import System.Environment
import Network

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

import Network.TigHTTP.Client

main :: IO ()
main = do
	addr : spn : _ <- getArgs
	(pn :: Int) <- readIO spn
	sv <- connectTo addr (PortNumber $ fromIntegral pn)
	p <- run sv $ do
		setHost (BSC.pack addr) pn
		httpPost $ LBS.fromChunks
			["I am client.\n", "You are server.\n"]
	_ <- runPipe $ responseBody p =$= printP
	return ()

printP :: MonadIO m => Pipe BSC.ByteString () m ()
printP = await >>= maybe (return ()) (\s -> liftIO (BSC.putStr s) >> printP)
