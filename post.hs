{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, TupleSections,
	PackageImports #-}

import "monads-tf" Control.Monad.State
import Data.Pipe
import System.Environment
import Network

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

import Network.TigHTTP.Client
import Network.TigHTTP.Types

main :: IO ()
main = do
	addr : spn : pth : msgs <- getArgs
	(pn :: Int) <- readIO spn
	sv <- connectTo addr (PortNumber $ fromIntegral pn)
	p <- request sv . post addr pn pth . (Nothing ,) .
		LBS.fromChunks $ map BSC.pack msgs -- ["I am client.\n", "You are server.\n"]
	_ <- runPipe $ responseBody p =$= printP
	return ()

printP :: MonadIO m => Pipe BSC.ByteString () m ()
printP = await >>= maybe (return ()) (\s -> liftIO (BSC.putStr s) >> printP)
