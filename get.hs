{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

import "monads-tf" Control.Monad.Trans
import Data.Pipe
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
		setHost (BSC.pack addr) 80
		p <- httpGet
		_ <- runPipe $ p =$= printP
		return ()

printP :: MonadIO m => Pipe BSC.ByteString () m ()
printP = do
	ms <- await
	case ms of
		Just s -> liftIO (BSC.putStrLn (BSC.take 100 s)) >> printP
--		Just s -> liftIO (BSC.putStrLn s) >> printP
		_ -> return ()
