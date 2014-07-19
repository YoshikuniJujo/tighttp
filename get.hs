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
	_ <- run sv $ do
		setHost (BSC.pack addr) 80
		p <- httpGet
		runPipe $ p =$= printP
	return ()

printP :: MonadIO m => Pipe BSC.ByteString () m ()
printP = await >>=
	maybe (return ()) (\s -> liftIO (BSC.putStrLn (BSC.take 100 s)) >> printP)
