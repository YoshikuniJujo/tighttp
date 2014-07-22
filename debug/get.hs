{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.Trans
-- import "monads-tf" Control.Monad.State
import Data.Pipe
import Data.HandleLike
import System.Environment
import Network

import qualified Data.ByteString.Char8 as BSC

import Network.TigHTTP.Client
import Network.TigHTTP.Types

main :: IO ()
main = do
	addr : spn : pth : _ <- getArgs
	(pn :: Int) <- readIO spn
	sv <- flip DebugHandle (Just "low") <$>
		connectTo addr (PortNumber $ fromIntegral pn)
	p <- request sv $ get addr pn pth
	_ <- runPipe $ responseBody p =$= printP
	return ()

printP :: MonadIO m => Pipe BSC.ByteString () m ()
printP = await >>=
	maybe (return ()) (\s -> liftIO (BSC.putStrLn (BSC.take 100 s)) >> printP)
