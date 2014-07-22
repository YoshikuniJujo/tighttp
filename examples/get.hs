{-# LANGUAGE PackageImports #-}

import "monads-tf" Control.Monad.Trans
import Data.Pipe
import System.Environment
import Network
import Network.TigHTTP.Client
import Network.TigHTTP.Types

import qualified Data.ByteString as BS

main :: IO ()
main = do
	addr : pth : _ <- getArgs
	h <- connectTo addr $ PortNumber 80
	r <- request h $ get addr 80 pth
	_ <- runPipe $ responseBody r =$= finally printP (putStrLn "")
	return ()

printP :: MonadIO m => Pipe BS.ByteString () m ()
printP = await >>= maybe (return ()) (\s -> liftIO (BS.putStr s) >> printP)
