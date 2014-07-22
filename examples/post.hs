{-# LANGUAGE PackageImports #-}

import Control.Monad
import "monads-tf" Control.Monad.Trans
import System.Environment
import Data.Pipe
import Network
import Network.TigHTTP.Client
import Network.TigHTTP.Types

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
	addr : pth : msgs <- getArgs
	let msg = LBS.fromChunks $ map BSC.pack msgs
	h <- connectTo addr $ PortNumber 80
	r <- request h $ post addr 80 pth
		(Just . fromIntegral $ LBS.length msg, msg)
	void . runPipe $ responseBody r =$= (printP `finally` putStrLn "")

printP :: MonadIO m => Pipe BSC.ByteString () m ()
printP = await >>= maybe (return ()) (\s -> liftIO (BSC.putStr s) >> printP)
