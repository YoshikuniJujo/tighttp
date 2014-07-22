import Control.Monad
import Control.Concurrent
import System.Environment
import Network
import Network.TigHTTP.Server

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
	as <- getArgs
	soc <- listenOn $ PortNumber 80
	forever $ do
		(h, _, _) <- accept soc
		void . forkIO $ do
			req <- getRequest h
			print $ requestPath req
			putResponse h . response . LBS.fromChunks $ map BSC.pack as
