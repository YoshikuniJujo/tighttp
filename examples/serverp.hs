{-# LANGUAGE PackageImports #-}

import Control.Monad
import "monads-tf" Control.Monad.Trans
import Control.Concurrent
import Data.Pipe
import System.IO
import System.Environment
import Network
import Network.TigHTTP.Server
import Network.TigHTTP.Types

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
	as <- getArgs
	soc <- listenOn $ PortNumber 80
	forever $ do
		(h, _, _) <- accept soc
		void . forkIO $ do
			r <- getRequest h
			print $ requestPath r
			void . runPipe $
				requestBody r =$= printP `finally` putStrLn ""
			putResponse h
				. (response :: LBS.ByteString -> Response Pipe Handle)
				. LBS.fromChunks $ map BSC.pack as

printP :: MonadIO m => Pipe BSC.ByteString () m ()
printP = await >>= maybe (return ()) (\s -> liftIO (BSC.putStr s) >> printP)
