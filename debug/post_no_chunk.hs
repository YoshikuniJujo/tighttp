{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

import Control.Applicative
import "monads-tf" Control.Monad.State
import Data.HandleLike
import Data.Pipe
import System.Environment
import Network

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC

import Network.TigHTTP.Client
import Network.TigHTTP.Types

main :: IO ()
main = do
	addr : spn : pth : pst : _ <- getArgs
	(pn :: Int) <- readIO spn
	sv <- flip DebugHandle (Just "low") <$>
		connectTo addr (PortNumber $ fromIntegral pn)
	p <- request sv $ post addr pn pth -- "/salamander/first/talk.pl"
		(Just $ length pst, LBSC.pack pst)
--		(Just 28, "user=hello&tips=hoge%0ahi+ge")
--		(LBS.fromChunks ["I am client.\n", "You are server.\n"])
	_ <- runPipe $ responseBody p =$= printP
	return ()

printP :: MonadIO m => Pipe BSC.ByteString () m ()
printP = await >>= maybe (return ()) (\s -> liftIO (BSC.putStr s) >> printP)
