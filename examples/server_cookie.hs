{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Concurrent
import Data.Pipe
import Data.Time
import System.IO
import System.Environment
import Network
import Network.TigHTTP.Server
import Network.TigHTTP.Types

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

cookie1, cookie2 :: SetCookie
cookie1 = (setCookie "hello" "world") {
--	cookieExpires = Just $ UTCTime (fromGregorian 2014 9 21) 36000,
	cookieMaxAge = Just 10,
	cookieSecure = False
	}
cookie2 = (setCookie "good-bye" "Nancy") {
	cookieExpires = Just $ UTCTime (fromGregorian 2015 9 21) 36000,
--	cookieMaxAge = Just $ 60 * 60 * 24,
	cookieMaxAge = Just 0,
--	cookieDomain = Just "www.hoge.jp",
	cookieDomain = Just "localhost",
	cookiePath = Just "/",
	cookieSecure = False
	}

main :: IO ()
main = do
	as <- getArgs
	soc <- listenOn $ PortNumber 80
	forever $ do
		(h, _, _) <- accept soc
		void . forkIO $ do
			req <- getRequest h
			print $ requestPath req
			case req of
				RequestGet _ _ g -> print $ getCookie g
				_ -> return ()
			putResponse h
				((response :: LBS.ByteString -> Response Pipe Handle)
					. LBS.fromChunks $ map BSC.pack as) {
						responseSetCookie = [cookie1, cookie2]
						{-
						responseOthers = [
							("Set-Cookie", "Name=tanaka;"),
							("Set-Cookie", "Addr=tokyo;")
							]
						-}
						}
