{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

module Network.TigHTTP.Client (request, get, post) where

import Control.Applicative
import Control.Arrow hiding ((+++))
import Control.Monad
import "monads-tf" Control.Monad.State (lift)
import Data.Pipe
import Data.Pipe.List
import Numeric

import Network.TigHTTP.HttpTypes
import Data.HandleLike

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

request, httpGet :: HandleLike h => h -> Request h -> HandleMonad h (Response h)
request = httpGet
httpGet sv req = do
	putRequest sv req
	src <- hGetHeader sv
	let res = parseResponse src
--	mapM_ (hlDebug sv "critical" . (`BS.append` "\n") . BS.take 100)
--		. catMaybes =<< showResponse sv res
	let res' = putResponseBody sv res
		(httpContent (contentLength <$> responseContentLength res) sv)
	return res'

get :: HostName -> Int -> FilePath -> Request h
get hn pn fp = RequestGet (Uri $ BSC.pack fp) (Version 1 1)
	Get {
		getHost = uncurry Host . second Just <$> Just (BSC.pack hn, pn),
		getUserAgent = Just [Product "Mozilla" (Just "5.0")],
		getAccept = Just [Accept ("text", "plain") (Qvalue 1.0)],
		getAcceptLanguage = Just [AcceptLanguage "ja" (Qvalue 1.0)],
		getAcceptEncoding = Just [],
		getConnection = Just [Connection "keep-alive"],
		getCacheControl = Just [MaxAge 0],
		getOthers = []
	 }

putResponseBody :: HandleLike h =>
	h -> Response h -> Pipe () BS.ByteString (HandleMonad h) () -> Response h
putResponseBody _ res rb = res { responseBody = rb }

httpContent :: HandleLike h =>
	Maybe Int -> h -> Pipe () BS.ByteString (HandleMonad h) ()
httpContent (Just n) h = yield =<< lift (hlGet h n)
httpContent _ h = getChunked h `onBreak` readRest h

getChunked :: HandleLike h => h -> Pipe () BS.ByteString (HandleMonad h) ()
getChunked h = do
	(n :: Int) <- lift $ (fst . head . readHex . BSC.unpack) `liftM` hlGetLine h
	lift . hlDebug h "medium" . BSC.pack . (++ "\n") $ show n
	case n of
		0 -> return ()
		_ -> do	r <- lift $ hlGet h n
			"" <- lift $ hlGetLine h
			yield r
			getChunked h

readRest :: HandleLike h => h -> HandleMonad h ()
readRest h = do
	hlDebug h "low" "begin readRest\n"
	(n :: Int) <- (fst . head . readHex . BSC.unpack) `liftM` hlGetLine h
	hlDebug h "medium" . BSC.pack . (++ "\n") $ show n
	case n of
		0 -> return ()
		_ -> do	_ <- hlGet h n
			"" <- hlGetLine h
			readRest h

post :: HandleLike h =>
	HostName -> Int -> FilePath -> Maybe Int -> LBS.ByteString -> Request h
post hn pn fp len pst = RequestPost (Uri $ BSC.pack fp) (Version 1 1)
	Post {
		postHost = uncurry Host . second Just <$> hnpn,
		postUserAgent = Just [Product "Mozilla" (Just "5.0")],
		postAccept = Just [Accept ("text", "plain") (Qvalue 1.0)],
		postAcceptLanguage = Just [AcceptLanguage "ja" (Qvalue 1.0)],
		postAcceptEncoding = Just [],
		postConnection = Just [Connection "keep-alive"],
		postCacheControl = Just [MaxAge 0],
		postContentType = Just $ ContentType Text Plain [],
		postContentLength = cl,
		postTransferEncoding = ch,
		postOthers = [],
		postBody = fromList cnt
	 }
	where
	hnpn = Just (BSC.pack hn, pn)
	(cl, ch, cnt) = case len of
		Just l -> (Just $ ContentLength l, Nothing, LBS.toChunks pst)
		_ -> (Nothing, Just Chunked, [mkChunked $ LBS.toChunks pst])

mkChunked :: [BS.ByteString] -> BS.ByteString
mkChunked = flip foldr ("0" `BS.append` "\r\n\r\n") $ \b ->
	BS.append (BSC.pack (showHex (BS.length b) "") `BS.append` "\r\n"
		`BS.append` b `BS.append` "\r\n")

hGetHeader :: HandleLike h => h -> HandleMonad h [BS.ByteString]
hGetHeader h = do
	l <- hlGetLine h
	if BS.null l then return [] else (l :) `liftM` hGetHeader h
