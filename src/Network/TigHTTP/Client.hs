{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

module Network.TigHTTP.Client (ClientM, httpGet, get, post) where

import Control.Applicative
import Control.Arrow hiding ((+++))
import Control.Monad
import "monads-tf" Control.Monad.State (lift)
import Data.Maybe
import Data.Pipe
import Data.Pipe.List
import Numeric

import Network.TigHTTP.HttpTypes
import Data.HandleLike

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS

type ClientM h = HandleMonad h

httpGet :: HandleLike h => h -> Request h -> ClientM h (Response h)
httpGet sv req = do
	hlPutStrLn sv =<< encodeRequest sv req
	src <- hGetHeader sv
	let res = parseResponse src
	mapM_ (hlDebug sv "critical" . (`BS.append` "\n") . BS.take 100)
		. catMaybes =<< showResponse sv res
	let res' = putResponseBody sv res
		(httpContent (contentLength <$> responseContentLength res) sv)
	return res'

get :: Maybe (BS.ByteString, Int) -> Request h
get = request

encodeRequest :: HandleLike h => h -> Request h -> HandleMonad h BS.ByteString
encodeRequest h req = (crlf . catMaybes) `liftM` showRequest h req

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
	lift . hlDebug h "critical" . BSC.pack . (++ "\n") $ show n
	case n of
		0 -> return ()
		_ -> do	r <- lift $ hlGet h n
			"" <- lift $ hlGetLine h
			yield r
			getChunked h

readRest :: HandleLike h => h -> HandleMonad h ()
readRest h = do
	hlDebug h "critical" "begin readRest\n"
	(n :: Int) <- (fst . head . readHex . BSC.unpack) `liftM` hlGetLine h
	hlDebug h "critical" . BSC.pack . (++ "\n") $ show n
	case n of
		0 -> return ()
		_ -> do	_ <- hlGet h n
			"" <- hlGetLine h
			readRest h

post :: HandleLike h => Maybe (BS.ByteString, Int) -> LBS.ByteString -> Request h
post hn pst = post_ hn (mkChunked $ LBS.toChunks pst)

mkChunked :: [BS.ByteString] -> BS.ByteString
mkChunked = flip foldr ("0" `BS.append` "\r\n\r\n") $ \b ->
	BS.append (BSC.pack (showHex (BS.length b) "") `BS.append` "\r\n"
		`BS.append` b `BS.append` "\r\n")

hGetHeader :: HandleLike h => h -> HandleMonad h [BS.ByteString]
hGetHeader h = do
	l <- hlGetLine h
	if BS.null l then return [] else (l :) `liftM` hGetHeader h

crlf :: [BS.ByteString] -> BS.ByteString
crlf = BS.concat . map (+++ "\r\n")

request :: Maybe (BS.ByteString, Int) -> Request h
request hnpn = RequestGet (Uri "/") (Version 1 1)
	Get {
		getHost = uncurry Host . second Just <$> hnpn,
		getUserAgent = Just [Product "Mozilla" (Just "5.0")],
		getAccept = Just [Accept ("text", "plain") (Qvalue 1.0)],
		getAcceptLanguage = Just [AcceptLanguage "ja" (Qvalue 1.0)],
		getAcceptEncoding = Just [],
		getConnection = Just [Connection "keep-alive"],
		getCacheControl = Just [MaxAge 0],
		getOthers = []
	 }

post_ :: HandleLike h => Maybe (BS.ByteString, Int) -> BS.ByteString -> Request h
post_ hnpn cnt = RequestPost (Uri "/") (Version 1 1)
	Post {
		postHost = uncurry Host . second Just <$> hnpn,
		postUserAgent = Just [Product "Mozilla" (Just "5.0")],
		postAccept = Just [Accept ("text", "plain") (Qvalue 1.0)],
		postAcceptLanguage = Just [AcceptLanguage "ja" (Qvalue 1.0)],
		postAcceptEncoding = Just [],
		postConnection = Just [Connection "keep-alive"],
		postCacheControl = Just [MaxAge 0],
		postContentType = Just $ ContentType Text Plain [],
		postContentLength = Nothing, --Just . ContentLength $ BS.length cnt,
		postTransferEncoding = Just Chunked,
		postOthers = [],
		postBody = fromList [cnt]
	 }
