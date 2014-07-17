{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Network.TigHTTP.Client (httpGet, httpPost) where

import Control.Monad
import Data.Maybe

import Network.TigHTTP.HttpTypes
import Data.HandleLike

import qualified Data.ByteString as BS

httpGet :: HandleLike h => h -> HandleMonad h BS.ByteString
httpGet sv = do
	hlPutStrLn sv request
	src <- hGetHeader sv
	let res = parseResponse src
	cnt <- hlGet sv $ maybe 10 contentLength $ responseContentLength res
	let res' = res { responseBody = cnt }
	mapM_ (hlDebug sv "critical" . (`BS.append` "\n"))
		. catMaybes $ showResponse res'
	return cnt

httpPost :: HandleLike h => h -> BS.ByteString -> HandleMonad h BS.ByteString
httpPost sv cnt = do
	hlPutStrLn sv . requestToString $ post cnt
	res <- parseResponse `liftM` hGetHeader sv
	cnt' <- hlGet sv $ maybe 10 contentLength $ responseContentLength res
	let res' = res { responseBody = cnt' }
	hlDebug sv "critical" . (`BS.append` "\n") $ responseToString res'
	return cnt'

hGetHeader :: HandleLike h => h -> HandleMonad h [BS.ByteString]
hGetHeader h = do
	l <- hlGetLine h
	if (BS.null l) then return [] else (l :) `liftM` hGetHeader h

crlf :: [BS.ByteString] -> BS.ByteString
crlf = BS.concat . map (+++ "\r\n")

request :: BS.ByteString
request = crlf . catMaybes . showRequest . RequestGet (Uri "/") (Version 1 1) $
	Get {
		getHost = Just . Host "www.google.co.jp" $ Just 443,
		getUserAgent = Just [Product "Mozilla" (Just "5.0")],
		getAccept = Just [Accept ("text", "plain") (Qvalue 1.0)],
		getAcceptLanguage = Just [AcceptLanguage "ja" (Qvalue 1.0)],
		getAcceptEncoding = Just [],
		getConnection = Just [Connection "keep-alive"],
		getCacheControl = Just [MaxAge 0],
		getOthers = []
	 }

requestToString :: Request -> BS.ByteString
requestToString = crlf . catMaybes . showRequest

responseToString :: Response -> BS.ByteString
responseToString = crlf . catMaybes . showResponse

post :: BS.ByteString -> Request
post cnt = RequestPost (Uri "/") (Version 1 1) $
	Post {
		postHost = Just . Host "www.google.co.jp" $ Just 443,
		postUserAgent = Just [Product "Mozilla" (Just "5.0")],
		postAccept = Just [Accept ("text", "plain") (Qvalue 1.0)],
		postAcceptLanguage = Just [AcceptLanguage "ja" (Qvalue 1.0)],
		postAcceptEncoding = Just [],
		postConnection = Just [Connection "keep-alive"],
		postCacheControl = Just [MaxAge 0],
		postContentType = Just $ ContentType ("text", "plain"),
		postContentLength = Just . ContentLength $ BS.length cnt,
		postOthers = [],
		postBody = cnt
	 }
