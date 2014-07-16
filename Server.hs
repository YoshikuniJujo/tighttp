{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

module Server (httpServer) where

import Control.Monad
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Time
import System.Locale

import HttpTypes
import Data.HandleLike

httpServer :: HandleLike h => h -> BS.ByteString -> HandleMonad h BS.ByteString
httpServer cl cnt = do
	h <- hlGetHeader cl
	let req = parse h
	b <- hlGet cl $ requestBodyLength req
	let req' = postAddBody req b
	hlDebug cl "critical" . BSC.pack . (++ "\n") $ show req'
	mapM_ (hlDebug cl "critical" . (`BS.append` "\n")) .
		catMaybes . showRequest $ req'
	hlPutStrLn cl . crlf . catMaybes . showResponse $ mkContents cnt
	return $ getPostBody req'

mkContents :: BS.ByteString -> Response
mkContents cnt = Response {
	responseVersion = Version 1 1,
	responseStatusCode = OK,
	responseDate = readTime defaultTimeLocale
		"%a, %d %b %Y %H:%M:%S" "Wed, 07 May 2014 02:27:34",
	responseContentLength = ContentLength $ BS.length cnt,
	responseContentType = ContentType ("text", "plain"),
	responseServer = Nothing,
	responseLastModified = Nothing,
	responseETag = Nothing,
	responseAcceptRanges = Nothing,
	responseConnection = Nothing,
	responseOthers = [],
	responseBody = cnt
 }

hlGetHeader :: HandleLike h => h -> HandleMonad h [BS.ByteString]
hlGetHeader h = do
	l <- hlGetLine h
	if (BS.null l) then return [] else (l :) `liftM` hlGetHeader h

-- dropCR :: BS.ByteString -> BS.ByteString
-- dropCR s = if myLast "dropCR" s == '\r' then BS.init s else s

crlf :: [BS.ByteString] -> BS.ByteString
crlf = BS.concat . map (+++ "\r\n")
