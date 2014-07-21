{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

module Network.TigHTTP.Server (
	getRequest, putResponse, response,
	ContentType(..), Type(..), Subtype(..), Parameter(..), Charset(..),
	Request(..), Get(..), Post(..),
	requestBody,
	) where

import Control.Monad
import "monads-tf" Control.Monad.Trans
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Time
import Data.Pipe
import Data.Pipe.List
import System.Locale

import Network.TigHTTP.HttpTypes
import Data.HandleLike

import Numeric

getRequest :: HandleLike h => h -> HandleMonad h (Request h)
getRequest cl = do
	h <- hlGetHeader cl
	let req = parseReq h
	r <- case req of
		RequestPost {} -> return . httpContent cl $
			requestBodyLength req
		_ -> return (return ())
	mapM_ (hlDebug cl "critical" . (`BS.append` "\n")) .
		catMaybes =<< showRequest cl req
	return $ putPostBody cl req r

putResponse' :: HandleLike h => h -> LBS.ByteString -> HandleMonad h ()
putResponse' cl = putResponse cl . response

response :: HandleLike h => LBS.ByteString -> Response h
response = mkContents . mkChunked . LBS.toChunks

putResponse :: HandleLike h => h -> Response h -> HandleMonad h ()
putResponse cl res = hlPutStrLn cl . crlf . catMaybes =<< showResponse cl res

httpContent :: HandleLike h =>
	h -> Maybe Int -> Pipe () BS.ByteString (HandleMonad h) ()
httpContent h (Just n) = lift (hlGet h n) >>= yield
httpContent h _ = getChunked h

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

mkChunked :: [BS.ByteString] -> BS.ByteString
mkChunked = flip foldr ("0" `BS.append` "\r\nr\n") $ \b ->
	BS.append (BSC.pack (showHex (BS.length b) "") `BS.append` "\r\n"
		`BS.append` b `BS.append` "\r\n")
{-
mkChunked [] = "0" `BS.append` "\r\n\r\n"
mkChunked (b : bs) = BSC.pack (showHex (BS.length b) "") `BS.append` "\r\n"
	`BS.append` b `BS.append` "\r\n" `BS.append` mkChunked bs
	-}

mkContents :: HandleLike h => BS.ByteString -> Response h
mkContents cnt = Response {
	responseVersion = Version 1 1,
	responseStatusCode = OK,
	responseDate = Just $ readTime defaultTimeLocale
		"%a, %d %b %Y %H:%M:%S" "Wed, 07 May 2014 02:27:34",
	responseContentLength = Nothing, -- Just $ ContentLength $ BS.length cnt,
	responseTransferEncoding = Just Chunked,
	responseContentType = ContentType Text Plain [],
	responseServer = Nothing,
	responseLastModified = Nothing,
	responseETag = Nothing,
	responseAcceptRanges = Nothing,
	responseConnection = Nothing,
	responseOthers = [],
	responseBody = fromList [cnt]
 }

hlGetHeader :: HandleLike h => h -> HandleMonad h [BS.ByteString]
hlGetHeader h = do
	l <- hlGetLine h
	if BS.null l then return [] else (l :) `liftM` hlGetHeader h

-- dropCR :: BS.ByteString -> BS.ByteString
-- dropCR s = if myLast "dropCR" s == '\r' then BS.init s else s

-- crlf :: [BS.ByteString] -> LBS.ByteString
-- crlf = LBS.fromChunks . map (`LBS.append` "\r\n")

crlf :: [BS.ByteString] -> BS.ByteString
crlf = BS.concat . map (`BS.append` "\r\n")
