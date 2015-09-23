{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PackageImports #-}

module Network.TigHTTP.Server (
	getRequest, putResponse, response,
--	ContentType(..), Type(..), Subtype(..), Parameter(..), Charset(..),
--	Request(..), Get(..), Post(..),
	requestBody,
	requestPath,
	) where

import Control.Monad
import "monads-tf" Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Time
import Data.Pipe
import Data.Pipe.List
import System.Locale

import Network.TigHTTP.HttpTypes
import Data.HandleLike

import Numeric

getRequest :: HandleLike h => h -> HandleMonad h (Request h)
getRequest cl = do
	hlDebug cl "medium" "begin getRequest\n"
	h <- hlGetHeader' cl
	hlDebug cl "medium" $ "hlGetHeader' cl: " `BS.append`
		BSC.pack (show h ++ "\n")
	let req = parseReq h
	r <- case req of
		RequestPost {} -> return . httpContent cl $
			requestBodyLength req
		_ -> return (return ())
--	mapM_ (hlDebug cl "critical" . (`BS.append` "\n")) .
--		catMaybes =<< showRequest cl req
	return $ putPostBody cl req r

response :: (
	PipeClass p, Monad (p () BS.ByteString (HandleMonad h)),
	HandleLike h) => LBS.ByteString -> Response p h
response = mkContents . mkChunked . LBS.toChunks

httpContent :: HandleLike h =>
	h -> Maybe Int -> Pipe () BS.ByteString (HandleMonad h) ()
httpContent h (Just n) = lift (hlGet h n) >>= yield
httpContent h _ = getChunked h

getChunked :: HandleLike h => h -> Pipe () BS.ByteString (HandleMonad h) ()
getChunked h = do
	(n :: Int) <- lift $ (fst . head . readHex . BSC.unpack) `liftM` hlGetLineNotNull h
	lift . hlDebug h "low" . BSC.pack . (++ "\n") $ show n
	case n of
		0 -> return ()
		_ -> do	r <- lift $ hlGet h n
--			"" <- lift $ hlGetLine h
			yield r
			getChunked h

hlGetLineNotNull :: HandleLike h => h -> HandleMonad h BS.ByteString
hlGetLineNotNull h = do
	ln <- hlGetLine h
	if BS.null ln then hlGetLineNotNull h else return ln

mkChunked :: [BS.ByteString] -> LBS.ByteString
mkChunked = flip foldr ("0\r\n\r\n") $ \b ->
	LBS.append (LBSC.pack (showHex (BS.length b) "") `LBS.append` "\r\n"
		`LBS.append` LBS.fromStrict b `LBS.append` "\r\n")

mkContents :: (
	PipeClass p, Monad (p () BS.ByteString (HandleMonad h)),
	HandleLike h ) => LBS.ByteString -> Response p h
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
	responseSetCookie = [],
	responseOthers = [],
	responseBody = fromList $ LBS.toChunks cnt
 }

hlGetHeader' :: HandleLike h => h -> HandleMonad h [BS.ByteString]
hlGetHeader' h = do
	l <- hlGetLine h
	hlDebug h "medium" . BSC.pack $ show l
	if BS.null l then hlGetHeader' h else (l :) `liftM` hlGetHeader h

hlGetHeader :: HandleLike h => h -> HandleMonad h [BS.ByteString]
hlGetHeader h = do
	l <- hlGetLine h
	hlDebug h "medium" l
	if BS.null l then return [] else (l :) `liftM` hlGetHeader h
