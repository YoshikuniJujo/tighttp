{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.TigHTTP.Papillon (
	ContentType(..), parseContentType, showContentType,
) where

import Data.Char
import Text.Papillon

import Data.ByteString (ByteString, append)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

data ContentType = ContentType
	BS.ByteString
	BS.ByteString
	deriving Show

parseContentType :: BS.ByteString -> ContentType
parseContentType ct = case runError . contentType $ parse ct of
	Left _ -> error "parseContentType"
	Right (r, _) -> r

showContentType :: ContentType -> BS.ByteString
showContentType (ContentType t st) = t `BS.append` "/" `BS.append` st

[papillon|

source: ByteString

contentType :: ContentType
	= c:token '/' sc:token r:rst		{ ContentType c (sc `append` r) }

token :: ByteString
	= t:<isAlpha>+			{ pack t }

rst :: ByteString
	= r:<(/= '\n')>*		{ pack r }

|]

instance Source ByteString where
	type Token ByteString = Char
	data Pos ByteString = NoPos
	getToken = BSC.uncons
	initialPos = NoPos
	updatePos _ _ = NoPos
