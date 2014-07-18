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

import Network.TigHTTP.Token

data ContentType = ContentType
	BS.ByteString
	BS.ByteString
	[(BS.ByteString, BS.ByteString)]
	deriving Show

parseContentType :: BS.ByteString -> ContentType
parseContentType ct = case runError . contentType $ parse ct of
	Left _ -> error "parseContentType"
	Right (r, _) -> r

showContentType :: ContentType -> BS.ByteString
showContentType (ContentType t st ps) = t `BS.append` "/" `BS.append` st
	`BS.append` showParameters ps

showParameters :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
showParameters [] = ""
showParameters ((a, v) : ps) = "; " `BS.append` a `BS.append` "=" `BS.append` v
	`BS.append` showParameters ps

testQuoted :: BS.ByteString -> BS.ByteString
testQuoted bs = case runError . quotedString $ parse bs of
	Left _ -> error "testQuoted"
	Right (r, _) -> r

bsconcat = BS.concat

[papillon|

source: ByteString

contentType :: ContentType
	= c:token '/' sc:token ps:(';' ' '* p:parameter { p })*
						{ ContentType c sc ps }

token :: ByteString
	= t:<isTokenChar>+			{ pack t }

rst :: ByteString
	= r:<(/= '\n')>*			{ pack r }

quotedString :: ByteString
	= '"' t:(qt:qdtext { qt } / qp:quotedPair { pack [qp] })* '"'
						{ bsconcat t }

quotedPair :: Char
	= '\\' c:<isAscii>			{ c }

crlf :: () = '\r' '\n'

lws :: () = _:crlf _:(' ' / '\t')+

text :: ByteString
	= ts:(cs:<isTextChar>+ { cs } / _:lws { " " })+		{ pack $ concat ts }

qdtext :: ByteString
	= ts:(cs:<isQdtextChar>+ { cs } / _:lws { " " })+	{ pack $ concat ts }

parameter :: (ByteString, ByteString)
	= a:attribute '=' v:value				{ (a, v) }

attribute :: ByteString = t:token				{ t }

value :: ByteString
	= t:token						{ t }
	/ qs:quotedString					{ qs }

|]

instance Source ByteString where
	type Token ByteString = Char
	data Pos ByteString = NoPos
	getToken = BSC.uncons
	initialPos = NoPos
	updatePos _ _ = NoPos
