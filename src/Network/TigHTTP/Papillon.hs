{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.TigHTTP.Papillon (
	ContentType(..), Type(..), Subtype(..),
		parseContentType, showContentType,
) where

import Data.Char
import Text.Papillon

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Network.TigHTTP.Token

data ContentType = ContentType Type Subtype
	[(BS.ByteString, BS.ByteString)]
	deriving Show

data Type
	= Type BS.ByteString
	| Text
	deriving Show

mkType :: BS.ByteString -> Type
mkType "text" = Text
mkType t = Type t

showType :: Type -> BS.ByteString
showType Text = "text"
showType (Type t) = t

data Subtype
	= Subtype BS.ByteString
	| Plain
	| Html
	deriving Show

mkSubtype :: BS.ByteString -> Subtype
mkSubtype "html" = Html
mkSubtype "plain" = Plain
mkSubtype s = Subtype s

showSubtype :: Subtype -> BS.ByteString
showSubtype Plain = "plain"
showSubtype Html = "html"
showSubtype (Subtype s) = s

parseContentType :: BS.ByteString -> ContentType
parseContentType ct = case runError . contentType $ parse ct of
	Left _ -> error "parseContentType"
	Right (r, _) -> r

showContentType :: ContentType -> BS.ByteString
showContentType (ContentType t st ps) = showType t
	`BS.append` "/"
	`BS.append` showSubtype st
	`BS.append` showParameters ps

showParameters :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
showParameters [] = ""
showParameters ((a, v) : ps) = "; " `BS.append` a `BS.append` "=" `BS.append` v
	`BS.append` showParameters ps

bsconcat :: [ByteString] -> ByteString
bsconcat = BS.concat

[papillon|

source: ByteString

contentType :: ContentType
	= c:token '/' sc:token ps:(';' ' '* p:parameter { p })*
	{ ContentType (mkType c) (mkSubtype sc) ps }

token :: ByteString
	= t:<isTokenChar>+			{ pack t }

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
