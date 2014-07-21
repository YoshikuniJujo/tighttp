module Network.TigHTTP.Types (
	-- * Request and Response
	Request(..), Get(..), Post(..), Response(..),

	-- * Header Types

	-- ** First Line
	Version(..), Uri(..), RequestType(..), StatusCode(..),

	-- ** General Header
	CacheControl(..), Connection(..), TransferEncoding(..),

	-- ** Request Header
	Accept(..), AcceptEncoding(..), AcceptLanguage(..), Host(..),

	-- ** Response Header
	-- ** Entity Header
	ContentLength(..), ContentType(..), Type(..), Subtype(..),

	-- ** Basic Types
	Parameter(..),
	Charset(..),

	Product(..),
	Qvalue(..),
	) where

import Network.TigHTTP.HttpTypes
