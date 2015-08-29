-- # Content-Types {{{
------------------------------------------------------------------------------
-- ## Preamble {{{
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module ContentTypes where

import           Codec.Picture
import           Codec.Picture.Saving
import qualified Data.ByteString.Lazy     as BL
import           Data.Proxy
import qualified Network.HTTP.Media       as M
import           Network.Wai              (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant
import qualified Servant.JuicyPixels      as JP

-- }}}
------------------------------------------------------------------------------
-- ## Introduction {{{
--
-- The content-type list remains a little mysterious. How does servant know how
-- to serialize and deserialize data? In this file, we answer that question,
-- and show how easy it is to add new content-types.
--
-- As some background, HTTP requests are expected to set their `Accept` header
-- to whatever content-types they accept, and their `Content-Type` header to
-- the content-type of the request body. HTTP responses are also expected to
-- set the `Content-Type` header.
--
--
-- Much of the code in this module is inspired by, or directly taken from,
-- Timo von Holz's servant-JuicyPixels package
-- }}}
------------------------------------------------------------------------------
-- ## Declaring a datatype for MIME types {{{
--
-- We need a new datatype that we can put in the content-type list. Let's do
-- PNG.
data PNG

-- We declare what MIME type it represents using the great 'Network.HTTP.Media'
-- package
instance Accept PNG where
    contentType _ = "image" M.// "png"

-- Now servant knows how to match against `Accept` headers in the request, and
-- what to place as the `Content-Type` v
--
-- }}}
------------------------------------------------------------------------------
-- ## MimeRender {{{
--
-- Now we need to say how, when we get request that accepts "image/png", we
-- should serialize the datatype. We do that by declaring the datatype an
-- instance of 'MimeRender'. Here the datatype we'll work with is JuicyPixel's
-- 'DynamicImage'
instance MimeRender PNG DynamicImage where
    mimeRender _ = imageToPng

-- Easy! That is enough to allow us to write a server with the API:

type RenderPNGApi = Get '[PNG] DynamicImage

-- }}}
------------------------------------------------------------------------------
-- ## MimeUnrender {{{
--
-- Similarly we can declare how to deserialize a request of content-type
-- "image/png" into a 'DynamicImage'.

instance MimeUnrender PNG DynamicImage where
    mimeUnrender _ = decodePng . BL.toStrict

-- }}}
------------------------------------------------------------------------------
-- ## Conversion {{{
--
-- Timo's servant-JuicyPixels already has these instances, and many more
-- besides. Let's take them out for a test spin.

-- A single endpoint, which expects and returns the same type, but in a variety
-- of content-types
type ConversionApi
     = ReqBody '[JP.BMP, JP.GIF, JP.JPEG 50, JP.PNG, JP.TIFF, JP.RADIANCE] DynamicImage
    :> Post '[JP.BMP, JP.GIF, JP.JPEG 50, JP.PNG, JP.TIFF, JP.RADIANCE] DynamicImage

conversionApi :: Proxy ConversionApi
conversionApi = Proxy

-- Whatever we get, we return.
server :: Server ConversionApi
server = return

conversion :: Application
conversion = serve conversionApi server

main :: IO ()
main = run 8001 conversion

-- Try it out!
-- $ curl localhost:8001 -H "Content-Type: image/png"  \
--                       -H "Accept: image/jpeg"  \
--                       --data-binary "@haskell-logo.png" \
--                       > haskell-logo.jpeg
-- }}}
-- }}}
