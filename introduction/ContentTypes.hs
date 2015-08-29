-- # Content-Types
--
------------------------------------------------------------------------------
-- ## Preamble
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
module ContentTypes where

import           Codec.Picture
import           Codec.Picture.Saving
import           Data.Proxy
import qualified Network.HTTP.Media   as M
import           Servant.API

------------------------------------------------------------------------------
-- ## Declaring a datatype for MIME types
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
------------------------------------------------------------------------------
-- ## MimeRender
--
-- Now we need to say how, when we get request that accepts "image/png", we
-- should serialize the datatype. We do that by declaring the datatype an
-- instance of 'MimeRender'. Here the datatype we'll work with is JuicyPixel's
-- 'DynamicImage'
instance MimeRender PNG DynamicImage where
    mimeRender _ = imageToPng

-- Easy! That is enough to allow us to write a server with the API:

type RenderPNGApi = Get '[PNG] DynamicImage

------------------------------------------------------------------------------
-- ## MimeUnrender
--
-- Similarly we can declare how to deserialize a request of content-type
-- "image/png" into a 'DynamicImage'.

instance MimeUnrender PNG DynamicImage where
    mimeUnrender _ = decodePng . BL.toStrict

-- Which in turn means we can accept request bodies of type 'DynamicImage'
-- with content-type "image/png".
type PNGApi = ReqBody '[PNG] DynamicImage :> Get '[PNG] DynamicImage
