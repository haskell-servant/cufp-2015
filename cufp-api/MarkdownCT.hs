{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This file defines a new content-type for markdown.
module MarkdownCT where

import Servant
import Servant.Docs (API, markdown)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Network.HTTP.Media as M

data Markdown

-- | What the 'Accept' and 'Content-Type' headers should or are expected to
-- look like
instance Accept Markdown where
    contentType _ = "text" M.// "markdown" M./: ("charset", "utf-8")

-- | We declare that we know how to convert the @API@ datatype for servant-docs
-- into the 'text/markdown' content-type
instance MimeRender Markdown API where
    mimeRender _ = pack . markdown
