{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This file defines a new content-type for markdown.
module MarkdownCT where

import           Data.ByteString.Lazy.Char8 (pack, unpack)
import qualified Network.HTTP.Media as M
import           Servant.API

newtype Markdown = Markdown { unMarkdown :: String }
  deriving (Eq, Show, Read)

-- | What the 'Accept' and 'Content-Type' headers should or are expected to
-- look like
instance Accept Markdown where
    contentType _ = "text" M.// "markdown" M./: ("charset", "utf-8")

instance MimeRender Markdown Markdown where
    mimeRender _ = pack . unMarkdown

instance MimeUnrender Markdown Markdown where
    mimeUnrender _ = return . Markdown . unpack
