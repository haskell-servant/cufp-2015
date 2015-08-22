{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module MarkdownCT where

import Servant
import Servant.Docs (API, markdown)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Network.HTTP.Media as M


data Markdown

instance Accept Markdown where
    contentType _ = "text" M.// "markdown" M./: ("charset", "utf-8")

instance MimeRender Markdown API where
    mimeRender _ = pack . markdown
