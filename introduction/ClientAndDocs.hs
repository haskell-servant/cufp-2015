-- # servant-client and servant-docs
--
------------------------------------------------------------------------------
-- ## Preamble
--
-- Imports, pragmas, and some APIs to play around with.
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ClientAndDocs where

import Servant
-- Qualified imports are not necessary, but will help make clear where
-- functions and datatypes we use are coming from.
import qualified Servant.Docs  as Docs
import qualified Servant.Client as Client
import Control.Monad.Trans.Either


type SimpleApi = "simple" :> Get '[JSON] Int

simpleApi :: Proxy SimpleApi
simpleApi = Proxy

type SimpleWithCapture =
        SimpleApi
   :<|> "int" :> Capture "favourite number" Int :> Get '[JSON] ()

simpleWithCapture :: Proxy (SimpleApi :<|> SimpleWithCapture)
simpleWithCapture = Proxy

------------------------------------------------------------------------------
-- ## Introduction

-- If API descriptions are expressions of a DSL, what are their
-- interpretations?
--
-- Usually languages have two very common interpretations - evaluation and
-- pretty-printing - but not many more besides. The servant DSL on the other
-- hand has many.
--

------------------------------------------------------------------------------
-- ## Docs
-- The simplest is documentation-generation.
--

simpleApiDocs :: Docs.API
simpleApiDocs = Docs.docs simpleApi

-- 'Docs.API' is an intermediary representation that stores processed
-- information about the datatype. We can then render it in different formats.
-- Currently, however, the only supported format is markdown
simpleApiMarkdown :: String
simpleApiMarkdown = Docs.markdown simpleApiDocs

-- This won't yet compile, since 'Docs.docs' needs to know how to generate
-- samples of the datatatypes used in the API.
--
--              Almost always these two types will
--              be the same, but we have two for
--              the rare cases where the type of
--              the sample is actually different
--                      |   |
--                     vvv vvv
instance Docs.ToSample Int Int where
    -- The argument to 'toSample' is a Proxy used to pick instances, and can
    -- always be ignored
    toSample _         --  toSample :: Proxy Int -> Maybe Int
        = Just 1729
    -- Sometimes, it is useful to have multiple samples to e.g. represent the
    -- multiple constructors of a datatype. For that one case define the
    -- toSamples method, Both 'toSample' and 'toSamples' have default
    -- implementations in terms of one another.

-- Now we're ready. Try "simpleApiMarkdown" in ghci:
--
-- ## GET /simple
--
-- #### Response:
--
-- - Status code 200
-- - Headers: []
--
-- - Supported content types are:
--
--     - `application/json`
--
-- - Response body as below.
--
-- ```javascript
-- 1729
-- ```


------------------------------------------------------------------------------
-- ## Client
--
-- Another interpretation of the API is client function generation. The
-- 'Client.client' function generates one function for each endpoint in the
-- API, with the appropriate type.

s :: EitherT Client.ServantError IO Int
s = Client.client simpleApi (Client.BaseUrl Client.Http "localhost" 8000)


