
---------------------------------------
-- T13: Web Programming with Servant --
---------------------------------------

-- # Outline

-- 1) Introduction
-- 2) Coding

-- # Preparations for 2)

-- git clone git@github.com:haskell-servant/cufp-2015
-- (or: git clone https://github.com/haskell-servant/cufp-2015)
-- cd cufp-2015
-- ./setup.sh

-- # Introduction

-- ## How his introduction works

-- It's a compilable Haskell module. Here's proof:

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Part1 where

import           Control.Monad.Trans.Either
import           Data.Text
import           Network.Wai
import           Network.Wai.Handler.Warp as Warp
import           Servant
import           Servant.API.ContentTypes ()

-- And it includes doctest examples.

-- $ >>> 1 + 2 :: Int
-- 3

-- This hopefully allows to make this
-- introduction very interactive.
-- Questions and comments welcome.

-- ## API DSL

-- The central idea of servant is that APIs can
-- be expressed as types. Type synonyms to be exact:

type SimpleGet = Get '[JSON] [Int]

type SimplePost = Post '[JSON] String


-- There's two important infix operators (on the type level):

-- 1) The sequential operator, aka bird-face: :>

type SimplePath = "foo" :> Get '[JSON] [Int]

-- 2) The alternative operator: :<|>

type SimpleAlternative =
       Get '[JSON] [Int]
  :<|> Post '[JSON] String


-- Of course, these operators can be combined:

type Combined =
       "list" :> Get '[JSON] [Int]
  :<|> "list" :> "poke" :> Post '[JSON] String

-- Note that so far these are just types that describe APIs
-- There is no executable piece of code yet.

-- ## Writing a Server to an API

-- `serve` let's us create wai `Application`s for all types that
-- have a `HasServer` instance.

type Simple = Get '[JSON] [Int]

simpleApp :: Application
simpleApp = serve simple simpleServer

simple :: Proxy Simple
simple = Proxy

simpleServer :: Server Simple
simpleServer = error "nyi"

-- $ >>> :type (undefined :: Server Simple)
-- (undefined :: Server Simple) :: EitherT ServantErr IO [Int]

simpleRun :: IO ()
simpleRun = Warp.run 8080 simpleApp


-- We can change the type. We can e.g.
-- - add a path,
-- - change the return type,
-- - add an endpoint.

-- The typesystem will always make sure that our server always implements the
-- specified API.

-- # Further Combinators

-- `DemoReqBody` is just used to demonstrate the principle of so-called
-- combinators in servant.

data DemoReqBody a

-- It's an uninhabited type. It can be used in API specifications.

type SimpleBody = DemoReqBody :> Get '[PlainText] Text

-- To be able to implement a `Server` for this API we need a `HasServer`
-- instance. This instance -- together with its associated type (`ServerT`) --
-- gives `DemoReqBody` its meaning. (This is also called an interpretation
-- of the type level term.)

instance HasServer api => HasServer (DemoReqBody :> api) where
  type ServerT (DemoReqBody :> api) m = Text -> ServerT api m
  route = error "DemoReqBody.route: nyi"

-- The `HasServer` instances of the different combinators of the API
-- description pin down the type of the server implementation.

-- $ >>> :type (undefined :: Server SimpleBody)
-- (undefined :: Server SimpleBody)
--   :: Text -> EitherT ServantErr IO Text

simpleBodyServer :: Server SimpleBody
simpleBodyServer s = return $ Data.Text.reverse s

simpleBodyRun :: IO ()
simpleBodyRun =
  Warp.run 8080 (serve (Proxy :: Proxy SimpleBody) simpleBodyServer)

-- It's not hard to write your own combinators. But servant already provides
-- a bunch of common combinators, e.g. `ReqBody`.


-- from here on only notes: (TODO)

-- abstraction with haskell types
-- you can write your own combinators, but servant comes with these:
--   (list of other combinators with examples (maybe in one big API?))

-- ## API Interpretations

-- - Client
-- -
-- - Server

-- # Game

-- Everything is fair game, e.g. invalid requests or responses are not
-- considered playing dirty.

type Foo = EitherT