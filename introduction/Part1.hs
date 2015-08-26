
--------------------------
-- todo: official title --
--------------------------

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
{-# LANGUAGE TypeOperators #-}

module Part1 where

import           Control.Monad.Trans.Either
import           Network.Wai
import           Servant

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

type SimplePost = Post '[PlainText] String


-- There's two important infix operators (on the type level):

-- 1) The sequential operator, aka bird-face: :>

type SimplePath = "foo" :> Get '[JSON] [Int]

-- 2) The alternative operator: :<|>

type SimpleAlternative =
       Get '[JSON] [Int]
  :<|> Post '[PlainText] String


-- Note that so far these are just types that describe APIs
-- There is no executable piece of code yet.


-- TODO:

-- - The combinators

-- ## API Interpretations

-- - Client
-- -
-- - Server

-- # Game

-- Everything is fair game, e.g. invalid requests or responses are not
-- considered playing dirty.
