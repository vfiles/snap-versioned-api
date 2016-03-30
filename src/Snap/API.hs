{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.API ( requireBody
                , requireBody'
                , runAPI
                , runAPI'
                , parseBody
                , parseBody'
                , module Snap.API.Utils
                )
                 where

--------------------------------------------------------------------------------

import Snap.API.Versions
import Snap.API.Utils

--------------------------------------------------------------------------------

import Control.Applicative

import Data.Aeson
import Data.Aeson.Versions

import Data.Functor.Identity

import Data.Int

import Network.HTTP.Media

import Snap

-- | Attempts to parse a JSON structure from a request body
--   Enforces a maximum body size of 256KiB
parseBody :: (DeserializedVersion a, MonadSnap m, TraversableFromJSON t) => m (Maybe (t a))
parseBody = parseBodyOfMax 262144

parseBody' :: (DeserializedVersion a, MonadSnap m) => m (Maybe a)
parseBody' = (runIdentity <$>) <$> parseBody

-- | Attempts to parse a JSON structure from a request body
--   Enforcing a maximum given request size (in bytes)
parseBodyOfMax :: (DeserializedVersion a, MonadSnap m, TraversableFromJSON t)
               => Int64
               -> m (Maybe (t a))
parseBodyOfMax n = do
  deserializer <- getDeserializerFromHeader
  val <- decode <$> readRequestBody n
  return (val >>= deserialize deserializer)

-- | Attempts to parse a JSON structure from a request body, short circuits
--   with a malformed request body error if the body is empty or can not be
--   parsed to the expected FromJSON instance.
--   Also enforces a maximum body size of 10KiB
requireBody :: (MonadSnap m, DeserializedVersion a, TraversableFromJSON t) => m (t a)
requireBody = parseBody >>= \case Nothing -> do
                                    modifyResponse $ setResponseStatus 400 "Malformed JSON body"
                                    getResponse >>= finishWith
                                  Just a -> return a

requireBody' :: (MonadSnap m, DeserializedVersion a) => m a
requireBody' = runIdentity <$> requireBody

-- | API layer
runAPI :: (MonadSnap m, SerializedVersion a, CatMaybes f, FunctorToJSON f) => m (f a) -> m ()
runAPI handler = do
  (serializer, mediaType) <- getSerializerFromHeader
  fa <- handler
  let mValue = serialize serializer fa
  case mValue of
    Nothing -> do
      modifyResponse $ setResponseStatus 406 "Version requested too old!"
      getResponse >>= finishWith
    Just value -> writeLBS $ encode value
  modifyResponse $ setHeader "Content-Type" (renderHeader mediaType)

-- | API Layer for handlers returning a single value
runAPI' :: (MonadSnap m, SerializedVersion a) => m a -> m ()
runAPI' = runAPI . (Identity <$>)
