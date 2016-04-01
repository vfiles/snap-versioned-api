{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Snap.API ( requireBody
                , requireBody'
                , runAPI
                , runAPI'
                , parseBody
                , parseBody'
                , EmptyObject(..)
                , EmptyArray(..)
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

-- | The default json serialization for `()` is `[]` which is not an
--   obvious response from an API that is returning an empty response.
--   So returning `()` from an api function is disallowed and the user
--   can instead use a newtype which chooses an explicit serialization.
--   `EmptyObject` and `EmptyArray` are provided.
type family APIable a where
  APIable () = False
  APIable a = True

newtype EmptyObject = EmptyObject ()

instance ToJSON EmptyObject where
  toJSON _ = object []

newtype EmptyArray = EmptyArray ()

instance ToJSON EmptyArray where
  toJSON _ = Array []

-- | API layer
runAPI :: (MonadSnap m, SerializedVersion a
          ,CatMaybes f, FunctorToJSON f
          ,APIable a ~ True) => m (f a) -> m ()
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
runAPI' :: (MonadSnap m, SerializedVersion a, APIable a ~ True) => m a -> m ()
runAPI' = runAPI . (Identity <$>)
