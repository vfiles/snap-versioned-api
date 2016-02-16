{-# LANGUAGE OverloadedStrings #-}

module Snap.API.Versions where

--------------------------------------------------------------------------------

import Snap.API.Utils

--------------------------------------------------------------------------------

import Control.Applicative
import Control.Monad hiding (sequence)
import Control.Monad.IO.Class

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Aeson.Versions

import Data.CaseInsensitive

import qualified Data.Map as M

import Network.HTTP.Media

import Text.Read

import Snap

noParams :: MediaType -> MediaType
noParams media = original (mainType media) // original (subType media)

getAcceptMedia :: MonadSnap m => Maybe BS.ByteString -> MediaType -> m MediaType
getAcceptMedia mAcceptHeader defaultMedia = case mAcceptHeader of
  Nothing -> return defaultMedia
  Just acceptHeader -> case parseAccept acceptHeader of
    Nothing -> badRequest "Bad Accept header"
    Just media -> if ("application" // "json") `matches` noParams media
                  then return $ case media /. "version" of
                                  Just v -> ("application" // "json") /: ("version", original v)
                                  Nothing -> defaultMedia
                  else notAcceptable "Unsupported media type"

getContentType :: MonadSnap m => Maybe BS.ByteString -> MediaType -> m MediaType
getContentType mContentTypeHeader defaultMedia = case mContentTypeHeader of
    Nothing -> return defaultMedia
    Just contentTypeHeader -> case parseAccept contentTypeHeader of
      Nothing -> badRequest "Bad Content-Type header"
      Just media -> if media `matches` ("application" // "json")
                    then return $ if media /? "version"
                                  then media
                                  else defaultMedia
                    else unsupportedMediaType "Unsupported media type"

-- | This method is a beast. It does a few things:
-- 1. Grabs the value of the "Accept" header (or picking a default)
-- 2. Parses it to a media type or sends a 400 if it's malformed
-- 3. Specializes it to a default version if none exist in the media type
-- 4. Parses the version text our sends a 406 if it's not recognized
-- 5. Get's the appropriate serializer or sends a 406 if there is none
-- 6. Returns the serializer and the specialized media type so that
--    the caller can set the content type (we don't want to set it here
--    in case the handler fails later on).
getSerializerFromHeader :: (MonadSnap m, SerializedVersion a) => m (Serializer a, MediaType)
getSerializerFromHeader = do
  let serializers' = serializers
      defaultVersion = fst $ M.findMax serializers'
  mAcceptHeader <- getHeader "Accept" <$> getRequest
  media <- getAcceptMedia mAcceptHeader $ "application" // "json" /: ("version", BS.pack $ show defaultVersion)
  let mVersionBS = original <$> media /. "version"
  version <- maybe (pure defaultVersion) parseVersion mVersionBS
  let mSerializer = M.lookup version serializers'
  case mSerializer of
    Just serializer -> return (serializer, media)
    Nothing -> notAcceptable "Unsupported version"

  where parseVersion v = case readMaybe (BS.unpack v) of
          Just x -> return x
          Nothing -> notAcceptable "Unsupported version"


getDeserializerFromHeader :: (MonadSnap m, DeserializedVersion a) =>
                             m (Deserializer a)
getDeserializerFromHeader = do
  let deserializers' = deserializers
      defaultVersion = fst $ M.findMax deserializers'
  mContentTypeHeader <- getHeader "Content-Type" <$> getRequest
  media <-  getContentType mContentTypeHeader $ "application" // "json" /: ("version", BS.pack $ show defaultVersion)
  let mVersionBS = original <$> media /. "version"
  version <- maybe (pure defaultVersion) parseVersion mVersionBS
  let mDeerializer = M.lookup version deserializers'
  case mDeerializer of
    Just deserializer -> return deserializer
    Nothing -> unsupportedMediaType "Unsupported version"

  where parseVersion v = case readMaybe (BS.unpack v) of
          Just x -> return x
          Nothing -> unsupportedMediaType "Unsupported version"
