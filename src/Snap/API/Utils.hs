{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Snap.API.Utils where

import Control.Applicative

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (readInt, readInteger, unpack)

import Data.Monoid

import Data.Text (Text)
import Data.Text.Encoding

import Snap

writeError :: (MonadSnap m)
           => Int -- ^ The HTTP status code to return
           -> ByteString  -- ^ The error message to return
           -> m a  -- ^ A handler returning the given HTTP status code and msg
writeError code msg = do
    modifyResponse $ setResponseStatus code msg
    finishWith =<< getResponse

badRequest :: MonadSnap m => ByteString -> m a
badRequest = writeError 400

unauthorized :: MonadSnap m => ByteString -> m a
unauthorized = writeError 401

forbidden :: MonadSnap m => ByteString -> m a
forbidden = writeError 403

notFound :: MonadSnap m => ByteString -> m a
notFound = writeError 404

notAcceptable :: MonadSnap m => ByteString -> m a
notAcceptable = writeError 406

conflict :: MonadSnap m => ByteString -> m a
conflict = writeError 409

gone :: MonadSnap m => ByteString -> m a
gone = writeError 410

unsupportedMediaType :: MonadSnap m => ByteString -> m a
unsupportedMediaType = writeError 415

------------
-- Params --
------------

class Param a where
    parseParam :: ByteString -> Maybe a

instance Param Integer where
    parseParam n = readInteger n >>= \case
                       (m,"") -> Just m
                       _ -> Nothing

instance Param Int where
   parseParam n = readInt n >>= \case
                      (m,"") -> Just m
                      _ -> Nothing

instance Param ByteString where
    parseParam = Just

instance Param Bool where
    parseParam "true" = Just True
    parseParam "false" = Just False
    parseParam _ = Nothing

instance Param [Char] where
    parseParam = Just . unpack

-- assumes utf8 encoding
instance Param Text where
    parseParam = decodeUtf8' |> \case
                    Left _ -> Nothing
                    Right x -> Just x
        where x |> y = y . x



-- | Attempts to parse a parameter for a request, short-circuits with
--   missing parameter error response if unsuccessful, otherwise returns
--   the parsed parameter.
requireParam :: (Param p, MonadSnap m)
             => ByteString       -- ^ The key name of the parameter to parse
             -> m p    -- ^ The parsed parameter returning in the Handler context
requireParam paramName = do
    mParamBS <- getParam paramName
    case mParamBS of
      Nothing -> badRequest $ "Missing parameter" <> paramName
      Just paramBS -> case parseParam paramBS of
         Nothing -> badRequest $ "Invalid parameter " <> paramName
         Just p  -> return p
