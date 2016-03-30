{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Snap.API.Utils where

import Control.Applicative
import Control.Monad

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (readInt, readInteger, unpack)

import Data.Readable (Readable, fromBS, fromText)

import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding

import Data.Time.Clock (UTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

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

notFound' :: MonadSnap m => m a
notFound' = notFound "Not Found"

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

-- | Attempts to parse a parameter for a request, short-circuits with
--   missing parameter error response if unsuccessful, otherwise returns
--   the parsed parameter.
requireParam :: (Readable p, MonadSnap m)
             => ByteString       -- ^ The key name of the parameter to parse
             -> m p    -- ^ The parsed parameter returning in the Handler context
requireParam paramName = do
    mParamBS <- getParam paramName
    case mParamBS of
      Nothing -> badRequest $ "Missing parameter" <> paramName
      Just paramBS -> case fromBS paramBS of
         Nothing -> badRequest $ "Invalid parameter " <> paramName
         Just p  -> return p

optionalParam :: (Readable p, MonadSnap m)
              => ByteString
              -> m (Maybe p)
optionalParam paramName = (fromBS =<<) <$> getParam paramName

instance Readable UTCTime where
  fromText t = case parseTimeM True defaultTimeLocale format $ T.unpack t of
    Just x  -> return x
    Nothing -> mzero
    where format = "%Y-%m-%dT%H:%M:%S%Q%Z"
