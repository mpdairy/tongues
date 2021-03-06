module Tongues.Prelude
  ( module Exports
  , liftEitherIO
  , liftMaybeIO
  , liftEitherM
  , liftMaybe
  , liftMaybeM
  , liftMaybeTIO
  ) where

import           Prelude         as Exports        ( String
                                                   , head
                                                   , (!!)
                                                   )

import qualified Data.Text.Lazy as L (Text)

import System.Random as Exports (randomIO, randomRIO)
import Data.Data as Exports
import Data.UUID as Exports (UUID)
--import Data.Typeable as Exports
import           Control.Lens    as Exports        ( (%~)
                                                   , (.~)
                                                   , (.=)
                                                   , (%=)
                                                   , (?~)
                                                   , (^.)
                                                   , (^?)
                                                   , Iso'
                                                   , Lens'
                                                   , iso
                                                   , lens
                                                   , makeClassy
                                                   , makeClassyPrisms
                                                   , makeFields
                                                   , makeFieldsNoPrefix
                                                   , makeLenses
                                                   , makePrisms
                                                   , use
                                                   , view
                                                   )
import Data.String.Conversions as Exports ( cs )
import           Data.Maybe      as Exports        ( fromJust )
import           Protolude       as Exports hiding ( head, Infix, Prefix, Fixity, Word )
import Control.Monad.Trans.Maybe as Exports (runMaybeT, MaybeT)

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe e Nothing = throwError e
liftMaybe _ (Just x) = return x

liftMaybeM :: Monad m => e -> m (Maybe a) -> ExceptT e m a
liftMaybeM e m = ExceptT $ m >>= return . maybe (Left e) Right

--sort of redundant, actually...
liftEitherM :: m (Either e a) -> ExceptT e m a
liftEitherM = ExceptT

liftEither :: (MonadError e m) => Either e a -> m a
liftEither (Left e) = throwError e
liftEither (Right x) = return x

liftEitherIO :: (MonadError e m, MonadIO m) => IO (Either e a) -> m a
liftEitherIO m = liftIO m >>= liftEither

liftMaybeIO :: (MonadError e m, MonadIO m) => e -> IO (Maybe a) -> m a
liftMaybeIO e m = liftIO m >>= liftEither . maybe (Left e) Right

liftMaybeTIO :: MonadIO m => IO (Maybe a) -> MaybeT m a
liftMaybeTIO m = liftIO m >>= maybe mzero return
