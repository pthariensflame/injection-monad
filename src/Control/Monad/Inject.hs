{-# LANGUAGE Safe #-}
module Control.Monad.Inject (Inj(..),
                             MonadInject(..),
                             Inject,
                             runInject,
                             InjectT,
                             runInjectT) where
import Control.Monad.Inject.Internal
