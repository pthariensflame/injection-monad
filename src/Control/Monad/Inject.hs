{-# LANGUAGE Trustworthy #-}
module Control.Monad.Inject (Inj(..),
                             Inject,
                             runInject,
                             InjectT,
                             runInjectT,
                             liftInject) where
import Control.Monad.Inject.Internal
