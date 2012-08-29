{-# LANGUAGE DataKinds, KindSignatures, GADTs, ScopedTypeVariables, ConstraintKinds, TypeFamilies, FlexibleContexts #-}
module Control.Monad.Inject.Internal where
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Base

-- | A kind for the specification of injected or injectable values
data Inj (star :: *) -- ^ kind @*@
  = G [Inj star starToConstraint] -- ^ group of injections
  | V star -- ^ single value
  | L star -- ^ list of values

data InjectSig :: [Inj *] -> * -> * where
  Lift :: forall (x :: *). x -> InjectSig '[] x
  Pad :: forall (i :: Inj *) (is :: [Inj *]) (x :: *). InjectSig is x -> InjectSig (i ': is) x
  InjectG :: forall (l :: [Inj *]) (is :: [Inj *]) (x :: *). InjectSig l (InjectSig is x) -> InjectSig ((G l) ': is) x
  InjectV :: forall (t :: *) (is :: [Inj *]) (x :: *). (t -> InjectSig is x) -> InjectSig ((V t) ': is) x
  InjectL :: forall (t :: *) (is :: [Inj *]) (x :: *). ([t] -> InjectSig is x) -> InjectSig ((L t) ': is) x

mapSig :: forall (l :: [Inj *]) (x :: *). InjectSig

newtype InjectT (l :: [Inj *]) (m :: * -> *) (a :: *) = InjectT (InjectSig l (m a))

type Inject (l :: [Inj *]) (a :: *) :: (* -> *) = InjectT l Identity

class (Monad m) => MonadInjectCore (m :: * -> *) where
  type InjList m :: [Inj *]

instance (Monad m) => MonadInjectCore (InjectT l m) where
  type InjList (InjectT l m) = l

class (MonadInjectCore m, MonadInjectCore (PrevMonad m), InjList m ~ ((CurrInj m) ': (InjList (PrevMonad m)))) => MonadInjectChain (m :: * -> *) where
  type PrevMonad m :: * -> *
  type CurrInj m :: Inj *                                                                      

instance (Monad m) => MonadInjectChain (InjectT (i ': is) m) where
  type PrevMonad (InjectT (i ': is) m) = InjectT is m
  type CurrInj (InjectT (i ': is) m) = (i ': is)

class (MonadInjectChain m) => MonadInject (m :: * -> *) where
  type InjectionPoint
