{-# LANGUAGE DataKinds, KindSignatures, GADTs, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, Safe #-}
module Control.Monad.Inject.Internal where
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Base
import Control.Applicative

-- | A kind for the specification of injected or injectable values
data Inj (k :: *) -- ^ the kind being injected
  = V k -- ^ single value
  | L k -- ^ list of values
  | G [Inj k] -- ^ group of injections

data InjectSig :: [Inj *] -> * -> * where
  Lift :: forall (x :: *). x -> InjectSig '[] x
  InjectV :: forall (t :: *) (is :: [Inj *]) (x :: *). (t -> InjectSig is x) -> InjectSig ((V t) ': is) x
  InjectL :: forall (t :: *) (is :: [Inj *]) (x :: *). ([t] -> InjectSig is x) -> InjectSig ((L t) ': is) x
  InjectG :: forall (is' :: [Inj *]) (is :: [Inj *]) (x :: *). InjectSig is' (InjectSig is x) -> InjectSig ((G is') ': is) x

mapSig :: forall (is :: [Inj *]) (x :: *) (x' :: *). (x -> x') -> InjectSig is x -> InjectSig is x'
mapSig f (Lift x) = Lift $ f x
mapSig f (InjectV g) = InjectV $ \x -> mapSig f $ g x
mapSig f (InjectL g) = InjectL $ \xs -> mapSig f $ g xs
mapSig f (InjectG m) = InjectG . flip mapSig m $ mapSig f

newtype InjectT (is :: [Inj *]) (m :: * -> *) (a :: *) = InjectT (InjectSig is (m a))

type Inject (is :: [Inj *]) :: (* -> *) = InjectT is Identity

instance (Functor m) => Functor (InjectT is m) where
  fmap f (InjectT m) = InjectT . flip mapSig m $ fmap f

instance (Applicative m) => Applicative (InjectT '[] m) where
  pure = InjectT . Lift . pure
  (InjectT (Lift q)) <*> (InjectT (Lift m)) = InjectT . Lift $ q <*> m

instance (Applicative (InjectT is m)) => Applicative (InjectT ((V i) ': is) m) where
  pure = InjectT . InjectV . const . pure
  (InjectT (InjectV qf)) <*> (InjectT (InjectV mf)) = InjectT . InjectV $ \x -> qf x <*> mf x

instance (Applicative (InjectT is m)) => Applicative (InjectT ((L i) ': is) m) where
  pure = InjectT . InjectL . const . pure
  (InjectT (InjectL qf)) <*> (InjectT (InjectL mf)) = InjectT . InjectL $ \xs -> qf xs <*> mf xs

instance (Applicative (InjectT is' (InjectT is m))) => Applicative (InjectT ((G is') ': is) m) where
  pure = InjectT . InjectG . pure
  (InjectT (InjectG q)) <*> (InjectT (InjectG m)) = InjectT . InjectG $ q <*> m

