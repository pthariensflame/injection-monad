{-# LANGUAGE DataKinds, KindSignatures, GADTs, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies, TypeFamilies, TypeOperators, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Control.Monad.Inject.Internal where
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Monad.Base
import Control.Applicative

-- | A kind for the specification of injected or injectable values.  @k@ will determine the kind of each individual injection.
data Inj (k :: *)
  = V k -- ^ single value
  | L k -- ^ list of values m' -> m is
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

newtype InjectT (is :: [Inj *]) (m :: * -> *) (a :: *) = InjectT { getInjectT :: InjectSig is (m a) }

runInjectT :: InjectT '[] m a -> m a
runInjectT (InjectT (Lift x)) = x

type Inject (is :: [Inj *]) = InjectT is Identity

runInject :: Inject '[] a -> a
runInject = runIdentity . runInjectT

instance (Functor m) => Functor (InjectT is m) where
  fmap f (InjectT m) = InjectT . flip mapSig m $ fmap f

instance (Applicative m) => Applicative (InjectT '[] m) where
  pure = InjectT . Lift . pure
  (InjectT (Lift q)) <*> (InjectT (Lift m)) = InjectT . Lift $ q <*> m

instance (Functor m, Applicative (InjectT is m)) => Applicative (InjectT ((V i) ': is) m) where
  pure = InjectT . InjectV . const . getInjectT . pure
  (InjectT (InjectV qf)) <*> (InjectT (InjectV mf)) = InjectT . InjectV $ \x -> getInjectT $ InjectT (qf x) <*> InjectT (mf x)

instance (Functor m, Applicative (InjectT is m)) => Applicative (InjectT ((L i) ': is) m) where
  pure = InjectT . InjectL . const . getInjectT . pure
  (InjectT (InjectL qf)) <*> (InjectT (InjectL mf)) = InjectT . InjectL $ \xs -> getInjectT $ InjectT (qf xs) <*> InjectT (mf xs)

instance (Functor m, Applicative (InjectT is' (InjectT is m))) => Applicative (InjectT ((G is') ': is) m) where
  pure = InjectT . InjectG . mapSig getInjectT . getInjectT . pure
  (InjectT (InjectG q)) <*> (InjectT (InjectG m)) = InjectT . InjectG . mapSig getInjectT . getInjectT $ InjectT (mapSig InjectT q) <*> InjectT (mapSig InjectT m)

instance (Monad m) => Monad (InjectT '[] m) where
  return = InjectT . Lift . return
  (InjectT (Lift x)) >>= f = InjectT . Lift $ do v <- x
                                                 let (InjectT (Lift x')) = f v
                                                 x'

instance (Monad (InjectT is m)) => Monad (InjectT ((V i) ': is) m) where
  return = InjectT . InjectV . const . getInjectT . return
  (InjectT (InjectV mf)) >>= f = InjectT . InjectV $ \x -> getInjectT $ do v <- InjectT $ mf x
                                                                           case f v of (InjectT (InjectV mf')) -> InjectT $ mf' x

instance (Monad (InjectT is m)) => Monad (InjectT ((L i) ': is) m) where
  return = InjectT . InjectL . const . getInjectT . return
  (InjectT (InjectL mf)) >>= f = InjectT . InjectL $ \xs -> getInjectT $ do v <- InjectT $ mf xs
                                                                            case f v of (InjectT (InjectL mf')) -> InjectT $ mf' xs

instance (Monad (InjectT is' (InjectT is m))) => Monad (InjectT ((G is') ': is) m) where
  return = InjectT . InjectG . mapSig getInjectT . getInjectT . return
  (InjectT (InjectG m)) >>= f = InjectT . InjectG . mapSig getInjectT . getInjectT $ do v <- InjectT $ mapSig InjectT m
                                                                                        case f v of (InjectT (InjectG m')) -> InjectT $ mapSig InjectT m'

instance (MonadTrans (InjectT

class LiftInject is is' where
  liftInject :: InjectT is m a -> InjectT is' m a

instance LiftInject is is where
  liftInject = id

instance (LiftInject is is') => LiftInject is ((V i) ': is') where
  liftInject = InjectT . InjectV . const . getInjectT . liftInject

instance (LiftInject is is') => LiftInject is ((L i) ': is') where
  liftInject = InjectT . InjectL . const . getInjectT . liftInject

instance (LiftInject is is', LiftInject '[] is'') => LiftInject is ((G is'') ': is') where
  liftInject = InjectT . InjectG . getInjectT . liftInject . InjectT . Lift . getInjectT . liftInject
