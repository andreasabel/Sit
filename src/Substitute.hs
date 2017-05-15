{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | Substitution and weak head evaluation

module Substitute where

import Internal

import Impossible
#include "undefined.h"

-- | Substitutions are lists of terms.

type Subst = [Term]

-- | Weakening substitution @Γ.Δ ⊢ wkS |Δ| : Γ@

wkS :: Int -> Subst
wkS n = map (Var . Index) [n,n+1..]

-- | Identity substitution @Γ ⊢ idS : Γ@.

idS :: Subst
idS = wkS 0

-- | Composing substitution
--   @
--      Γ₁ ⊢ τ : Γ₂    Γ₂ ⊢ σ : Γ₃
--      -------------------------
--      Γ₁ ⊢ compS τ σ : Γ₃
--   @

compS :: Subst -> Subst -> Subst
compS = subst

-- | Extending a substitution
--   @
--      Γ ⊢ σ : Δ    Δ ⊢ T    Γ ⊢ t : Tσ
--      --------------------------------
--      Γ ⊢ consS t σ : Δ.T
--   @

consS :: Term -> Subst -> Subst
consS = (:)

-- | Lifting a substitution under a binder.
--   @
--      Γ ⊢ σ : Δ      Δ ⊢ T
--      --------------------
--      Γ.Tσ ⊢ liftS σ : Δ.T
--   @

liftS :: Subst -> Subst
liftS s = consS (Var 0) $ weakS s

-- | Weakening a substitution.
--
--   @
--     Γ ⊢ σ : Δ    Γ ⊢ T
--     ------------------
--     Γ.T ⊢ weakS σ : Δ
--   @

weakS :: Subst -> Subst
weakS = compS (wkS 1)

-- | Looking up an entry in a substitution.

lookupS :: Subst -> Index -> Term
lookupS s i =  s !! dbIndex i

-- | Substitution for various syntactic categories.

class Substitute a where
  subst :: Subst -> a -> a

instance Substitute a => Substitute [a] where
  subst s = map (subst s)

instance Substitute a => Substitute (Dom a) where
  subst s = fmap (subst s)

instance Substitute a => Substitute (Arg a) where
  subst s = fmap (subst s)

instance Substitute a => Substitute (Elim' a) where
  subst s = fmap (subst s)

instance Substitute Term where
  subst s = \case
    Type l  -> Type $ subst s l
    Size    -> Size
    Nat a   -> Nat $ subst s a
    Zero a  -> Zero $ subst s a
    Suc a t -> Suc (subst s a) $ subst s t
    Infty   -> Infty
    Pi u t  -> Pi (subst s u) $ subst s t
    Lam r t -> Lam r $ subst s t
    Var i   -> lookupS s i
    Def f   -> Def f
    App t u -> App (subst s t) (subst s u)

instance Substitute (Abs Term) where
  subst s (Abs   x t) = Abs   x $ subst (liftS s) t
  subst s (NoAbs x t) = NoAbs x $ subst s t

raise :: Substitute a => Int -> a -> a
raise n = subst (wkS n)

{- TODO!?

-- | Application

class Substitute a => Apply a where
  applyE     :: a -> Elims -> a
  applyE t [] = t
  applyE t es = substApply t idS es

  substApply :: a -> Subst -> Elims -> a
  substApply t s es = subst s t `applyE` es

instance Apply a => Apply [a] where
  applyE ts es       = map (`applyE` es) ts
  substApply ts s es = map (\ t -> substApply t s es) ts

instance Apply a => Apply (Dom a) where
  applyE ts es       = fmap (`applyE` es) ts
  substApply ts s es = fmap (\ t -> substApply t s es) ts

instance Apply a => Apply (Arg a) where
  applyE ts es       = fmap (`applyE` es) ts
  substApply ts s es = fmap (\ t -> substApply t s es) ts

instance Apply a => Apply (Elim' a) where
  applyE ts es       = fmap (`applyE` es) ts
  substApply ts s es = fmap (\ t -> substApply t s es) ts

instance Apply Term where
  substApply t s es = case t of
    -- Eliminations
    Var i   -> lookupS s i `applyE` es
    Def f   -> foldl App (Def f) es
    App t u -> substApply t s $ Apply (subst s u) : es
    -- Eliminateables

    -- Types & non-eliminateables
    Type l
      | null es   -> Type $ subst s l
      | otherwise -> __IMPOSSIBLE__
    Size
      | null es   -> Size
      | otherwise -> __IMPOSSIBLE__
    Nat a
      | null es   -> Nat $ subst s a
      | otherwise -> __IMPOSSIBLE__
    Zero
      | null es   -> Zero
      | otherwise -> __IMPOSSIBLE__
    Suc t
      | null es   -> Suc $ subst s t
      | otherwise -> __IMPOSSIBLE__
    Infty
      | null es   -> Infty
      | otherwise -> __IMPOSSIBLE__
    Pi u t
      | null es   -> Pi (subst s u) $ subst s t
      | otherwise -> __IMPOSSIBLE__

instance Apply (Abs Term) where

  substApply (Abs x t) s = \case
    (Apply (Arg _ u) : es) -> substApply t (consS u s) es
    _ -> __IMPOSSIBLE__

  substApply (NoAbs x t) s = \case
    (Apply _ : es) -> substApply t s es
    _ -> __IMPOSSIBLE__


-}



-- | Construct the type of the functional for fix.
--
--   @fixType t = .(i : Size) -> ((x : Nat i) -> T i x) -> (x : Nat (i + 1)) -> T (i + 1) x

fixType :: Term -> Term
fixType t =
  Pi (Dom Irrelevant Size) $ Abs "i" $
    Pi (Dom Relevant $ f $ Var 0) $ NoAbs "_" $
      f $ sSuc $ Var 0
  where
  f a = Pi (Dom Relevant (Nat a)) $ Abs "x" $
          raise 2 t
            `App` Apply (Arg ShapeIrr $ raise 1 a)
            `App` Apply (Arg Relevant $ Var 0)
