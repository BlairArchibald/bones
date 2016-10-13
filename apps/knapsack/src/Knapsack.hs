{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Knapsack
(
    skeletonSafe
  , skeletonBroadcast
  , skeletonSequential
  , sequentialInlined
  , declareStatic
  , Solution(..)
) where

import Control.Parallel.HdpH hiding (declareStatic)

import Bones.Skeletons.BranchAndBound.HdpH.Types ( BAndBFunctions(BAndBFunctions)
                                                 , BAndBFunctionsL(BAndBFunctionsL)
                                                 , PruneType(..), ToCFns(..))

import Bones.Skeletons.BranchAndBound.HdpH.GlobalRegistry
import qualified Bones.Skeletons.BranchAndBound.HdpH.Safe as Safe
import qualified Bones.Skeletons.BranchAndBound.HdpH.Broadcast as Broadcast
import qualified Bones.Skeletons.BranchAndBound.HdpH.Sequential as Sequential

import Control.DeepSeq (NFData)
import Control.Monad (when, foldM)

import GHC.Generics (Generic)

import Data.Serialize (Serialize)
import Data.IORef

import Data.List (delete)

import Data.Array.Unboxed

data Solution = Solution !Int !Int ![Item] !Int !Int deriving (Generic, Show)
type Item = Int

instance Serialize Solution where
instance NFData Solution where

skeletonSafe :: [(Int, Int, Int)] -> Int -> Int -> Bool -> Par Solution
skeletonSafe items capacity depth diversify =
  Safe.search
    diversify
    depth
    (Solution (length items) capacity [] 0 0, 0, map (\(a,b,c) -> a) items)
    (toClosure (BAndBFunctions
      $(mkClosure [| orderedGenerator |])
      $(mkClosure [| pruningPredicate|])
      $(mkClosure [| strengthen |])))
    (toClosure (ToCFns
      $(mkClosure [| toClosureSolution |])
      $(mkClosure [| toClosureInt |])
      $(mkClosure [| toClosureItemList |])
      $(mkClosure [| toClosureKPNode |])))

skeletonBroadcast :: [(Int, Int, Int)] -> Int -> Int -> Bool -> Par Solution
skeletonBroadcast items capacity depth diversify =
  Broadcast.search
    depth
    (Solution (length items) capacity [] 0 0, 0, map (\(a,b,c) -> a) items)
    (toClosure (BAndBFunctions
      $(mkClosure [| orderedGenerator |])
      $(mkClosure [| pruningPredicate|])
      $(mkClosure [| strengthen |])))
    (toClosure (ToCFns
      $(mkClosure [| toClosureSolution |])
      $(mkClosure [| toClosureInt |])
      $(mkClosure [| toClosureItemList |])
      $(mkClosure [| toClosureKPNode |])))

skeletonSequential :: [(Int, Int, Int)] -> Int -> Par Solution
skeletonSequential items capacity =
  Sequential.search
    (Solution (length items) capacity [] 0 0, 0, map (\(a,b,c) -> a) items)
    (BAndBFunctionsL orderedGenerator pruningPredicate strengthen)

-- --------------------------------------------------------------------------------
-- -- An inlined version of the sequential skeleton
-- --------------------------------------------------------------------------------
sequentialInlined :: [(Int, Int, Int)] -> Int -> Par Solution
sequentialInlined items capacity =
  seqSearch (Solution (length items) capacity [] 0 0) (map (\(a,b,c) -> a) items) 0

-- Assumes any global space state is already initialised
seqSearch :: Solution -> [Item] -> Int -> Par a
seqSearch ssol sspace sbnd = do
  io $ addToRegistry solutionKey (ssol, sbnd)
  io $ addToRegistry boundKey sbnd
  expand ssol sspace
  io $ fst <$> readFromRegistry solutionKey

expand :: Solution -> [Item] -> Par ()
expand = go1
  where
    go1 s r = generateChoices s r >>= go s r -- \cs -> case cs of [] -> io (putStrLn "Close") >> go s r []
                                                        --xs -> go s r xs

    go _ _ [] = return ()

    go sol remaining (c:cs) = do
      bnd <- io $ readFromRegistry boundKey

      sp <- shouldPrune c bnd sol remaining
      case sp of
        Prune      -> do
          remaining'' <- removeChoice c remaining
          go sol remaining'' cs

        PruneLevel -> do
          -- io . putStrLn $ "Prune"
          return ()

        NoPrune    -> do
          (newSol, newBnd, remaining') <- step c sol remaining

          when (shouldUpdateBound newBnd bnd) $
              updateLocalBoundAndSol newSol newBnd

          go1 newSol remaining'

          remaining'' <- removeChoice c remaining
          go sol remaining'' cs

-- TODO: Technically we don't need atomic modify when we are sequential but this
-- keeps us closer to the parallel version.
updateLocalBoundAndSol :: Solution -> Int -> Par ()
updateLocalBoundAndSol sol bnd = do
  -- Bnd
  ref <- io $ getRefFromRegistry boundKey
  io $ atomicModifyIORef' ref $ \b ->
    if shouldUpdateBound bnd b then (bnd, ()) else (b, ())

  -- Sol
  ref <- io $ getRefFromRegistry solutionKey
  io $ atomicModifyIORef' ref $ \prev@(_,b) ->
        if shouldUpdateBound bnd b
            then ((sol, bnd), True)
            else (prev, False)

  return ()

--------------------------------------------------------------------------------
-- Skeleton Functions
--------------------------------------------------------------------------------

--  generateChoices :: Closure (Closure a -> Closure s -> Par [Closure c])
--  shouldPrune     :: Closure (Closure c -> Closure a -> Closure b -> Bool)
--  updateBound     :: Closure (Closure b -> Closure b -> Bool)
--  step            :: Closure (Closure c -> Closure a -> Closure s
--                      -> Par (Closure a, Closure b, Closure s))
--  removeChoice    :: Closure (Closure c -> Closure s-> Closure s)

-- Potential choices is simply the list of un-chosen items

generateChoices :: Solution -> [Item] -> Par [Item]
generateChoices (Solution _ cap _ _ curWeight) remaining = do
  (_ , weights) <- io (getGlobalSearchSpace :: IO (Array Int Int, Array Int Int))
  return $ filter (\i -> curWeight + fromIntegral (weights ! i) <= cap) remaining

-- Calculate the bounds function
shouldPrune :: Item
            -> Int
            -> Solution
            -> [Item]
            -> Par PruneType
shouldPrune i bnd (Solution mix cap _ p w) _ = do
  (profits, weights) <- io getGlobalSearchSpace
  let ub' = ub profits weights (p + (profits ! i)) (w + (weights ! i)) (i + 1)
  if fromIntegral bnd >= ub' then
    return PruneLevel
  else
    return NoPrune

  where
    -- TODO: Scope capturing function
    ub :: Array Int Int -> Array Int Int -> Int -> Int -> Item -> Double
    ub profits weights p w i
      | i > mix = fromIntegral p
      | cap - (w + (weights ! i)) >= 0 = ub profits weights (p + (profits ! i)) (w + (weights ! i)) (i + 1)
      | otherwise = fromIntegral p + (fromIntegral (cap - w) * divd (profits ! i) (weights ! i))

    divd :: Int -> Int -> Double
    divd a b = fromIntegral a / fromIntegral b

shouldUpdateBound :: Int -> Int -> Bool
shouldUpdateBound x y = x > y

step :: Item -> Solution -> [Item] -> Par (Solution, Int, [Item])
step i (Solution mix cap is p w) r = do
  (profits, weights) <- io (getGlobalSearchSpace :: IO (Array Int Int, Array Int Int))
  rm <- removeChoice i r

  return (Solution mix cap (i:is) (p + (profits ! i)) (w + (weights ! i)), p + (profits ! i), rm)

removeChoice :: Item -> [Item] -> Par [Item]
removeChoice i its = return $ delete i its

--------------------------------------------------------------------------------
-- Skeleton Functions
--------------------------------------------------------------------------------

type KPNode = (Solution, Int, [Int])

orderedGenerator :: KPNode -> Par [Par KPNode]
orderedGenerator ((Solution mix cap is solP solW), bnd, remaining) = do
  (profits, weights) <- io (getGlobalSearchSpace :: IO (Array Int Int, Array Int Int))
  let items = filter (\i -> solW + fromIntegral (weights ! i) <= cap) remaining

  return $ map (\i -> return (Solution mix cap (i:is) (solP + (profits ! i)) (solW + (weights ! i)),
                              solP + (profits ! i),
                              delete i remaining)) items

pruningPredicate :: KPNode -> Int -> Par PruneType
pruningPredicate ((Solution mix cap (i:is) solP solW), _, _) bnd = do
  (profits, weights) <- io getGlobalSearchSpace
  -- Profits/weights already in solution for i so don't add them again does this work for i = 0?

  let ub' = ub profits weights solP solW (i + 1)
  if fromIntegral bnd >= ub' then
    return PruneLevel
  else
    return NoPrune

  where
    -- TODO: Scope capturing function
    ub :: Array Int Int -> Array Int Int -> Int -> Int -> Item -> Double
    ub profits weights p w i
      | i > mix = fromIntegral p
      | cap - (w + (weights ! i)) >= 0 = ub profits weights (p + (profits ! i)) (w + (weights ! i)) (i + 1)
      | otherwise = fromIntegral p + (fromIntegral (cap - w) * divd (profits ! i) (weights ! i))

    divd :: Int -> Int -> Double
    divd a b = fromIntegral a / fromIntegral b

strengthen :: KPNode -> Int -> Bool
strengthen (_, lbnd, _) gbnd = lbnd > gbnd

--------------------------------------------------------------------------------
-- Closure Instances
--------------------------------------------------------------------------------
instance ToClosure (BAndBFunctions Solution Int [Item]) where
  locToClosure = $(here)

instance ToClosure (ToCFns Solution Int [Item]) where
  locToClosure = $(here)

--------------------------------------------------------------------------------
-- Explicit ToClousre Instances (needed for performance)
--------------------------------------------------------------------------------
toClosureItem :: Item -> Closure Item
toClosureItem x = $(mkClosure [| toClosureItem_abs x |])

toClosureItem_abs :: Item -> Thunk Item
toClosureItem_abs x = Thunk x

toClosureItemList :: [Item] -> Closure [Item]
toClosureItemList x = $(mkClosure [| toClosureItemList_abs x |])

toClosureItemList_abs :: [Item] -> Thunk [Item]
toClosureItemList_abs x = Thunk x

toClosureSolution :: Solution -> Closure Solution
toClosureSolution x = $(mkClosure [| toClosureSolution_abs x |])

toClosureSolution_abs :: Solution -> Thunk Solution
toClosureSolution_abs x = Thunk x

toClosureInteger :: Integer -> Closure Integer
toClosureInteger x = $(mkClosure [| toClosureInteger_abs x |])

toClosureInteger_abs :: Integer -> Thunk Integer
toClosureInteger_abs x = Thunk x

toClosureInt :: Int -> Closure Int
toClosureInt x = $(mkClosure [| toClosureInt_abs x |])

toClosureInt_abs :: Int -> Thunk Int
toClosureInt_abs x = Thunk x

toClosureKPNode :: KPNode -> Closure KPNode
toClosureKPNode x = $(mkClosure [| toClosureKPNode_abs x |])

toClosureKPNode_abs :: KPNode -> Thunk KPNode
toClosureKPNode_abs x = Thunk x


$(return [])
declareStatic :: StaticDecl
declareStatic = mconcat
  [
    declare (staticToClosure :: StaticToClosure (BAndBFunctions Solution Int [Item]))
  , declare (staticToClosure :: StaticToClosure (ToCFns Solution Int [Item]))

  -- B&B Functions
  , declare $(static 'orderedGenerator)
  , declare $(static 'pruningPredicate)
  , declare $(static 'strengthen)

  -- Explicit toClosure
  , declare $(static 'toClosureInteger)
  , declare $(static 'toClosureInteger_abs)
  , declare $(static 'toClosureInt)
  , declare $(static 'toClosureInt_abs)
  , declare $(static 'toClosureItemList)
  , declare $(static 'toClosureItemList_abs)
  , declare $(static 'toClosureSolution)
  , declare $(static 'toClosureSolution_abs)
  , declare $(static 'toClosureKPNode)
  , declare $(static 'toClosureKPNode_abs)

  , Safe.declareStatic
  ]