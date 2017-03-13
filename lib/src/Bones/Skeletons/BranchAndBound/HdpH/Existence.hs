{-# LANGUAGE TemplateHaskell #-}

module Bones.Skeletons.BranchAndBound.HdpH.Existence
  (
    declareStatic
  , exists
  ) where

import           Control.Parallel.HdpH (Closure, Node, Par, StaticDecl, Thunk (Thunk), IVar, GIVar,
                                        allNodes, declare, get, io, mkClosure, myNode,
                                        fork, one, spawn, new, glob, unClosure, toClosure,
                                        static, put, spawnAt, rput)

import           Control.Monad         (when, unless)

import           Data.IORef

-- import           Bones.Skeletons.BranchAndBound.HdpH.Common hiding (declareStatic)
import qualified Bones.Skeletons.BranchAndBound.HdpH.Common as Common
import           Bones.Skeletons.BranchAndBound.HdpH.Types hiding (declareStatic)
import qualified Bones.Skeletons.BranchAndBound.HdpH.Types as Types (declareStatic)
import           Bones.Skeletons.BranchAndBound.HdpH.GlobalRegistry
import           Bones.Skeletons.BranchAndBound.HdpH.Util

--------------------------------------------------------------------------------
--- Skeleton Functionality
--------------------------------------------------------------------------------

-- | Perform a backtracking search using a skeleton with distributed work
-- spawning. Makes no guarantees on task ordering.
exists ::
          Bool                              -- ^ Enable pruneLevel optimisation
       -> Int                               -- ^ Depth in the tree to spawn to. 0 implies top level tasks.
       -> BBNode a b s                      -- ^ Root Node
       -> Closure (BAndBFunctions g a b s)  -- ^ Higher order B&B functions
       -> Closure (ToCFns a b s)            -- ^ Explicit toClosure instances
       -> Par a                             -- ^ The solution, if found
exists pl depth root fs' toC = do
  master <- myNode
  nodes  <- allNodes

  -- Configuration initial state
  initLocalRegistries nodes (bound root) toC
  Common.initSolutionOnMaster root toC

  -- Early solution signal
  found    <- new
  foundRef <- glob found

  -- Gen top level (tasks then recursively create each other)
  space <- io getGlobalSearchSpace
  ts <- orderedGenerator (unClosure fs') space root >>= sequence
  let tasks = map (createChildren foundRef depth master) ts

  futures <- mapM (spawn one) tasks

  fork $ checkChildren futures found

  get found
  io $ unClosure . fst <$> readFromRegistry solutionKey
    where
      createChildren done d m n =
          let n' = toCnode (unClosure toC) n
          in $(mkClosure [| branchAndBoundChild (done, d, m, pl, n', fs', toC) |])

      -- If call the child tasks complete then we didn't find the thing we were looking for
      checkChildren :: [IVar (Closure ())] -> IVar (Closure ()) -> Par ()
      checkChildren children found = mapM_ get children >> put found toClosureUnit

branchAndBoundChild ::
    (
      GIVar (Closure ())
    , Int
    , Node
    , Bool
    , Closure (BBNode a b s)
    , Closure (BAndBFunctions g a b s)
    , Closure (ToCFns a b s))
    -> Thunk (Par (Closure ()))
branchAndBoundChild (done, spawnDepth, parent, pl, n, fs', toC) =
  Thunk $ do
    let fs = unClosure fs'
    gbnd <- io $ readFromRegistry boundKey
    space <- io getGlobalSearchSpace

    -- TODO: Enable early exit in this case
    lbnd <- pruningHeuristic fs space (unClosure n)
    case compareB fs lbnd gbnd  of
      GT -> branchAndBoundExpand done pl spawnDepth parent n fs' toC >> return toClosureUnit
      _  -> return toClosureUnit

branchAndBoundExpand ::
       GIVar (Closure ())
    -> Bool
    -> Int
    -> Node
    -> Closure (BBNode a b s)
    -> Closure (BAndBFunctions g a b s)
    -> Closure (ToCFns a b s)
    -> Par ()
branchAndBoundExpand done pl depth parent n fs toC
  | depth == 0 = let fsl  = unClosure fs
                     toCl = unClosure toC
                 in do
                    space <- io getGlobalSearchSpace
                    expandSequential pl parent done (unClosure n) space fs fsl toCl
  | otherwise  = do
        -- Duplication from the main search function, extract
        let fs' = unClosure fs
        space <- io getGlobalSearchSpace
        ns <- orderedGenerator fs' space (unClosure n) >>= sequence

        let tasks = map (createChildren (depth - 1) parent) ns

        mapM (spawn one) tasks >>= mapM_ get

  where
      createChildren d m n =
          let n' = toCnode (unClosure toC) n
          in $(mkClosure [| branchAndBoundChild (done, d, m, pl, n', fs, toC) |])

expandSequential ::
       Bool
       -- ^ PruneLevel Optimisation Enabled?
    -> Node
       -- ^ Master node (for transferring new bounds)
    -> GIVar (Closure ())
       -- ^ IVar to toggle if we find a solution
    -> BBNode a b s
       -- ^ Root node for this (sub-tree) search
    -> g
       -- ^ Global search space
    -> Closure (BAndBFunctions g a b s)
       -- ^ Closured function variants
    -> BAndBFunctions g a b s
       -- ^ Pre-unclosured local function variants
    -> ToCFns a b s
       -- ^ Explicit toClosure instances
    -> Par ()
       -- ^ Side-effect only function
-- Be careful of n aliasing
expandSequential pl parent done n' space fs fsl toC = expand n'
    where
      expand n = orderedGenerator fsl space n >>= go

      go [] = return ()

      go (n:ns) = do
        gbnd <- io $ readFromRegistry boundKey

        -- Manually force evaluation (used to avoid fully evaluating the node list
        -- if it's not needed)
        node@(sol, bndl, _) <- n

        lbnd <- pruningHeuristic fsl space node
        case compareB fsl lbnd gbnd of
          GT -> do
            when (compareB fsl (bound node) gbnd == EQ) $ do
                let cSol = toCa toC sol
                    cBnd = toCb toC bndl
                notifyParentOfSolution parent done cSol

            expand node >> go ns
          _ -> unless pl $ go ns

notifyParentOfSolution :: Node
                       -- ^ Master node
                       -> GIVar (Closure ())
                       -- ^ Early termination flag
                       -> Closure a
                       -- ^ New updated solution
                       -> Par ()
                       -- ^ Side-effect only function
notifyParentOfSolution parent done best = do
  -- We wait for an ack (get) to avoid a race condition where all children
  -- finish before the final updateBest task is ran on the master node.
  spawnAt parent $(mkClosure [| updateParentSolution best |]) >>= get
  rput done toClosureUnit -- Found solution, we are done!
  return ()

-- | Update the global solution with the new solution. If this succeeds then
--   tell all other nodes to update their local information.
updateParentSolution :: Closure a
                     -- ^ Solution
                     -> Thunk (Par (Closure ()))
                     -- ^ Side-effect only function
updateParentSolution s = Thunk $ do
  ref     <- io $ getRefFromRegistry solutionKey
  updated <- io $ atomicWriteIORef ref s

  return toClosureUnit

$(return []) -- TH Workaround
declareStatic :: StaticDecl
declareStatic = mconcat
  [
    declare $(static 'branchAndBoundChild)
  , declare $(static 'initRegistryBound)
  , declare $(static 'updateParentSolution)
  , Common.declareStatic
  , Types.declareStatic
  ]
