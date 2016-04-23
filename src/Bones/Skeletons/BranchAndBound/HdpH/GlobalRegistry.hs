module Bones.Skeletons.BranchAndBound.HdpH.GlobalRegistry
  (
    registry

  , getRefFromRegistry
  , addRefToRegistry

  , readFromRegistry
  , addToRegistry

  , addGlobalSearchSpaceToRegistry
  , getGlobalSearchSpace

  , initRegistryBound

  , getUserState
  , putUserState

  , searchSpaceKey
  , solutionKey
  , solutionSignalKey
  , boundKey
  , userStateKey
  ) where

import           Control.Parallel.HdpH (Closure, Thunk(..), Par, io)

import           Data.IORef      (IORef, atomicWriteIORef, newIORef, readIORef)

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM (empty, insert, lookup)

import           System.IO.Unsafe      (unsafePerformIO)

--------------------------------------------------------------------------------
-- Global (process local) Registry
--------------------------------------------------------------------------------

registry :: IORef (IntMap (IORef a))
{-# NOINLINE registry #-}
registry = unsafePerformIO $ newIORef IM.empty

getRefFromRegistry :: Int -> IO (IORef a)
getRefFromRegistry k = do
  r <- readIORef registry
  case IM.lookup k r of
    Nothing -> error $ " Could not find key: " ++ show k ++ " in global registry."
    Just x  -> return x

readFromRegistry :: Int -> IO a
readFromRegistry k = getRefFromRegistry k >>= readIORef

addRefToRegistry :: Int -> IORef a -> IO ()
addRefToRegistry k v = do
  reg <- readIORef registry
  atomicWriteIORef registry $ IM.insert k v reg

addToRegistry :: Int -> a -> IO ()
addToRegistry k v = newIORef v >>= addRefToRegistry k


-- Functions a user can use to manipulate state
getUserState :: IO a
getUserState = readFromRegistry userStateKey

putUserState :: a -> IO ()
putUserState = addToRegistry userStateKey

--------------------------------------------------------------------------------
-- Skeleton Interface
--------------------------------------------------------------------------------

searchSpaceKey :: Int
{-# INLINE searchSpaceKey #-}
searchSpaceKey = 0

solutionKey :: Int
{-# INLINE solutionKey #-}
solutionKey = 1

solutionSignalKey :: Int
{-# INLINE solutionSignalKey #-}
solutionSignalKey = 3

boundKey :: Int
{-# INLINE boundKey #-}
boundKey = 2

userStateKey :: Int
{-# INLINE userStateKey #-}
userStateKey = 3

initRegistryBound :: Closure a -> Thunk (Par ())
initRegistryBound bnd = Thunk $ io (addToRegistry boundKey bnd)

addGlobalSearchSpaceToRegistry :: IORef a -> IO ()
addGlobalSearchSpaceToRegistry = addRefToRegistry searchSpaceKey

getGlobalSearchSpace :: IO a
getGlobalSearchSpace = readFromRegistry searchSpaceKey
