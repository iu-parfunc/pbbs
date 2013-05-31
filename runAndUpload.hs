#!/usr/bin/env runghc

{-# LANGUAGE NamedFieldPuns #-}

-- Requires hsbencher >= 0.2

import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import System.Directory
import System.FilePath
import System.Exit
import System.Process
import GHC.Conc (getNumProcessors)
import System.IO.Unsafe (unsafePerformIO)

import HSBencher (defaultMainModifyConfig)
import HSBencher.Types
import HSBencher.Methods (makeMethod)
import HSBencher.Logging (log)
import HSBencher.MeasureProcess
import Prelude hiding (log)

type Kind = ([FilePath], FilePath, [(Int, FilePath)])

all_benches :: [Benchmark DefaultParamMeaning]
all_benches = concatMap expandBenches all_bench_kinds

all_bench_kinds :: [Kind]
all_bench_kinds =
  [ (sortBenches, "comparisonSort/sequenceData/data", sortInputs)
  , (bfsBenches,  "breadthFirstSearch/graphData/data", bfsInputs)
  ]

expandBenches :: Kind -> [Benchmark DefaultParamMeaning]
expandBenches (bins, dir, weightedfiles) =
  [ mkBenchmark bin [dir </> file] (tagVariant file stdParamSpace)
  | bin      <- bins
  , (_,file) <- weightedfiles ]

--------------------------------------------------------------------------------

tagVariant path conf =
  And [Set (Variant penultimate) (RuntimeParam ""),
       conf]
  where
    penultimate = takeBaseName$ dropTrailingPathSeparator path2
    (path2,_) = splitFileName path
    

stdParamSpace :: BenchSpace DefaultParamMeaning
stdParamSpace = varyThreads defaultSettings

defaultSettings :: BenchSpace DefaultParamMeaning
defaultSettings  = And []

varyThreads :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
varyThreads conf = And [ conf, Or (map fn threadSelection) ]
 where
   fn n = And$ map (Set (Threads n)) (fromJust (setThreads pbbsMethod) n)

threadSelection :: [Int]
threadSelection = unsafePerformIO $ do
  p   <- getNumProcessors
  return$
    if p <= 4  then [1..p] else
    if p <= 16 then 1: [2,4 .. p]
    else            1:2:[4,8 .. p]

--------------------------------------------------------------------------------

   -- convexHull/quickHull/
   -- convexHull/serialHull/
   -- delaunayRefine/incrementalRefine/
   -- delaunayTriangulation/incrementalDelaunay/
   -- delaunayTriangulation/serialDelaunay/
   -- dictionary/serialHash/
   -- dictionary/deterministicHash/   
   -- integerSort/blockRadixSort/
   -- integerSort/serialRadixSort/   
   -- maximalIndependentSet/serialMIS/
   -- maximalIndependentSet/incrementalMIS/
   -- maximalIndependentSet/ndMIS/   
   -- maximalMatching/incrementalMatching/
   -- maximalMatching/ndMatching/   
   -- minSpanningTree/parallelKruskal/
   -- minSpanningTree/serialMST/   
   -- nBody/parallelCK/
   -- nearestNeighbors/octTree2Neighbors/   
   -- rayCast/kdTree/   
   -- removeDuplicates/serialHash/
   -- removeDuplicates/deterministicHash/
   -- setCover/serialDFG/
   -- setCover/manis/
   -- spanningTree/serialST/
   -- spanningTree/incrementalST/
   -- spanningTree/ndST/
   -- suffixArray/parallelKS/
   -- suffixArray/serialKS/   

 -- Data directories:
 -- breadthFirstSearch/graphData/
 -- comparisonSort/sequenceData/
 -- convexHull/geometryData/
 -- delaunayRefine/geometryData/
 -- delaunayTriangulation/geometryData/
 -- dictionary/sequenceData/
 -- integerSort/sequenceData/
 -- maximalIndependentSet/graphData/
 -- maximalMatching/graphData/
 -- minSpanningTree/graphData/
 -- nBody/geometryData/
 -- nearestNeighbors/geometryData/
 -- rayCast/geometryData/
 -- removeDuplicates/sequenceData/
 -- setCover/graphData/
 -- spanningTree/graphData/
 -- suffixArray/sequenceData/

--------------------------------------------------------------------------------

sortBenches :: [String]
sortBenches =
  [ "comparisonSort/sampleSort/sort"
  , "comparisonSort/serialSort/sort"
  , "comparisonSort/stlParallelSort/sort" ]

sortInputs :: [(Int, String)]
sortInputs = [ (1, "randomSeq_10M_double"),
               (1, "exptSeq_10M_double"),
               (1, "almostSortedSeq_10M_double"),
               (3, "trigramSeq_10M"),
               (3, "trigramSeq_10M") ]

-- testsLong = [
--     [1, "randomSeq_100M_double", "", ""],
--     [1, "exptSeq_100M_double", "", ""],
--     [1, "almostSortedSeq_100M_double", "", ""],
--     [3, "trigramSeq_100M", "", ""],
--     [3, "trigramSeq_100M", "-p", ""]]

----------------------------------------

bfsBenches :: [String]
bfsBenches =
 [ "breadthFirstSearch/deterministicBFS/BFS"
 , "breadthFirstSearch/ndBFS/BFS"
 , "breadthFirstSearch/serialBFS/BFS" ]
  
bfsInputs :: [(Int, String)]
bfsInputs = 
  [ (1, "randLocalGraph_J_5_10000000")
  , (1, "rMatGraph_J_5_10000000")
  , (1, "3Dgrid_J_10000000") ]

--------------------------------------------------------------------------------


-- NBODY inputs:
--------------------------------------------------------------------------------
  
  -- [1, "3DonSphere_1000000", "", ""],
  -- [1, "3DinCube_1000000", "", ""],
  -- [1, "3Dplummer_1000000", "", ""]

--------------------------------------------------------------------------------

makeData :: Kind -> IO ()
makeData (_,dir,files) = do
   orig <- getCurrentDirectory  
   setCurrentDirectory dir
   forM_ files $ \ (_weight,file) -> do
     let cmd = "make " ++ file 
     putStr$ "Running: "++cmd
     ExitSuccess <- system cmd
     return ()
   setCurrentDirectory orig

--------------------------------------------------------------------------------

prebuiltBinaryMethod :: BuildMethod
prebuiltBinaryMethod = BuildMethod
  { methodName = "prebuiltBinary"
  , canBuild   = AnyFile
  , concurrentBuild = False
  , setThreads      = Nothing
  , clean = \_ _ _ -> return ()
  , compile = \ _ _ _ targetpath -> do
     log $ tag++"Nothing to do, prebuilt binary: "++targetpath
     let runit args envVars =          
           CommandDescr
           { command = RawCommand targetpath args
           , timeout = Just 300
           , workingDir = Nothing
           , envVars
           }
     return (RunInPlace runit)
  }
 where
  tag = " [prebuiltMethod] "

pbbsMethod :: BuildMethod
pbbsMethod = prebuiltBinaryMethod {
  setThreads = Just$ \ n -> [ RuntimeParam ("-p "++show n)
                            , RuntimeEnv   "CILK_NWORKERS" (show n) ]
  }

myconf :: Config -> Config
myconf conf =
  conf
   { benchlist = all_benches
   , buildMethods = [pbbsMethod]
   , argsBeforeFlags = False
   , harvesters = (pbbsHarvester, Nothing)
   }

-- | Tweaked version for pbbs:
pbbsHarvester = taggedLineHarvester (B.pack "PBBS-time")

--------------------------------------------------------------------------------

main :: IO ()
main = do
  p <- getNumProcessors
  putStrLn$ "Running on a machine with "++ show p++ " processors"
  putStrLn "First we generate the data files used by PBBS."
  mapM makeData all_bench_kinds

  putStrLn "\n Finished.  Now we begin benchmarking...\n"
  defaultMainModifyConfig myconf
