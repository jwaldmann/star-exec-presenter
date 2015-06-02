module Importer.Internal where

import Prelude
import Data.ByteString.Lazy
import Data.Maybe

-- datatypes and type synonyms for LRI data

type Line = ByteString
type Lines = [ByteString]
type Name = ByteString
type Key = ByteString
type Value = ByteString
type Entry = (Name, Content)
type Content = [(Key, Value)]

data ParseState = None | Unfinished Name Content

data LRIBenchmark = LRIBenchmark
  { lribIdentifier :: !Name
  , lribName :: !Name               -- key: name
  , lribFile :: !Name               -- key: file
  , lribRating :: !Int              -- key: rating
  , lribSolved :: !Int              -- key: solved
  , lribConditional :: !Bool        -- key: conditional, value: true | false
  , lribContextSensitive :: !Bool   -- key: contextsensitive, value: true | false
  , lribInnermost :: !Bool          -- key: innermost, value: true | false
  , lribOutermost :: !Bool          -- key: outermost, value: true | false
  , lribRelative :: !Bool           -- key: relative, value: true | false
  , lribTheory :: !Bool             -- key: theory, value: true | false
  } deriving (Show,Eq)

data LRISolver = LRISolver
  { lrisIdentifier :: !Name
  , lrisName :: !Name               -- key: name
  , lrisAuthor :: !Name             -- key: author
  , lrisDescription :: !ByteString  -- key: description
  , lrisURL :: !ByteString          -- key: url
  , lrisStandard :: !Bool           -- key: standard, value: true | false
  , lrisRelative :: !Bool           -- key: relative, value: true | false
  , lrisConditional :: !Bool        -- key: conditional, value: true | false
  , lrisContextSensitive :: !Bool   -- key: contextsensitive, value: true | false
  , lrisInnermost :: !Bool          -- key: innermost, value: true | false
  , lrisTheory :: !Bool             -- key: theory, value: true | false
  , lrisCertifying :: !Bool         -- key: certifying, value: true | false
  } deriving (Show,Eq)

data LRIPairResult = LRIYES | LRINO | LRIERROR | LRIMAYBE | LRITIMEOUT | LRIOTHER ByteString
  deriving (Show,Eq)

data LRIResult = LRIResult
  { lrirPair :: !Key
  , lrirBenchmark :: !Key
  , lrirSolver :: !Key
  , lrirResult :: !LRIPairResult
  , lrirCpuTime :: !Double
  , lrirWallclockTime :: !Double
  , lrirCheckResult :: !(Maybe LRIPairResult)
  , lrirCheckCpuTime :: !(Maybe Double)
  , lrirCheckWallclockTime :: !(Maybe Double)
  } deriving (Show,Eq)

-- datatypes and type synonyms for UIBK data

type Version = ByteString
type UIBKSolverResult = ByteString
type Path = ByteString

data UIBKCompetition = UIBKCompetition
  { uibkCompID :: !Int
  , uibkCompName :: !Name
  , uibkCompCategories :: ![UIBKCategory]
  }
  deriving (Show, Read)

data UIBKCategory = UIBKCategory
  { uibkCatID :: !Int
  , uibkCatName :: !Name
  , uibkCatEntries :: ![UIBKResult]
  }
  deriving (Show, Read)

data UIBKResult = UIBKResult
  { uibkResultID :: !Int
  , uibkResultInputProblemID :: !Int
  , uibkResultInputProblemPath :: !Path
  , uibkResult :: !UIBKSolverResult
  , uibkResultWallclockTime :: !Int
  , uibkResultTool :: !Name
  , uibkResultToolVersion :: !Version
  }
  deriving (Show, Read, Eq, Ord)

data UIBKBenchmark = UIBKBenchmark
  { uibkBenchmarkID :: !Int
  , uibkBenchmarkPath :: !Path
  }
  deriving (Show, Read, Eq, Ord)

data UIBKSolver = UIBKSolver
  { uibkSolverName :: !Name
  , uibkSolverVersion :: !Version
  }
  deriving (Show, Read, Eq, Ord)
