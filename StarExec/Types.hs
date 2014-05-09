module StarExec.Types where

import Prelude

data StarExecPrimType = Solver | Benchmark | Job | User | Space
    deriving (Eq, Show, Read)

data StarExecListType = Solvers | Benchmarks | Jobs | Users | Spaces
    deriving (Eq, Show, Read)
