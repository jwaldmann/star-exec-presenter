* piping in requests: (e.g.)
    * => index -> login
    * => index -> logout
    * => index -> getUserID -> getJobs ...
* maybe use a Monad: StarExecSession-Monad (?)


Certified Solver, which takes previous results of other solvers to test them
    -> maybe as post-processor
    -> or additional functionality with SE

## Job-Management

-> register spaces by their id
-> update space:
    -> get solver
    -> get benchmarks
    -> get jobs
-> update jobs
    -> get job(s) status
-> list spaces
    -> list jobs
    -> list benchmarks
    -> list solver

## Model

    User
        seId
        spaces :: [SpaceId]

    Space
        seId
        UserId
        solver :: [SolverId]
        benchmarks :: [BenchmarkId]
        jobs :: [JobId]

    Solver
        UserId
        SpaceId

    Benchmark
        UserId
        SpaceId

    Job
        UserId
        SpaceId

## Routes

/user                           -> Overview of users spaces
/(user/)space/#Int              -> Overview of space with id
/(user/)space/#Int/solver       -> Overview of solver in specific space
/(user/)space/#Int/benchmarks   -> Overview of benchmarks in specific space
/(user/)space/#Int/jobs         -> Overview of jobs in specific space
/(user/)solver                  -> Overview of all solver of the current user
/(user/)benchmarks              -> Overview of all benchmarks of the current user
/(user/)jobs                    -> Overview of all jobs of the current user
/register
