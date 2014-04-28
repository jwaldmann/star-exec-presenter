* piping in requests: (e.g.)
    * => index -> login
    * => index -> logout
    * => index -> getUserID -> getJobs ...
* maybe use a Monad: StarExecSession-Monad (?)


Certified Solver, which takes previous results of other solvers to test them
    -> maybe as post-processor
    -> or additional functionality with SE