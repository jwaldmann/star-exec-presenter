
getJobInfo:
https://www.starexec.org/starexec/secure/download?id=1095&type=job&returnids=true
id -> job-id
type -> job
returnids -> true

getJobPair:
https://www.starexec.org/starexec/secure/download?id=12185856&type=jp_output
id -> job-pair-id (see result of getJobInfo)

* top-level:

```
data Competition = Competition Text [Metacategory] 
data Metacategory = Metacategory Text [Category] -> solver nach rang in den categories
data Category = Category Text [JobId] -> solver nach YES/CERTIFIED/NO sortiert, evtl mit scoring -> SolverResult

instance PathPiece Competition where ... 


c :: Competition 
c = Competition "2014" 
[ Metacategory "Termination" 
[ Category "TRS_Standard" [ 13,14,15 ] 
, Category "SRS_Standard" [16,17,18] 
] 
... ] 
```

# 140630

* ~~route to show all competitions~~
  * ~~example competition~~
* ~~SolverResult -> YES Maybe Int~~
* ~~link to solver-info, benchmark-info, job-info~~

# 140707

* ~~YES with Int -> smaller int is better -> add to results~~
  -> added polynomial detection to YES-results
* ~~unique solver -> solverId/configId/JobId~~
* ~~benchmark-names shorten with `drop` -> show max last xx chars~~
  -> shorten to 50 chars max
* ~~cache space-xml~~
* ~~htmlProof over multiple lines~~
* ~~what about xml-proofs?~~
* ~~why does a request on CompetitionWithConfigs pend so long?~~
  -> ~~there is no request to star-exec during the execution of the handler...~~
* ~~one route for show_job_results and flexible_tables~~
  -> ~~overlapping routes~~
* disable sql-logging in
* ~~show that jobs and pairs are incomplete~~
  -> done for `/show_job_results`
* ~~link job-, solver- and benchmark-infos to starexec~~
* ~~write down the process of polling all data from star-exec~~
* ~~check for unsafe reads~~
* ~~xml-proofs -> see job-results of 4067~~
* ~~hidden competitions~
* ~~refresh of compititions if job-pair or job incomplete~
* ~~isComplexity for JobInfo~~
* ~~Complexity-Scores: only Score: XXX, not YES: XXX, NO: XXX~~
* ~~display, that jobs/competition incomplete~~
* ~~extend Category with post-proc-info~~
* ~~scores for non-complexities like complexity: 1 point for each YES or NO~~
* ~~list post-proc in categories-list of competition and wether complete/incomplete~~
* ~~show, that meta-cat is incomplete, because a sub-cat is incomplete~~
* ~~hidden-test-runs: show all competitions~~
* ~~handle new record-field star-date~~
* ~~show duration in show-job-results and competitions~~
  * ~~completion-time and start-time~~
* ~~add min-start-time and max-completition-time to competition, meta-cat and cat~~
* ~~completion time for job-results~~
* ~~authentication for hidden-test-runs (and job-results)~~
* ~~why 0.0 on incomplete job-results?~~
* ~~route for competition_info by db-id~~
* ~~extend competition with meta-data~~
* ~~remove duration from category~~
* ~~add competition-meta -> name + desc~~
  - ~~deriving Eq, Ord~~
  - ~~TVar-Map -> key = CompMeta~~
* ~~start test-competition with ~12h duration~~
* ~~post-proc-info~~

# 140804

* load and cache space-hierarchy for imports
* check for unsafe reads
* write down the process of polling all data from star-exec
* move star-exec-session-data to a TVar
* import for old data
  - import-route
  - import form
  - import settings
  - import cases (LRI, UIBK)

# 140924

* Listing of jobs (with filter)
  - checkbox-selection for comparison of results
* mapping of Benchmarks and Solver
  - StarExec-Benchmark <-> LRI-Benchmark
  - StarExec-Solver <-> LRI-Solver
  - also considering UIBK
* url-shortener
* research best-practices for REST 
