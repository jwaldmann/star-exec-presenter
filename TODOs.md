
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
* unique solver -> solverId/configId/JobId
* ~~benchmark-names shorten with `drop` -> show max last xx chars~~
  -> shorten to 50 chars max
* cache space-xml
* ~~htmlProof over multiple lines~~
* ~~what about xml-proofs?~~
* why does a request on CompetitionWithConfigs pend so long?
* one route for show_job_results and flexible_tables
* disable sql-logging in
* show that jobs and pairs are incomplete
* ~~link job-, solver- and benchmark-infos to starexec~~
* write down the process of polling all data from star-exec
* check for unsafe reads
* ~~xml-proofs -> see job-results of 4067~~
* hidden competitions
