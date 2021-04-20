# Files Overview

* Files for the user
  - R/plans.R
    - defines the drake-targets
  - R/funs.R
    - defines the functions used in R/plans.R 
  - R/libs.R
    - libraries for the template but also for the
      functions defined in R/funs.R
  - prep_drake_run.R
    - here one prepares the next drake-run, e.g. restrict
      the targets that should be build, visualize the graph
      of the targets, etc.
* Files not meant for the user
  - R/infrastructure.R
    - functions called by the user to restrict targets,
      choose number of cpus used to build the targets, etc.
  - _drake.R
    - defines the drake-configuration that is used by
      drake::make() and drake::r_make()
      
# Data Flow

prep_drake_run.R saves the information about the targets
that should be build and t
When the targets for the next run are restricted 
_drake.R defines the drake-configuration and is used by
drake::make and drake::r_make but the user actual uses a different
file to restrict the targets to be build or to choose the number of 
CPUs 
