# Purpose

This repo is a template to start a new analysis using the
fantastic R-package [drake](https://books.ropensci.org/drake/)

Beside the RPushbullet integration the template uses only
drake functionalities. However, the functionalities I use
in my personal workflow are condensed in a file which makes
it easier to call them frequently.

# How to work with the template

* prep_drake_run.R is a convience function for analysing 
  and executing the plan, i.e.
  - restrict the execution to a subset of drake-targets
  - activate parallel computation only by specifying the 
    number of cpus
  - get outdated targets
  - plotting the subset of the selected targets
  - predict time to build the target under the specified
    number of cpus
  - build drake-targets in the console or under the 
    defined settings (selected target and number of cpus)
    in batch-mode

The following files follow the recommended pattern:

* R/plans.R defines the drake-plan(s)
* R/funs.R defines the functions that are used in the plans
* R/libs.R defines the libraries to be used

# RPushbullet

With Pushbullet you can sent notifications for instance to
your mobile phone. The template uses the RPushbullet package
to send such notifications if an error occured during
non-interactive execution of the drake-plan. But it is also
possible to send ggplot2-plot via Pushbullet, see 
h_send_plot_pushbullet().

In order to use Pushbullet with this template save the
Pushbullet-key in ~/.rpushbullet.json as 
~~~~
{
  "key": "..key.."
}
~~~~

# Packagemanger

The template uses renv to keep track of the package dependencies, 
where the packages can be restored with:
~~~~
install.packages("renv") # in case it is not already installed
renv::restore()
~~~~
