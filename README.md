# utPins
## Description:
This repository contains the R code that produces pins on the r studio connect server for Utah Tech University.

## Purpose:
Pins allow you to publish data sets, models, and other R objects.  More information on pins can be found by clicking [here.](https://pins.rstudio.com) Pins allow us to pull large data sets and have them readily available on the rstudio connect server.  This helps with performance and automation of reports and dashboards built using R Markdown and Shiny.

## Objectives:
- Create an enterprise solution using R Markdown and pins to automate data sets.  
- Add functionality to schedule a pin either on a daily, weekly or monthly cadence.
- Use try catch methods so that if a pin fails, then other scheduled pins will try and run
- Create audit logs of start time and end time and display how long each pin takes to process and complete.
- Create error logs that will help in troubleshooting pins when errors do occur.
- Ensure that connections to pins can be accessed both locally and by the server

## Development notes:
Pre-commit hooks have been added to the project to help maintain a consistent style, create
documentation and to check various other issues (spelling, committing of large files). Each
developer would have to set up the pre-commit hooks on their own machine, so these are
non-obligatory tools.

If you would like to use the pre-commit hooks, you will first need to install:

- the python `pre-commit` tool [Installation notes](https://pre-commit.com/#install)
- the R package `{precommit}` [CRAN Link](https://cran.rstudio.com/web/packages/precommit/index.html)

With those two tools in place, you can run:

- `precommit::use_precommit()` in R, or
- `pre-commit install --install-hooks` from the command line (in the {utPins} root directory)

When calling `git commit ...` the hooks will run, and if any changes are made to, or any errors are
found in, the committed files then the commit will not be made. You will have to `git add ...`
the files again (after fixing any issues).
