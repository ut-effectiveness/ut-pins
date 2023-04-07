# utPins
## Description:
This repository contains the R code that produces pins on the r studio connect server for Utah Tech University.

## Purpose:
Pins allow you to publish data sets, models, and other R objects.  More information on pins can be found by clicking [here.](https://pins.rstudio.com) Pins allow us to pull large data sets and have them readily available on the rstudio connect server.  This helps with performance and automation of reports and dashboards.

## Objectives:
- Create an enterprise solution using R Markdown and pins to automate data sets.  
- Create the ability to schedule a pin either on a daily, weekly or monthly schedule.
- Use try catch methods so that if a data pull fails, then other scheduled pins continue
- Create audit logs of start time and end time and display how long each pin takes to process and complete.
- Create error logs that will help in troubleshooting pins
- Ensure that connections to pins can be accessed both locally and by the server

