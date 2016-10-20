![CDS logo](https://github.com/CommerceDataService/netZero/blob/master/images/CDS-logo%20small.png) ![NIST Logo](https://github.com/CommerceDataService/netZero/blob/master/images/nist-small.png)

# netZero

This repo houses data cleaning scripts, tutorials, visualizations and statistical models using data from the [NIST Net-Zero Energy Residential Test Facility (NZERTF)](https://pages.nist.gov/netzero/).

While this repo has some files with the data dictionary or variables flagged for removal, it does not have the full TDMS or CSV data files, since they are huge. We did, however include a small sample of each for reference.


## Cleaning scripts

The [convert_tdms_to_csv.py](https://github.com/CommerceDataService/netZero/blob/master/convert_tdms_to_csv.py) python script needs to be run first to convert TDMS files into CSV files. The paths to the TDMS file locations and the folder to save the CSV files will need to be updated before running.

The [cleaning_script.R](https://github.com/CommerceDataService/netZero/blob/master/cleaning_script.R) file can be run subsequently. 

### Data caveats

Here are a few data cleaning and manipulation issues to keep in mind:

- Various channels and systems may not always exactly have 1440 readings per day because of resets at midnight, instrumentation boot time lags, different intervals for collecting data from instruments. Channel readings are rounded to the nearest minute and if readings start after midnight, the channel values are zero up until the readings begin.
- The TimeStamp_SystemTime variable, rounded to the minute, was used as a timestamp for the data from the LabView system. For the alternate system, 1440 readings were provided for each day, so an appropriate minute-level timestamp was generated.

## Tutorials

R markdowns and knitted html outputs are available for tutorials in R. Refer to the html file to see the code, formatting and output in one file. The tutorials have been posted on the [Net Zero website](https://pages.nist.gov/netzero/). 