![CDS](https://github.com/CommerceDataService/netZero/blob/master/images/CDS-logo%20small.png) ![NIST Logo](https://github.com/CommerceDataService/netZero/blob/master/images/nist-small.png =1000x200)

# netZero

This repo houses data cleaning scripts, tutorials, visualizations and statistical models using data from the [NIST Net-Zero Energy Residential Test Facility (NZERTF)](https://pages.nist.gov/netzero/).

While this repo has some files with the data dictionary or variables flagged for removal, it does not have the full TDMS or CSV data files, since they are huge. We did, however include a small sample of each for reference.


## Cleaning scripts

The [convert_tdms_to_csv.py](https://github.com/CommerceDataService/netZero/blob/master/convert_tdms_to_csv.py) python script needs to be run first to convert TDMS files into CSV files. The paths to the TDMS file locations and the folder to save the CSV files will need to be updated before running.

The [cleaning_script.R](https://github.com/CommerceDataService/netZero/blob/master/cleaning_script.R) file can be run subsequently. 