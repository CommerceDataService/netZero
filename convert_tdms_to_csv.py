#!/usr/bin/env python

from nptdms import TdmsFile
import glob
import re

# Update paths below to directory with tdms files (input_dir) and directory for csv outputs (output_dir)
input_dir = '/Users/prioberoi/Documents/nist/netZero/data/raw/'
output_dir = '/Users/prioberoi/Documents/nist/netZero/data/raw/'

for filename in glob.iglob(input_dir+'*.tdms'): #update filepath to location with .tdms files from LabView
    tdms_file = TdmsFile(filename)
    temp = tdms_file.as_dataframe(time_index=False, absolute_time=False)
    start_loc = re.search(' All Day.tdms', filename).start()-10
    stop_loc = re.search(' All Day.tdms', filename).start()
    temp.to_csv(path_or_buf=(output_dir+filename[start_loc:stop_loc]+".csv"), encoding='utf-8')