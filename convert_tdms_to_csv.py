#!/usr/bin/env python

from nptdms import TdmsFile
import glob

for filename in glob.iglob('/Volumes/NO NAME/*/*.tdms'):
    tdms_file = TdmsFile(filename)
    temp = tdms_file.as_dataframe(time_index=False, absolute_time=False)
    temp.to_csv(path_or_buf="/Users/prioberoi/Documents/nist/netZero/data/raw/"+filename[28:-13]+".csv", encoding='utf-8')