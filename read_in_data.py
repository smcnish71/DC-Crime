#import crime data

import pandas as pd
import glob

path =r'C:\Users\smcnish\Documents\Vizs\Crime\DC-Crime\raw data'
allFiles = glob.glob(path + "/*.csv")
frame = pd.DataFrame()
list_ = []
for file_ in allFiles:
    df = pd.read_csv(file_,index_col=None, header=0)
    list_.append(df)
frame = pd.concat(list_)
