# -*- coding: utf-8 -*-
"""
Created on Tue Aug  6 19:16:01 2019

@author: Michael Kahle
"""

import codecs
fileName1='../csv/p_1000_1499.csv'
fileName2='../csv/p_1500_1995.csv'
#fileName3='spi.csv'  # add missing values from spi1 from dwd

outFile='../csv/p_1000_2018.csv'

precData = {}
precData['999'] = {'jan':'NA','feb':'NA','mar':'NA','apr':'NA','mai':'NA','jun':'NA',  
                   'jul':'NA','aug':'NA','sep':'NA','oct':'NA','nov':'NA','dec':'NA'} 


          
with open(fileName1, "r") as ins:
    i=0
    firstLine=""
    for line in ins:
        i=i+1
        #array.append(line)
        #print(line)
        if (1==i):
            firstLine = line
            #print(firstLine)
            header = line.split(',')
        if(i>1):
            #print(line)
            item = {}
            data = line.split(',')
            i = 0
            year = '003'
            for column in data:
              label = header[i].strip()
              value = data[i].strip()
              if(label=='year'):
                  year = value
                  precData[year] = {'jan':'NA','feb':'NA','mar':'NA','apr':'NA','mai':'NA','jun':'NA', \
                                    'jul':'NA','aug':'NA','sep':'NA','oct':'NA','nov':'NA','dec':'NA'}
              if(label=='winter'):
                  precData[str(int(year)-1)]['dec'] = value
                  precData[year]['jan'] = value
                  precData[year]['feb'] = value
              if(label=='spring'):
                  precData[year]['mar'] = value
                  precData[year]['apr'] = value
                  precData[year]['mai'] = value
              if(label=='summer'):
                  precData[year]['jun'] = value
                  precData[year]['jul'] = value
                  precData[year]['aug'] = value
              if(label=='autumn'):
                  precData[year]['sep'] = value
                  precData[year]['oct'] = value
                  precData[year]['nov'] = value                  
              i += 1
            #print(lineData) 
           
with open(fileName2, "r") as ins:
    i=0
    firstLine=""
    for line in ins:
        i=i+1
        #array.append(line)
        #print(line)
        if (1==i):
            firstLine = line
            #print(firstLine)
            header = line.split(',')
        if(i>1):
            #print(line)
            item = {}
            data = line.split(',')
            i = 0
            year = '004'
            for column in data:
              label = header[i].strip()
              value = data[i].strip()
              if(label=='year'):
                  year = value
                  precData[year] = {'jan':'NA','feb':'NA','mar':'NA','apr':'NA','mai':'NA','jun':'NA', \
                                    'jul':'NA','aug':'NA','sep':'NA','oct':'NA','nov':'NA','dec':'NA'}
              else:
                  precData[year][label] = value
              i += 1
  
months = ['jan','feb','mar','apr','mai','jun','jul','aug','sep','oct','nov','dec']            
csvfile = codecs.open(outFile, "w", "utf-8")
csvfile.write("year,month,time,m,precipitation\n")
for year in sorted(precData):
 m=1   
 for month in months:
   time = float(year)+(m-1.0)/12.0
   csvfile.write(year+","+str(m)+","+str(time)+","+month+","+precData[year][month]+"\n") 
   m+=1
csvfile.close()


              
