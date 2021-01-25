LOAD DATA LOCAL INFILE 'C:/Users/t24x137/Desktop/TempTool_2020/IskulpaaRiverTemp_TempToolFormat_noheader.csv'
INTO TABLE temptoolfour.ts_upstreamtemp
FIELDS TERMINATED BY ","
LINES TERMINATED BY "\n"