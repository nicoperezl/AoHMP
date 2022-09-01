# AoHMP
Network Architecture Project - Analysis of Human Mobility Patterns
## Draw MID CCDF Graphs
To draw MID CCDF grpahs the twozero.r file is important.
The path in line 9 must be changed so that it goes to the MiD2017_Wege.csv data from MID 2017 on YOUR PC.
It's also necessary to get twozero.r up and running in r studio at leat one time to execute simulation2.r,
as it creates vectors that simulation2.r relies on.
## Draw NHTS CCDF Graphs
To draw NHTS CCDF grpahs the NHTS.r file is important.
The path in line 10 must be changed so that it goes to the trippub.csv from NHTS 2017 data on YOUR PC.
## Find optimal alpha, epsilon, p values to resemble specific mode in distribution
For this the simulation2.r AND twozero.R is important.
targetvec in line 8 must be set to the vector that our model output should look like.
messpt must be set to include every point on which distance between modelcreated and realdate ecd difference should be messured.
samplezahl should be chosen in relation to highest value in messpt. if 1% of all trips of selected targetvec are long(er) then highest messpt, it's enough to set samplezahl to 100. If its 0.1% it should be set to 1000 corespondingly.
