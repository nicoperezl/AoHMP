# plotparameter
linethickness <- 2

# install.packages("tidyverse")
# install.packages("data.table")
library(tidyverse)
library("data.table")

nhtsdata <- fread(
  "C:\\Users\\Nico\\Desktop\\INET PRJ\\NHTS17\\trippub.csv",
                                        sep = ",",
                                        dec = ".",
                                        select = c("TRPMILES", "TRPTRANS","WHYTRP1S"))

nhts_walk = nhtsdata$`TRPMILES`[nhtsdata$`TRPTRANS` == 1]*1.60934 # Walk and transform to km
nhts_bike = nhtsdata$`TRPMILES`[nhtsdata$`TRPTRANS` == 2]*1.60934 # Bicycle and transform to km
nhts_auto = nhtsdata$`TRPMILES`[nhtsdata$`TRPTRANS` == 3| #auto
                                  nhtsdata$`TRPTRANS` == 4| #suv
                                  nhtsdata$`TRPTRANS` == 5| #van
                                  nhtsdata$`TRPTRANS` == 18] *1.60934 #rentalcar
nhts_srpublic = nhtsdata$`TRPMILES`[nhtsdata$`TRPTRANS` == 10| # schulbuss
                                     nhtsdata$`TRPTRANS` == 11| # public bus
                                     nhtsdata$`TRPTRANS` == 12| # paratransit / dial-a-ride
                                     nhtsdata$`TRPTRANS` == 13| # private/charter/tour/shutnhtse bus
                                     nhtsdata$`TRPTRANS` == 15| # amtrack / commuter rail
                                     nhtsdata$`TRPTRANS` == 16| # subway
                                     nhtsdata$`TRPTRANS` == 17] *1.60934 # taxi /limo (incl uber)
nhts_lrpublic = nhtsdata$`TRPMILES`[nhtsdata$`TRPTRANS` == 14| # City-to-city bus (Greyhound, Megabus)	
                                      nhtsdata$`TRPTRANS` == 19|# Airplane
                                      nhtsdata$`TRPTRANS` == 20] *1.60934 # Boat/ferry/water taxi

nhts_total = c(nhts_walk, nhts_bike, nhts_auto, nhts_srpublic, nhts_lrpublic) # Total

par(pty='s')  # force the plot to be square before we start

plot(sort(nhts_walk), 1-ecdf(nhts_walk)(sort(nhts_walk)), #reinbe ploterstellung mit weißer linie, liegt unter grid
     log="xy", #definiert x&y als log skala
     ylim=c(1e-04,1e+0),  # untere+obere grenze von y
     xlim=c(1e-1,1e+3),  # untere+obere grenze von x
     xlab="Trip Length (km)",
     ylab="CCDF",
     sub ="(Fig. 1a)",
     type="s",
     col="white",# color
     lwd= linethickness) 

grid(10,10) #einsetzen des grids

lines(sort(nhts_walk), 1-ecdf(nhts_walk)(sort(nhts_walk)), #ccdf definition
      col="green",
      lwd= linethickness)
lines(sort(nhts_bike), 1-ecdf(nhts_bike)(sort(nhts_bike)), #ccdf definition
      col="blue",
      lwd= linethickness)
lines(sort(nhts_auto), 1-ecdf(nhts_auto)(sort(nhts_auto)), #ccdf definition
      col="orange",
      lwd= linethickness)
lines(sort(nhts_srpublic), 1-ecdf(nhts_srpublic)(sort(nhts_srpublic)), #ccdf definition
      col="brown",
      lwd= linethickness)
lines(sort(nhts_lrpublic), 1-ecdf(nhts_lrpublic)(sort(nhts_lrpublic)), #ccdf definition
      col="grey",
      lwd= linethickness)
lines(sort(nhts_total), 1-ecdf(nhts_total)(sort(nhts_total)), #ccdf definition
      col="black",
      lwd= linethickness)

legend('bottomleft', #definition der legende
       legend=c("I. All Modes",
                "A. Walking",
                "B. Bicycling",
                "C. Automobiles",
                "D. ÖPNV",
                "E. Long Distance Travel"),  # text in the legend
       col=c("black","green","blue","orange","brown", "grey"),  # point colors
       lty = 1, # lines als symbol in legende
       bty = "n", # keine boarder und hintergrund
       title = "Mode", #legendentitel
       title.adj = 0.1,         # Horizontal adjustment of the tinhtse
       title.cex = 1.5)

