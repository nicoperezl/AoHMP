# plotparameter
linethickness <- 2

# install.packages("tidyverse")
# install.packages("data.table")
library(tidyverse)
library("data.table")

tldata <- fread("C:\\Users\\Nico\\Desktop\\INET PRJ\\B1_Standard-Datensatzpaket\\CSV\\MiD2017_Wege.csv", #path must be changed!
                                        sep = ";",
                                        dec = ",",
                                        select = c("wegkm",
                                                   "hvm",
                                                   "hvm_diff2"))
tldata <- tldata[!(tldata$`wegkm`=="9994" |
                     tldata$`wegkm`=="9999" |
                     tldata$`wegkm`=="70703")] # exclude unwanted lines with "unknown" distancevalue et al

tl_walk = tldata$`wegkm`[tldata$`hvm` == 1] # Walk
tl_bike = tldata$`wegkm`[tldata$`hvm` == 2] # Bicycle (incl. Ebike)
tl_autopass = tldata$`wegkm`[tldata$`hvm` == 3] # Auto Passengers
tl_autodriver = tldata$`wegkm`[tldata$`hvm` == 4] # Auto Drivers
tl_srpublic = tldata$`wegkm`[tldata$`hvm` == 5 & !(
                               tldata$`hvm_diff2` =="19"|
                               tldata$`hvm_diff2` =="20"|
                               tldata$`hvm_diff2` =="21"|
                               tldata$`hvm_diff2` =="22"|
                               tldata$`hvm_diff2` =="23")]  # Public Transport w/o long distance vehicles
tl_lrpublic = tldata$`wegkm`[tldata$`hvm_diff2` == 19 | #Schiff&Fähre
                   tldata$`hvm_diff2` == 20 | #Fernzug
                   tldata$`hvm_diff2` == 21 | #Fernlinienbus
                   tldata$`hvm_diff2` == 22 ]#| #Reisebuss
                   #tldata$`hvm_diff2` == 23 ] #Flugzeug

tl_total = c(tl_walk, tl_bike, tl_bike, tl_autopass,
             tl_autodriver, tl_srpublic, tl_lrpublic) # Total

par(pty='s')  # force the plot to be square before we start

plot(sort(tl_walk), 1-ecdf(tl_walk)(sort(tl_walk)), 
     #reine ploterstellung mit weißer linie, liegt unter grid
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

lines(sort(tl_walk), 1-ecdf(tl_walk)(sort(tl_walk)), #ccdf definition
      col="green",
      lwd= linethickness)
lines(sort(tl_bike), 1-ecdf(tl_bike)(sort(tl_bike)), #ccdf definition
      col="blue",
      lwd= linethickness)
lines(sort(tl_autodriver), 1-ecdf(tl_autodriver)(sort(tl_autodriver)), #ccdf definition
      col="orange",
      lwd= linethickness)
lines(sort(tl_srpublic), 1-ecdf(tl_srpublic)(sort(tl_srpublic)), #ccdf definition
      col="brown",
      lwd= linethickness)
lines(sort(tl_total), 1-ecdf(tl_total)(sort(tl_total)), #ccdf definition
      col="black",
      lwd= linethickness)
lines(sort(tl_lrpublic), 1-ecdf(tl_lrpublic)(sort(tl_lrpublic)), #ccdf definition
      col="grey",
      lwd= linethickness)

legend('bottomleft', #definition der legende
       legend=c("I. All Modes",
                "A. Walking",
                "B. Bicycling",
                "C. Automobile Driver",
                "D. Short Range PT",
                "E. Long Range PT"),  # text in the legend
       col=c("black","green","blue","orange","brown", "grey"),  # point colors
       lty = 1, # lines als symbol in legende
       bty = "n", # keine boarder und hintergrund
       title = "Mode", #legendentitel
       title.adj = 0.1,         # Horizontal adjustment of the title
       title.cex = 1.5)

#------------- linear trip distribution through mode of transportation -table

#install.packages(dplyr)
#library(dplyr)
#zusammenfassung <- tldata[tldata$`hvm` == 4] %>% 
#  group_by(hvm_diff2) %>% 
#  summarise(totalcount = n(),above100km = sum(wegkm > 100), percent = above100km/totalcount)
#zusammenfassung