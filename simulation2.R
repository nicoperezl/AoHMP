#load("tl_lrpublic.rda") #daten aus anhang, ggf pfad anpassen
#load("tl_autodriver.rda") #daten aus anhang, ggf pfad anpassen

# Settings
linethickness <- 2
line1 = 99 # händischer vergleich linenummer
samplezahl = 400 # anzahl trips die für jede paramterkombination generiert werden soll. hoher einfluss auf rechendauer!
targetvec <- tl_autodriver # vector auf den gematcht werden soll
messpt <- c(1, 3, 5.5, 10, 30, 55, 100, 300) # triplängen an denen die differenz zwischen targetvec und modell output ermittelt und aufsummiert wird

#definition
eps_range <- seq(0, 2, by=0.05)#0.05
p_range <- seq(0.1, 0.95, by=0.05)#0.05
alp_range <- seq(1.1, 2.5, by=0.05)#0.05


kombitable <- expand.grid(eps_range, p_range, alp_range, ptfehler) # tabellenerstellung aller möglichen wertekombis

for (i in 1:nrow(kombitable)) {
  eps <- kombitable[i , ]$Var1
  p <- kombitable[i , ]$Var2
  alp <- kombitable[i , ]$Var3
  vec <- vector()
  
  for (n in 1:samplezahl) {
    distanz <- 9999999
    while(distanz >= 1000){
      sprungl <- runif(1)^(-1/(alp-1))
      random <- runif(1)
      distanz <- sprungl
      
      while (random < p) {
        distanz <- (1+eps)* distanz
        random <- runif(1)
      }
    }
    vec <- append(vec, distanz)
  }
  ptfehler <- 0
  
  for (x in messpt){
    ptfehler <- ptfehler + ((((ecdf(vec)(x)-ecdf(targetvec)(x))^2)^0.5)/(1-ecdf(targetvec)(x)))
  }
  kombitable[i , ]$Var4 <- ptfehler
  print (i)
  print(kombitable[i , ])
  print((i/nrow(kombitable))*100)
}

# ---------- graphen aus tabelle zeichnen


# in einen vector loopen
vec <- vector()
eps = 2#kombitable[line1,1]
p = 0.7#kombitable[line1,2]
alp = 1.1#kombitable[line1,3]

for (i in 1:samplezahl) {
  distanz <- 9999999
  while(distanz >= 1000){
    sprungl <- runif(1)^(-1/(alp-1))
    random <- runif(1)
    distanz <- sprungl
    
    while (random < p) {
      distanz <- (1+eps)* distanz
      random <- runif(1)
    }
  }
  vec <- append(vec, distanz)
}


# in einen vector loopen 2
line2 = which.min(kombitable$Var4)
vec2 <- vector()
eps2 = kombitable[line2,1]
p2 = kombitable[line2,2]
alp2 = kombitable[line2,3]

for (i in 1:samplezahl) {
  distanz2 <- 9999999
  while(distanz2 >= 1000){
    sprungl2 <- runif(1)^(-1/(alp2-1))
    random2 <- runif(1)
    distanz2 <- sprungl2
    
    while (random2 < p2) {
      distanz2 <- (1+eps2)* distanz2
      random2 <- runif(1)
    }
  }
  vec2 <- append(vec2, distanz2)
}

# in einen vector loopen 3
line3 = which.max(kombitable$Var4)
vec3 <- vector()
eps3 = kombitable[line3,1]
p3 = kombitable[line3,2]
alp3 = kombitable[line3,3]

for (i in 1:samplezahl) {
  distanz3 <- 9999999
  while(distanz3 >= 1000){
    sprungl3 <- runif(1)^(-1/(alp3-1))
    random3 <- runif(1)
    distanz3 <- sprungl3
    
    while (random3 < p3) {
      distanz3 <- (1+eps3)* distanz3
      random3 <- runif(1)
    }
  }
  vec3 <- append(vec3, distanz3)
}

# draw graph
par(pty='s')  # force the plot to be square before we start
plot(sort(targetvec), 1-ecdf(targetvec)(sort(targetvec)), #reine ploterstellung mit weißer linie, liegt unter grid
     log="xy", #definiert x&y als log skala
     ylim=c(1e-04,1e+0),  # untere+obere grenze von y
     xlim=c(1e-1,1e+4),  # untere+obere grenze von x
     xlab="Trip Length (km)",
     ylab="CCDF",
     sub ="(Fig. 1a)",
     type="s",
     col="white",# color
     lwd= linethickness) 
grid(10,10) #einsetzen des grids
lines(sort(targetvec), 1-ecdf(targetvec)(sort(targetvec)), #ccdf definition
      col="green",
      lwd= linethickness)
lines(sort(vec), 1-ecdf(vec)(sort(vec)), #ccdf definition
      col="blue",
      lwd= linethickness)
lines(sort(vec2), 1-ecdf(vec2)(sort(vec2)), #ccdf definition
      col="orange",
      lwd= linethickness)
lines(sort(vec3), 1-ecdf(vec3)(sort(vec3)), #ccdf definition
      col="red",
      lwd= linethickness)
legend('bottomleft', #definition der legende
       legend=c("A. Vec1 handpicked",
                "B. Auto Driver",                          #ggf anpassen
                "C. Vec2 best fit",
                "D. Vec3 worst fit"),  # text in the legend
       col=c("blue","green","orange","red"),  # point colors
       lty = 1, # lines als symbol in legende
       bty = "n", # keine boarder und hintergrund
       title = "Mode", #legendentitel
       title.adj = 0.1,         # Horizontal adjustment of the title
       title.cex = 1.5)
