#The boundaries of the modelled region are arbitrary, but the coordinates for the centroids were calculated using
#polygon data by Departamento Administrativo Nacional de Estadística (2020) and Datos Abiertos Bogotá (2020)
#

#Load packages----------------------------------------------------------------------------

library(rstan)
library(RColorBrewer)

#make results reproducible 
set.seed(2222)


#Load municipality centroids taken from DANE and Datos Abiertos Bogotá--------------------

#load coordinates
examp_coord <- read.csv("./figure3_gp_dmat/hypo_muisca_coord.csv")
examp_coord$combined <- gsub(", ", "..", examp_coord$combined)
examp_coord$combined <- gsub(" ", ".", examp_coord$combined)
examp_coord <- examp_coord[!duplicated(examp_coord$combined),19:21]


#Compile and run Stan Model for simulating realisations from a 2D GP-----------------------

sim_gp <- stan_model(file="./figure3_gp_dmat/sim_from_dmat.stan")

#create vector of 1s for intercpet
X <- as.data.frame(rep(1, times=length(examp_coord$combined)))


#select six par combinations 
pars_eta <- c(rep(5,times=3),rep(.8,times=3))
pars_rho <- c(rep(c(10, 30, 70), times=2))
pars_sigma <- c(rep(0.001, times=6))


sim <- list()


#run twice to get final result

for (i in 1:6) {
  
  sim[[i]] <- sampling(sim_gp,
                       data = list(N = length(examp_coord$combined),
                                   M = 1,
                                   K = ncol(X),
                                   X = X,
                                   eta_real = pars_eta[i],
                                   rho_real = pars_rho[i],
                                   sigma_real = 0.001,
                                   lat = examp_coord$latitude*111.1,
                                   lon = examp_coord$longitude*111.1),
                                   algorithm = c("Fixed_param"),
                                   iter=3, chains=1, warmup=0)
}


#Extract samples--------------------------------------------------------------------

prior <- list()

for (i in 1:6) {
  
  prior[[i]] <- extract(sim[[i]])
  
}


#Plot simulations------------------------------------------------------------------------

#choose col palette
rbPal <- colorRampPalette(c("white",brewer.pal(n = 9, name = "YlOrRd")))(10)

#for legend
ranges <- paste(seq(from=0, to=100, by=10),"-", seq(from=10, to=100, by=10))

#look at covariance for hypo simulations
png(file="./figure3_gp_dmat/hypo_cu.png",
    width=1548,
    height=740
)

par(mfrow=c(2,3),
    lwd=1,
    cex=1,
    mar=c(4,4,2,2),
    mgp = c(2.5, 1, 0),
    omi=c(0.05,0.05,0.05,0.05))



for (i in 1:6) {
  
  plot(
    examp_coord$longitude ,
    examp_coord$latitude ,
    xlab = "longitude" ,
    ylab = "latitude" ,
    bg = rbPal[cut(
      prior[[i]]$y_sim[1,],
      breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
      include.lowest = TRUE
    )] ,
    cex = 2,
    cex.lab = 1.5,
    cex.axis = 1.5,
    pch = 21
  )
  
  title(bquote(paste(
    .(letters[i]),") " 
    ~ eta,"=",.(prior[[i]]$eta[1]),
    ~ rho,"=",.(prior[[i]]$rho[1]),
    ~ sigma,"=",.(prior[[i]]$sigma[1])
  )))
  
  legend("topleft",legend=c(ranges[1:10]),
         pch=21,
         pt.bg=colorRampPalette(c("white",brewer.pal(n = 9, name = "YlOrRd")))(10),
         title="Cu (wt%)")  
  
}


dev.off()
