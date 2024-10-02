#Load packages -------------------------------------------------------------------------

library(rstan)
library(rethinking)

#make results reproducible
set.seed(1111)


#Compile and run Stan Model for simulating realisations from a 1D GP-----------------------

sim_code <- stan_model("./figure2_gp_simple/simple_gp_model.stan")

#create empty list for simulations with different pars 
sim <- list()

#select six par combinations 
pars_eta <- c(rep(5,times=3),rep(.8,times=3))
pars_rho <- c(rep(c(10, 30, 70), times=2))
pars_sigma <- c(rep(0.001, times=6))

#run simulations

for (i in 1:6) {
  
  sim[[i]] <- sampling(sim_code,
                       data = list(N_obs = 500, 
                                   x = seq(from=0, to=400, length.out=500),
                                   eta_real = pars_eta[i],
                                   rho_real = pars_rho[i],
                                   sigma_real = pars_sigma[i]),
                       algorithm = c("Fixed_param"),
                       iter=10, chains=1, warmup=0)
}



#Extract samples--------------------------------------------------------------------

prior <- list()

for (i in 1:6) {
  
  prior[[i]] <- extract.samples(sim[[i]])
  
}


#Plot simulations------------------------------------------------------------------------

#select simulations to plot
no_sim <- c(4, 1, 2, rep(4, times=4), 1, 4)

#plot covariance for hypo simulations
png(file="./figure2_gp_simple/hypogp.png",
    width=1548,
    height=740
)

par(mfrow=c(2,3),
    lwd=2,
    cex=2,
    cex.lab=0.9,
    cex.main=1.2,
    mar=c(4,4,2,2),
    mgp = c(1.5, 0.5, 0),
    omi=c(0.05,0.05,0.05,0.05))


for (i in 1:6) {
  
  plot(
    NULL,
    xlim = c(0, 400),
    ylim = c(0, 1),
    xlab = "Continuous dimension",
    ylab = "Proportion"
  )
  
  
  #plot five simulations
  
  for (j in 1:5) {
    lines(seq(
      from = 0,
      to = 400,
      length.out = 500
    ),
    inv_logit(prior[[i]]$y_sim[j, ]),
    col = "grey")
  }
  

  #highlight one simulation
  lines(
    seq(
      from = 0,
      to = 400,
      length.out = 500
    ),
    inv_logit(prior[[i]]$y_sim[no_sim[i], ]),
    col = "red",
    lwd = 3
  )
  
  title(bquote(paste(
    .(letters[i]),") " 
    ~ eta,"=", .(prior[[i]]$eta[1]),
     ~ rho,"=", .(prior[[i]]$rho[1]),
    ~ sigma, "=",.(prior[[i]]$sigma[1])
  )))
  
}

dev.off()

