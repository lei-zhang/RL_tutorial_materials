# performing model-free analysis

# Lei Zhang, UKE, Hamburg, DE
# lei.zhang@uke.de

# =============================================================================
#### Construct Data #### 
# =============================================================================
# clear workspace
library(rstan)
library(ggplot2)
library(R.matlab)
library(hBayesDM)

load('_data/dataList.RData')

f = bandit2arm_delta(data = '_data/rawdata.txt', niter = 2000, nwarmup = 1000, 
                     inc_postpred = T, ncore = 4, adapt_delta = .9)


# =============================================================================
#### Running Stan #### 
# =============================================================================
#rstan_options(auto_write = TRUE)

modelFile <- '_scripts/reinforcement_learning_ppc.stan'

nIter     <- 200
nChains   <- 2 
#nWarmup   <- floor(nIter/2)
nWarmup   <- 100

cat("Estimating", modelFile, "model... \n")
startTime = Sys.time(); print(startTime)
cat("Calling", nChains, "simulations in Stan... \n")

fit_rl <- stan(file    = modelFile, 
               data    = dataList, 
               chains  = nChains,
               iter    = nIter,
               warmup  = nWarmup,
               #algorithm = "NUTS",
               init    = "random",
               cores   = 2 
)

cat("Finishing", modelFile, "model simulation ... \n")
endTime = Sys.time(); print(endTime)  
cat("It took",as.character.Date(endTime - startTime), "\n")

# =============================================================================
#### Model Summary and Diagnostics #### 
# =============================================================================
#print(fit_rl)


# =============================================================================
#### ppc #### 
# =============================================================================
library(hBayesDM); library(rstan)
# load data
load('_data/dataList.RData')

# load model object
f = readRDS('_outputs/RL_stanfit.RData')

####################################################################
# overall mean

# trial-by-trial sequence
#plot(1:100, colMeans(dataList$choice == 1),type='b')
# y_mean = acc_mean
# 
# y_pred = extract(f$fit, pars='y_pred')$y_pred
# dim(y_pred)  # [4000,10,100]
# 
# y_pred_mean_mcmc = apply(y_pred==1, c(1,3), mean)
# dim(y_pred_mean_mcmc)  # [4000, 100]
# y_pred_mean = colMeans(y_pred_mean_mcmc)
# y_pred_mean_HDI = apply(y_pred_mean_mcmc, 2, HDIofMCMC)




