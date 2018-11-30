# performing model-free analysis

# Lei Zhang, UKE, Hamburg, DE
# lei.zhang@uke.de

# =============================================================================
#### Construct Data #### 
# =============================================================================
# clear workspace
library(rstan)
library(ggplot2)
library(hBayesDM)

load('_data/dataList.RData')

f_RL = bandit2arm_delta(data = '_data/rawdata.txt', niter = 2000, nwarmup = 1000, 
                     inc_postpred = T, ncore = 4, adapt_delta = .9)
f_fict = prl_fictitious(data = '_data/rawdata.txt', niter = 2000, nwarmup = 1000, 
                        inc_postpred = T, ncore = 1, adapt_delta = .8)


# =============================================================================
#### Running Stan #### 
# =============================================================================
#rstan_options(auto_write = TRUE)

# modelFile <- '_scripts/reinforcement_learning_ppc.stan'
# 
# nIter     <- 200
# nChains   <- 2 
# #nWarmup   <- floor(nIter/2)
# nWarmup   <- 100
# 
# cat("Estimating", modelFile, "model... \n")
# startTime = Sys.time(); print(startTime)
# cat("Calling", nChains, "simulations in Stan... \n")
# 
# fit_rl <- stan(file    = modelFile, 
#                data    = dataList, 
#                chains  = nChains,
#                iter    = nIter,
#                warmup  = nWarmup,
#                #algorithm = "NUTS",
#                init    = "random",
#                cores   = 2 
# )
# 
# cat("Finishing", modelFile, "model simulation ... \n")
# endTime = Sys.time(); print(endTime)  
# cat("It took",as.character.Date(endTime - startTime), "\n")

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
load('_data/rawdata.RData')

# load model object
f = readRDS('_outputs/RL_stanfit.RData')

####################################################################
# overall mean

# trial-by-trial sequence
y_mean = aggregate(rawdata$accuracy, list(rawdata$trialID), mean)[,2]

y_pred = extract(f$fit, pars='y_pred')$y_pred
dim(y_pred)  # [4000,10,80]
y_pred_mean_mcmc = apply(y_pred==1, c(1,3), mean)
dim(y_pred_mean_mcmc)  # [4000, 80]
y_pred_mean = colMeans(y_pred_mean_mcmc)
y_pred_mean_HDI = apply(y_pred_mean_mcmc, 2, HDIofMCMC)

# plot
myconfig <- theme_bw(base_size = 20) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank() )

reversal = c(0, (rawdata$correct[1:79] != rawdata$correct[2:80]) * 1.0)
correct = rawdata$correct[1:80]
y_pred_mean[correct == 2] = 1 - y_pred_mean[correct == 2]
y_pred_mean_HDI[1,correct == 2] = 1 - y_pred_mean_HDI[1,correct == 2]
y_pred_mean_HDI[2,correct == 2] = 1 - y_pred_mean_HDI[2,correct == 2]

df = data.frame(Trial = 1:80,
                Data  = y_mean,
                Model = y_pred_mean,
                HDI_l = y_pred_mean_HDI[1,],
                HDI_h = y_pred_mean_HDI[2,],
                reversal = reversal)

## time course of the choice
g1 = ggplot(df, aes(Trial,Data))
g1 = g1 + geom_line(size = 1.5, aes(color= 'Data')) + geom_point(size = 2, shape = 21, fill='skyblue3',color= 'skyblue3')
#g1 = g1 + geom_ribbon(aes(ymin=HDI_l, ymax=HDI_h), linetype=2, alpha=0.3, fill = 'skyblue3')
g1 = g1 + geom_ribbon(aes(ymin=HDI_l, ymax=HDI_h, fill='Model'), linetype=2, alpha=0.3)
g1 = g1 + myconfig + scale_fill_manual(name = '',  values=c("Model" = "skyblue3")) +
    scale_color_manual(name = '',  values=c("Data" = "skyblue"))  +
    labs(y = 'Choosing correct (%)')
g1 = g1 + theme(axis.text   = element_text(size=22),
                axis.title  = element_text(size=25),
                legend.text = element_text(size=25))
g1 = g1+ geom_vline(xintercept=which(reversal==1), colour="red", linetype="longdash")
g1
#ggsave(plot = g1, "_plots/choice_seq_ppc.png", width = 8, height = 4, type = "cairo-png", units = "in")
