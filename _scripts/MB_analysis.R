# clear workspace
library(rstan)
library(ggplot2)
library(hBayesDM)


f = bandit2arm_delta()
###


####################################################################
# posterior predictove check
y_mean = aggregate(rawdata$accuracy, list(rawdata$trialID), mean)[,2]

y_pred = extract(f$fit, pars='y_pred')$y_pred
dim(y_pred)  # [4000,10,80]
y_pred_mean_mcmc = 
dim(y_pred_mean_mcmc)  # [4000, 80]
y_pred_mean_HDI = apply(y_pred_mean_mcmc, 2, HDIofMCMC)

#
reversal = c(0, (rawdata$correct[1:79] != rawdata$correct[2:80]) * 1.0)


df = data.frame(Trial = 1:80,
                Data  = y_mean,
                Model = y_pred_mean,
                HDI_l = y_pred_mean_HDI[1,],
                HDI_h = y_pred_mean_HDI[2,],
                reversal = reversal)


### makeing the plot
myconfig <- theme_bw(base_size = 20) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank() )

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


