##
# performing model-free analysis
library(ggplot2)

load(file = '_data/rawdata.RData')
ns = 10
nt = 80

# averaged choice accuracy per trial across subject 
sem = function(x) (sd(x) / (length(x)-1))

acc_mean = aggregate(rawdata$accuracy, list(rawdata$trialID), mean)[,2]
acc_sem  = aggregate(rawdata$accuracy, list(rawdata$trialID), sem)[,2]

reversal = c(0, (rawdata$correct[1:79] != rawdata$correct[2:80]) * 1.0)


# plot
myconfig <- theme_bw(base_size = 20) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank() )

df = data.frame(Trial = 1:nt,
                Data  = acc_mean,
                sem_l = acc_mean - acc_sem,
                sem_h = acc_mean + acc_sem,
                reversal = reversal)

g1 = ggplot(df, aes(Trial,Data))
g1 = g1 + geom_line(size = 1.5, aes(color= 'Data')) + geom_point(size = 2, shape = 21, fill='skyblue3',color= 'skyblue3')
#g1 = g1 + geom_ribbon(aes(ymin=HDI_l, ymax=HDI_h), linetype=2, alpha=0.3, fill = 'skyblue3')
g1 = g1 + geom_ribbon(aes(ymin=sem_l, ymax=sem_h), linetype=2, alpha=0.3, fill='skyblue3')
g1 = g1 + myconfig + #scale_fill_manual(name = '',  values=c("Model" = "skyblue3")) +
    scale_color_manual(name = '',  values=c("Data" = "skyblue"))  +
    labs(y = 'Choosing correct (%)')
g1 = g1 + theme(axis.text   = element_text(size=22),
                axis.title  = element_text(size=25),
                legend.text = element_text(size=25))
g1 = g1+ geom_vline(xintercept=which(reversal==1), colour="red", linetype="longdash")
#ggsave(plot = g1, "_plots/choice_seq_ppc.png", width = 8, height = 4, type = "cairo-png", units = "in")


