########## 6 - Boxplot Evaluation metrics of predictive accuracy ##########

library(ggplot2)
library(dplyr)
library(ggpubr)
library(rstatix)

rm(list = ls())

### Set the working directory to the path where you downloaded the "Example_Virtual_Species" folder

setwd("./Example_Virtual_Species/Models")

model = c("PA.10000.0In.10000Out",
          "PA.10000.5000In.5000Out",
          "PA.10000.10000In.0Out")  ### To fill with the model you want to compare for different ratio of PA in
                                    ### Here example with 10,000 PA, ratio in = 0, 0.5, 1

setwd(paste0("./Example_Virtual_Species/Models/", model[1]))

eval_0 = read.csv("Eval.csv")

eval_0 = eval_0[c(5,10),]
eval_0 = as.data.frame(t(eval_0))
eval_0 = eval_0[-c(1),]
colnames(eval_0) = c("Metric", "Validation")
eval_0$Metric[eval_0$Metric == "KAPPA"] = "KAPPA_0"
eval_0$Metric[eval_0$Metric == "TSS"] = "TSS_0"
eval_0$Metric[eval_0$Metric == "ROC"] = "ROC_0"



setwd("..")
setwd(paste0("./Example_Virtual_Species/Models/", model[2]))

eval_0.5 = read.csv("Eval.csv")

eval_0.5 = eval_0.5[c(5,10),]
eval_0.5 = as.data.frame(t(eval_0.5))
eval_0.5 = eval_0.5[-c(1),]
colnames(eval_0.5) = c("Metric", "Validation")
eval_0.5$Metric[eval_0.5$Metric == "KAPPA"] = "KAPPA_0.5"
eval_0.5$Metric[eval_0.5$Metric == "TSS"] = "TSS_0.5"
eval_0.5$Metric[eval_0.5$Metric == "ROC"] = "ROC_0.5"




setwd("..")
setwd(paste0("./Example_Virtual_Species/Models/", model[3]))

eval_1 = read.csv("Eval_250000PA_0In_250000Out.csv")

eval_1 = eval_1[c(5,10),]
eval_1 = as.data.frame(t(eval_1))
eval_1 = eval_1[-c(1),]
colnames(eval_1) = c("Metric", "Validation")
eval_1$Metric[eval_1$Metric == "KAPPA"] = "KAPPA_1"
eval_1$Metric[eval_1$Metric == "TSS"] = "TSS_1"
eval_1$Metric[eval_1$Metric == "ROC"] = "ROC_1"




eval_kappa_0.5 = eval_0.5 %>% filter(Metric == "KAPPA_EcoPA_0.5")
eval_kappa_0 = eval_0 %>% filter(Metric == "KAPPA_EcoPA_0")
eval_kappa_1 = eval_1 %>% filter(Metric == "KAPPA_EcoPA_1")

eval_tss_0.5 = eval_0.5 %>% filter(Metric == "TSS_EcoPA_0.5")
eval_tss_0 = eval_0 %>% filter(Metric == "TSS_EcoPA_0")
eval_tss_1 = eval_1 %>% filter(Metric == "TSS_EcoPA_1")

eval_roc_0.5 = eval_0.5 %>% filter(Metric == "ROC_EcoPA_0.5")
eval_roc_0 = eval_0 %>% filter(Metric == "ROC_EcoPA_0")
eval_roc_1 = eval_1 %>% filter(Metric == "ROC_EcoPA_1")



eval_kappa_0.5$Metric = as.factor(eval_kappa_0.5$Metric)
eval_kappa_0.5$Validation = as.numeric(eval_kappa_0.5$Validation)

eval_kappa_0$Metric = as.factor(eval_kappa_0$Metric)
eval_kappa_0$Validation = as.numeric(eval_kappa_0$Validation)

eval_kappa_1$Metric = as.factor(eval_kappa_1$Metric)
eval_kappa_1$Validation = as.numeric(eval_kappa_1$Validation)


eval_tss_0.5$Metric = as.factor(eval_tss_0.5$Metric)
eval_tss_0.5$Validation = as.numeric(eval_tss_0.5$Validation)

eval_tss_0$Metric = as.factor(eval_tss_0$Metric)
eval_tss_0$Validation = as.numeric(eval_tss_0$Validation)

eval_tss_1$Metric = as.factor(eval_tss_1$Metric)
eval_tss_1$Validation = as.numeric(eval_tss_1$Validation)


eval_roc_0.5$Metric = as.factor(eval_roc_0.5$Metric)
eval_roc_0.5$Validation = as.numeric(eval_roc_0.5$Validation)

eval_roc_0$Metric = as.factor(eval_roc_0$Metric)
eval_roc_0$Validation = as.numeric(eval_roc_0$Validation)

eval_roc_1$Metric = as.factor(eval_roc_1$Metric)
eval_roc_1$Validation = as.numeric(eval_roc_1$Validation)





eval_kappa = rbind(eval_kappa_1, eval_kappa_0.5, eval_kappa_0)
eval_tss = rbind(eval_tss_1, eval_tss_0.5, eval_tss_0)
eval_roc = rbind(eval_roc_1, eval_roc_0.5, eval_roc_0)

############## Test normality

model_kappa = lm(Validation ~ Metric, data = eval_kappa)
# qqplot of residuals
ggqqplot(residuals(model_kappa)) ## if normality residuals along the line


model_tss = lm(Validation ~ Metric, data = eval_tss)
# qqplot of residuals
ggqqplot(residuals(model_tss))


model_roc = lm(Validation ~ Metric, data = eval_roc)
# qqplot of residuals
ggqqplot(residuals(model_roc))

############## If you have normality

### ANOVA
stat_test_kappa = eval_kappa %>% anova_test(Validation ~ Metric) ##### If significantly different, test post-hoc
stat_test_tss = eval_tss %>% anova_test(Validation ~ Metric)
stat_test_roc = eval_roc %>% anova_test(Validation ~ Metric)


### Test post-hoc(Tuket_hsd)
post_hoc_kappa = eval_kappa  %>% tukey_hsd(Validation ~ Metric)
post_hoc_tss = eval_tss %>% tukey_hsd(Validation ~ Metric)
post_hoc_roc = eval_roc %>% tukey_hsd(Validation ~ Metric)


############## If you do not have normality

### Kruskal-Wallis
stat_test_kappa = eval_kappa %>% kruskal_test(Validation ~ Metric) ##### If significantly different, test post-hoc
stat_test_tss = eval_tss %>% kruskal_test(Validation ~ Metric)
stat_test_roc = eval_roc %>% kruskal_test(Validation ~ Metric)


### Test post-hoc (Tuket_hsd)
post_hoc_kappa = eval_kappa  %>% dunn_test(Validation ~ Metric)
post_hoc_tss = eval_tss %>% dunn_test(Validation ~ Metric)
post_hoc_roc = eval_roc %>% dunn_test(Validation ~ Metric)




setwd("..")

png("Eval_KAPPA.png", width = 3000, height = 5000, res = 600)
ggplot() +
  geom_boxplot(eval_kappa, mapping = aes(Metric, Validation, color = Metric)) +
  scale_color_manual(values = c("#b1dab2", "#a8ce73", "darkgreen")) +
  scale_y_continuous(limits = c(0.1, 1)) +
  stat_pvalue_manual(post_hoc_kappa, label = "p.adj.signif",
                     tip.length = -0.01,
                     y.position = c(0.18, 0.1, 0.4),
                     bracket.shorten = 0.05) +
  theme_bw() +
  theme(legend.position = "none")
dev.off()


png("Eval_TSS.png", width = 3000, height = 5000, res = 600)
ggplot() +
  geom_boxplot(eval_tss, mapping = aes(Metric, Validation, color = Metric)) +
  scale_color_manual(values = c("#b1dab2", "#a8ce73", "darkgreen")) +
  scale_y_continuous(limits = c(0.1, 1)) +
  stat_pvalue_manual(post_hoc_tss, label = "p.adj.signif",
                     tip.length = -0.01,
                     y.position = c(0.18, 0.1, 0.4),
                     bracket.shorten = 0.05) +
  theme_bw() +
  theme(legend.position = "none")
dev.off()


png("Eval_ROC.png", width = 3000, height = 5000, res = 600)
ggplot() +
  geom_boxplot(eval_roc, mapping = aes(Metric, Validation, color = Metric)) +
  scale_color_manual(values = c("#b1dab2", "#a8ce73", "darkgreen")) +
  scale_y_continuous(limits = c(0.1, 1)) +
  stat_pvalue_manual(post_hoc_roc, label = "p.adj.signif",
                     tip.length = -0.01,
                     y.position = c(0.63, 0.55, 0.78),
                     bracket.shorten = 0.05) +
  theme_bw() +
  theme(legend.position = "none")
dev.off()
