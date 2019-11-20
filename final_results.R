#install.packages(c("car","ggplot2","reshape", "tidyverse", "hrbrthemes", "viridis", "plotly"),repos="https://cloud.r-project.org/")

library(car)
library(ggplot2)
library(reshape)

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plotly)

options(repr.plot.width=11, repr.plot.height=6)

setwd("C:/Users/acarrillo30/Downloads/Archivo/Proy_grad/Analysys")
# setwd("C:/Users/Alexis/Dropbox/MAESTRIA UCATOLICA/Proyecto Grado/Analysys")
####
data=read.csv2("final reults.csv",row.names=1)
data=data[!((rownames(data)=="EM_45c")|(rownames(data)=="LF_18a")|(rownames(data)=="LF_31b")),]


data_2=data[!data$process_2=="undefined",]
data_6=data[!data$process_6=="undefined",]

data$process_6=factor(data$process_6, levels=c("undefined","Recordar","Comprender","Aplicar","Analizar","Evaluar","Crear"))
data$process_2=factor(data$process_2, levels=c("undefined","Retener","Transferir"))

data_6$process_6=factor(data_6$process_6, levels=c("Recordar","Comprender","Aplicar","Analizar","Evaluar","Crear"))
data_2$process_2=factor(data_2$process_2, levels=c("Retener","Transferir"))

colnames(data)
summary(data)

###################################
data_clasifs=data[,c("KNN","RBF_SVM","Rand_Forest","AdaBoost","Naive_Bayes","QDA")]
data_clasifs_melt=melt(data_clasifs)
colnames(data_clasifs_melt)=c("Clasificador","Desempeño")
data_clasifs_melt$Clasificador=factor(data_clasifs_melt$Clasificador, levels=c("KNN","RBF_SVM","Rand_Forest","AdaBoost","Naive_Bayes","QDA"))

# sample size
sample_size = data_clasifs_melt %>% group_by(Clasificador) %>% summarize(num=n())

# Plot
data_clasifs_melt %>%
  left_join(sample_size) %>%
#  mutate(myaxis = paste0(Clasificador, "\n", "n=", num)) %>%
  ggplot( aes(x=Clasificador, y=Desempeño)) +
    geom_violin(width=1, fill="gray65") + #width=1.4
    geom_boxplot(width=.2, color="gray25", alpha=0.5, fill="gray90") +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("Distribuciones del desempeño de los algoritmos") +
    xlab("")+
    ylab("Desempeño")
#ggsave(filename = "f1_clasifs.jpeg",device="jpeg", width =11, height = 6, units = "in")


##################################
cor.test(data$IRT_difficulty, data$KNN)
cor.test(data$IRT_difficulty, data$RBF_SVM)
cor.test(data$IRT_difficulty, data$Rand_Forest)
cor.test(data$IRT_difficulty, data$AdaBoost)
cor.test(data$IRT_difficulty, data$Naive_Bayes)
cor.test(data$IRT_difficulty, data$QDA)

##################################
t.test(IRT_difficulty ~ process_2, data=data_2)
t.test(KNN ~ process_2, data=data_2)
t.test(RBF_SVM ~ process_2, data=data_2)
t.test(Rand_Forest ~ process_2, data=data_2)
t.test(AdaBoost ~ process_2, data=data_2)
t.test(Naive_Bayes ~ process_2, data=data_2)
t.test(QDA ~ process_2, data=data_2)

##################################
#Two-way ANOVA

dat_AOV_2=melt(data_2,#[,c("process_2","KNN","RBF_SVM","Rand_Forest","AdaBoost","Naive_Bayes","QDA")],
             id.vars = c("process_2","IRT_difficulty"), 
             measure.vars = c("KNN","RBF_SVM","Rand_Forest","AdaBoost","Naive_Bayes","QDA"))
colnames(dat_AOV_2)=c("process_2","IRT_difficulty","Clasificador","Desempeño")
dat_AOV_2$Clasificador=factor(dat_AOV_2$Clasificador, levels=c("KNN","RBF_SVM","Rand_Forest","AdaBoost","Naive_Bayes","QDA"))

leveneTest(Desempeño ~ Clasificador * process_2, data = dat_AOV_2)
process_2_aov <- aov(Desempeño ~ Clasificador + process_2 + Clasificador:process_2, data = dat_AOV_2)
summary(process_2_aov)
TukeyHSD(process_2_aov,which =c("Clasificador","process_2"))
plot(process_2_aov, 1)
plot(process_2_aov, 2)

dat_AOV_6=melt(data_6,#[,c("process_6","KNN","RBF_SVM","Rand_Forest","AdaBoost","Naive_Bayes","QDA")],
             id.vars = c("process_6","IRT_difficulty"), 
             measure.vars = c("KNN","RBF_SVM","Rand_Forest","AdaBoost","Naive_Bayes","QDA"))
colnames(dat_AOV_6)=c("process_6","IRT_difficulty","Clasificador","Desempeño")
dat_AOV_6$Clasificador=factor(dat_AOV_6$Clasificador, levels=c("KNN","RBF_SVM","Rand_Forest","AdaBoost","Naive_Bayes","QDA"))

leveneTest(Desempeño ~ Clasificador * process_6, data = dat_AOV_6)
process_6_aov <- aov(Desempeño ~ Clasificador + process_6 + Clasificador:process_6, data = dat_AOV_6)
summary(process_6_aov)
TukeyHSD(process_6_aov,which ="Clasificador")
plot(process_6_aov, 1)
plot(process_6_aov, 2)

####
leveneTest(IRT_difficulty ~ Clasificador * process_2, data = dat_AOV_2)
process_2_aov_dif <- aov(IRT_difficulty ~ Clasificador + process_2 + Clasificador:process_2, data = dat_AOV_2)
summary(process_2_aov_dif)
TukeyHSD(process_2_aov_dif,which ="process_2")
plot(process_2_aov_dif, 1)
plot(process_2_aov_dif, 2)


leveneTest(IRT_difficulty ~ Clasificador * process_6, data = dat_AOV_6)
process_6_aov <- aov(IRT_difficulty ~ Clasificador + process_6 + Clasificador:process_6, data = dat_AOV_6)
summary(process_6_aov)
TukeyHSD(process_6_aov,which ="process_6")
plot(process_6_aov, 1)
plot(process_6_aov, 2)

################################



