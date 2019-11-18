install.packages(c("car","ggplot2","reshape", "tidyverse", "hrbrthemes", "viridis", "plotly"),repos="https://cloud.r-project.org/")

library(car)
library(ggplot2)
library(reshape)
options(repr.plot.width=11, repr.plot.height=6)

# setwd("C:/Users/acarrillo30/Downloads/Archivo/Proy_grad/Analysys")
setwd("C:/Users/Alexis/Dropbox/MAESTRIA UCATOLICA/Proyecto Grado/Analysys")
####
data=read.csv2("final reults.csv",row.names=1)
data=data[!((rownames(data)=="EM_45c")|(rownames(data)=="LF_18a")|(rownames(data)=="LF_31b")),]


data_2=data[!data$process_2=="undefined",]
data_6=data[!data$process_6=="undefined",]

data$process_6=factor(data$process_6, levels=c("undefined","Recordar","Comprender","Aplicar","Analizar","Evaluar","Crear"))
data$process_2=factor(data$process_2, levels=c("undefined","Retener","Transferir"))

data_6$process_6=factor(data_6$process_6, levels=c("Recordar","Comprender","Aplicar","Analizar","Evaluar","Crear"))
data_2$process_2=factor(data_2$process_2, levels=c("Retener","Transferir"))

####
colnames(data)
summary(data)

plot(density(data$KNN),col="black", lwd=2, lty=1, main="Densidades del desempeño")
lines(density(data$RBF_SVM),col="black", lwd=2, lty=2)
lines(density(data$Rand_Forest),col="black", lwd=2, lty=3)
lines(density(data$AdaBoost),col="black", lwd=2, lty=4)
lines(density(data$Naive_Bayes),col="black", lwd=2, lty=5)
lines(density(data$QDA),col="black", lwd=2, lty=6)

data_clasifs=data[,c("KNN","RBF_SVM","Rand_Forest","AdaBoost","Naive_Bayes","QDA")]

boxplot(data_clasifs, 
        col="gray85",
        ylab="Índice F1",
        main="Gráfico de cajas del nivel de desempeño de los clasificadores")

####
data_clasifs_melt=melt(data_clasifs)
colnames(data_clasifs_melt)=c("Clasificador","Desempeño")
data_clasifs_melt$Clasificador=factor(data_clasifs_melt$Clasificador, levels=c("KNN","RBF_SVM","Rand_Forest","AdaBoost","Naive_Bayes","QDA"))

p <- ggplot(data_clasifs_melt, aes(Clasificador,Desempeño))
p + geom_violin()
# p + geom_violin(scale = "count")

library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plotly)


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
plot(data$RBF_SVM, data$KNN, pch=16, col=data$process_6)


class(data$Process)

t.test(IRT_difficulty ~ process_2, data=data_2)
t.test(KNN ~ process_2, data=data_2)
t.test(RBF_SVM ~ process_2, data=data_2)
t.test(Rand_Forest ~ process_2, data=data_2)
t.test(AdaBoost ~ process_2, data=data_2)
t.test(Naive_Bayes ~ process_2, data=data_2)
t.test(QDA ~ process_2, data=data_2)

cor.test(data$IRT_difficulty, data$KNN)
cor.test(data$IRT_difficulty, data$RBF_SVM)
cor.test(data$IRT_difficulty, data$Rand_Forest)
cor.test(data$IRT_difficulty, data$AdaBoost)
cor.test(data$IRT_difficulty, data$Naive_Bayes)
cor.test(data$IRT_difficulty, data$QDA)

plot(data$IRT_difficulty, data$KNN, pch=(data_2$process_2=="remember")*16+4, cex=1.5)
plot(data$IRT_difficulty, data$RBF_SVM, pch=(data_2$process_2=="remember")*16+4, cex=1.5)
plot(data$IRT_difficulty, data$Rand_Forest, pch=(data_2$process_2=="remember")*16+4, cex=1.5)
plot(data$IRT_difficulty, data$AdaBoost, pch=(data_2$process_2=="remember")*16+4, cex=1.5)
plot(data$IRT_difficulty, data$Naive_Bayes, pch=(data_2$process_2=="remember")*16+4, cex=1.5)
plot(data$IRT_difficulty, data$QDA, pch=(data_2$process_2=="remember")*16+4, cex=1.5)

plot(data$IRT_difficulty, data$KNN, pch=16, col=data$process_2)
points(data$IRT_difficulty, data$RBF_SVM, pch=16, col=data$process_2)
points(data$IRT_difficulty, data$Rand_Forest, pch=16, col=data$process_2)
points(data$IRT_difficulty, data$AdaBoost, pch=16, col=data$process_2)
points(data$IRT_difficulty, data$Naive_Bayes, pch=16, col=data$process_2)
points(data$IRT_difficulty, data$QDA, pch=16, col=data$process_2)

plot(data$IRT_difficulty, 
	rowMeans(data[,c("KNN","RBF_SVM","Rand_Forest","AdaBoost","Naive_Bayes","QDA")]),
	pch=16, col=data$process_2)

hist(data$IRT_difficulty, 
	col="gray75", 
	freq=FALSE,
	breaks=100,
	main="Hist Rash diff")
lines(density(data$IRT_difficulty))

hist(data$KNN, 
	col="gray75", 
	freq=FALSE,
	breaks=100,
	main="Hist Rash diff")
lines(density(data$KNN))

hist(data$RBF_SVM, 
	col="gray75", 
	freq=FALSE,
	breaks=100,
	main="Hist Rash diff")
lines(density(data$RBF_SVM))

barplot(summary(data$process_2))
barplot(summary(data$process_6))

leveneTest(weight ~ group, data = my_data)

### ANOVAS 6 grupos
http://www.sthda.com/english/wiki/one-way-anova-test-in-r

barplot(summary(data$process_6))
barplot(summary(data_6$process_6))

boxplot(KNN ~ process_6, data = data_6, col="gray85")
leveneTest(KNN ~ process_6, data = data_6)
aov_6_KNN <- aov(KNN ~ process_6, data = data_6)
summary(aov_6_KNN)
plot(aov_6_KNN, 1)
plot(aov_6_KNN, 2) 

boxplot(RBF_SVM~ process_6, data = data_6, col="gray85")
leveneTest(RBF_SVM~ process_6, data = data_6)
aov_6_RBF_SVM<- aov(RBF_SVM~ process_6, data = data_6)
summary(aov_6_RBF_SVM)
plot(aov_6_RBF_SVM, 1)
plot(aov_6_RBF_SVM, 2) 


boxplot(Rand_Forest~ process_6, data = data_6, col="gray85")
leveneTest(Rand_Forest~ process_6, data = data_6)
aov_6_Rand_Forest<- aov(Rand_Forest~ process_6, data = data_6)
summary(aov_6_Rand_Forest)
plot(aov_6_Rand_Forest, 1)
plot(aov_6_Rand_Forest, 2) 

boxplot(AdaBoost~ process_6, data = data_6, col="gray85")
leveneTest(AdaBoost~ process_6, data = data_6)
aov_6_AdaBoost<- aov(AdaBoost~ process_6, data = data_6)
summary(aov_6_AdaBoost)
plot(AdaBoost, 1)
plot(AdaBoost, 2) 


boxplot(Naive_Bayes~ process_6, data = data_6, col="gray85")
leveneTest(Naive_Bayes~ process_6, data = data_6)
aov_6_Naive_Bayes<- aov(Naive_Bayes~ process_6, data = data_6)
summary(aov_6_Naive_Bayes)
plot(AdaBoost, 1)
plot(AdaBoost, 2) 


boxplot(QDA~ process_6, data = data_6, col="gray85")
leveneTest(QDA~ process_6, data = data_6)
aov_6_QDA<- aov(QDA~ process_6, data = data_6)
summary(aov_6_QDA)
