# setwd("C:/Users/acarrillo30/Downloads/Archivo/Proy_grad/Analysys")
# setwd("C:/Users/Alexis/Dropbox/MAESTRIA UCATOLICA/Proyecto Grado/Analysys")
dir()

R.Version()$version.string
#install.packages("psych", repos="https://cloud.r-project.org/")
library(psych)
#install.packages("eRm", repos="https://cloud.r-project.org/") #https://rdrr.io/rforge/eRm/
library(eRm)
#install.packages("reshape2", repos="https://cloud.r-project.org/") #https://rdrr.io/rforge/eRm/
library(reshape2)

train_stud_ans=read.csv("train_answers_items.csv", row.names=1)
str(train_stud_ans)

test_stud_ans=read.csv("test_answers_items.csv", row.names=1)
str(test_stud_ans)

diffs_train=irt.item.diff.rasch(train_stud_ans)
train_desc=data.frame(describe(train_stud_ans))
train_desc$IRT_difficulty=diffs_train
#write.csv(data.frame(train_desc),"train_desc.csv")

diffs_test=irt.item.diff.rasch(test_stud_ans)
test_desc=data.frame(describe(test_stud_ans))
test_desc$IRT_difficulty=diffs_test
#write.csv(data.frame(test_desc),"test_desc.csv")

summary(test_desc)
str(test_desc)
######################

train_califs=read.csv("train_califs.csv", sep=";",row.names=1)

# https://seananderson.ca/2013/10/19/reshape/
# https://www.r-bloggers.com/reshape-r-package-reshape2-melt-and-cast/

train_califs_cast=dcast(train_califs, std_code~quest_id, value.var="st_ans_calif")
rownames(train_califs_cast)=train_califs_cast$std_code
train_califs_cast=train_califs_cast[,2:length(train_califs_cast)]
head(train_califs_cast)
###

train_califs_cast=train_califs_cast[rowSums(!is.na(train_califs_cast))>1,]
train_califs_cast=train_califs_cast[,colSums(!is.na(train_califs_cast))>1]

dim(train_califs_cast)

##########################
##### Infit y Outfit #####
# https://rdrr.io/rforge/eRm/man/itemfit.ppar.html
# https://rdrr.io/rforge/eRm/man/RM.html
# https://cran.r-project.org/web/packages/eRm/index.html
# https://www.rasch.org/rmt/rmt162f.htm
# https://www.rasch.org/rmt/rmt82a.htm
# 

# Rasch model, estimation of item and person parameters
res <- RM(train_califs_cast)#raschdat2
p.res <- person.parameter(res)

# Matrix with expected probabilities and corresponding residuals
pmat(p.res)
residuals(p.res)

#Itemfit
itemfit(p.res)

#Personfit
personfit(p.res)