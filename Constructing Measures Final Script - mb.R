## Delivery 4 ## 

# Packages ----------------------------------------------------------------
library(readxl)
library(lavaan)
library(psych)
library(corrplot)
library(apaTables)
library(semPlot)
library(tidyverse)
library(ggplot2)



# Data Handling -----------------------------------------------------------
## Importing dataset
data <- read_excel("CEMO_2022_dataset.xls") 
## Removing engagement items 
data <- data[,1:17]
## Removing rows with 6 or more NAs
newdata <- data[rowSums(is.na(data[,8:17]))<6,]
colnames(newdata)[8:16] <- 
  c("teaching_01","teaching_02","teaching_03","teaching_04",
    "teaching_05","teaching_06","teaching_07","teaching_08","teaching_09")
## Dataset with only teaching 1:10
teaching <- newdata[8:17] 
## Recoding 9999 to na: 
newdata1 <- na_if(newdata, 9999)
teaching1 <- na_if(teaching, 9999)
## Removing all NAs (for after missing data section)
teaching1 <- na.omit(teaching1)
teaching1 <- teaching1[1:9]



# Descriptives ------------------------------------------------------------
#Describing core features of the sample
institution = data.frame(sort(table(newdata$institution,useNA = "ifany"),decreasing = T))
institution$Var1 = as.character(institution$Var1)
institution[13,] = c("others", sum(institution[-c(1:12),2]))
institution = institution[1:13,]

subject = data.frame(sort(table(newdata$subject_field,useNA = "ifany"),decreasing = T))
degree = data.frame(sort(table(newdata$degree_level,useNA = "ifany"),decreasing = T))
yearofstudy = data.frame(sort(table(newdata$yearofstudy,useNA = "ifany"),decreasing = T))
gender =data.frame(sort(table(newdata$gender,useNA = "ifany"),decreasing = T))
## Making table
# feature = rbind(institution,subject,degree,yearofstudy,gender)
# write_csv2(feature, "feature_table.csv")
hist(newdata$age,main="Histogram of age",xlab="age")



# Distrubution Figure -----------------------------------------------------
newdata_long = pivot_longer(newdata[8:17], 
                           cols = colnames(newdata[8:17]),
                           names_to = "items",
                           values_to = "response")
table(newdata_long,exclude=NULL)
newdata_long[newdata_long == 9999] = 6
newdata_long[is.na(newdata_long)] = 7
p = ggplot(newdata_long)+
  geom_bar(aes(response))+
  facet_wrap(vars(items),scales = "free")+
  theme_minimal()+
  labs(title = "Response Frequency per Item",
       y= "Frequencies",
       caption = "Scale: from 1 (Not satisfied) to 5 (Very satisfied), 9999 indicates not applicable and NA indicates missing", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        plot.caption = element_text(size = 14),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.line.x = element_line(),
        axis.line.y = element_line())+
  scale_x_discrete(limit = c("1","2","3","4","5","6","7"),
                   labels = c("1","2","3","4","5","9999","NA"))

p
## Table 
table(newdata_long$items, newdata_long$response)

## 
newdata[newdata == "9999"] = NA
N = apply(newdata[8:17], 2, function(x) length(which(!is.na(x))))
Mean = round(apply(newdata[8:17], 2, function(x) mean(x,na.rm=TRUE)),2)
SD = round(apply(newdata[8:17], 2, function(x) sd(x,na.rm=TRUE)),2)
Median = apply(newdata[8:17], 2, function(x) median(x,na.rm=TRUE))
Skewness = round(apply(newdata[8:17], 2, function(x) skew(x,na.rm=TRUE)),2)
Kurtosis = round(apply(newdata[8:17], 2, function(x) kurtosi(x,na.rm=TRUE)),2)

# Descriptive Table -------------------------------------------------------
Correlation = list()
for (i in 1:10){
  Correlation[[i]] = cor.test(newdata[[i+7]],
                              apply(newdata[8:17], 1, sum) - newdata[[i+7]],
                              use="pairwise.complete.obs",
                              method="p")
}
Correlation = c(Correlation[[1]]$estimate,Correlation[[2]]$estimate,Correlation[[3]]$estimate,Correlation[[4]]$estimate,Correlation[[5]]$estimate,Correlation[[6]]$estimate,Correlation[[7]]$estimate,Correlation[[8]]$estimate,Correlation[[9]]$estimate,Correlation[[10]]$estimate)
Correlation = round(Correlation,2)
desc_table = rbind(N,Mean,SD,Median,Skewness,Kurtosis,Correlation)
#write.csv2(desc_table, "desc_table.csv")



# Missing Data ------------------------------------------------------------------
## dataset with only 9999 answers 
teaching9999 <- teaching%>% filter_all(any_vars(.== 9999)) ##77 rows with 1 or more 9999
teaching9999[teaching9999 == "9999"] <- NA  
infotable <- apply(teaching9999, 2, table, exclude = NULL)
#write.csv2(infotable, "infotable.csv")
## Average column 
teaching9999$average <- rowMeans(teaching9999, na.rm = TRUE)
## Dataset without 9999 and average column 
teachingNA <- teaching
teachingNA[teachingNA == "9999"] <- NA
teachingNA <- na.omit(teachingNA)
teachingNA$average <- rowMeans(teachingNA) 
## T.test 
t.test(teaching9999$average, teachingNA$average)

## Checking info of 9999 answers 
## Dataset with 9999 answers and info columns: 
teaching99991 <- newdata %>% filter_all(any_vars(.== 9999)) ##Looks random. 



# Corr Table ----------------------------------------------------------------
##correlation matrix
cor_table = cor(teaching1, use = "pairwise.complete.obs")
corrplot(cor_table, method = "number", order = "hclust") #borderline cor=0.3 #not too high, too high means redundancy
#correlatoin table looks good; item 789 are highly correlated
apa.cor.table(teaching1,filename = "cor_table_apa.docx")

##parallel analysis
fa.parallel(teaching1[1:9])



# CFA ---------------------------------------------------------------------
## Model 1: unidimensional full model 1:9
uni_full = "TQ =~ teaching_01+teaching_02+teaching_03+teaching_04+
teaching_05+teaching_06+teaching_07+teaching_08+
teaching_09"
model1 = cfa(uni_full, 
             data = teaching1,
             std.lv = TRUE)
fitmeasures(model1,c("npar","df","gfi","tli","rmsea","srmr","chisq","aic"))
## gfi 0.899 tli 0.880 rmsea 0.123 srmr 0.050
summary(model1, standardized = TRUE, fit.measures = TRUE)

lavResiduals(model1)

## Reliability for this modeL
psych::alpha(teaching1[1:9])#0.9
## Omega 
my_coef <- c(coef(model1))
loadings <- my_coef[1:9]
unique_var <- my_coef[10:18]

my_omega <- sum(loadings)^2/(sum(loadings)^2+sum(unique_var))


## Plot 
semPaths(model1, what = "paths",#paths turns gray the lines
         esize = 1, edge.label.cex=1.5, "std")



# Model 2: Bifactor, Items 1:9 --------------------------------------------
bimodel = "TQ1 =~ teaching_01+teaching_02+teaching_03+teaching_04+
teaching_05+teaching_06
C =~ teaching_07+teaching_08+teaching_09"

model2 = cfa(bimodel, data = teaching1, std.lv = TRUE, estimator = "ML")
fitmeasures(model2,c("npar","df","gfi","tli","rmsea","srmr","chisq","aic"))
## gfi 0.963 rmsea 0.071 srmr 0.031
summary(model2, standardized = TRUE, fit.measures = TRUE)

lavResiduals(model2)



# Model 3: Tridimensional Model, Items 1:9 --------------------------------
trimodel = "OD =~ teaching_01+teaching_02+teaching_04
TA =~ teaching_03+teaching_05+teaching_06
C =~ teaching_07+teaching_08+teaching_09"
## OD for "other dimensions"

model3 = cfa(trimodel, data = teaching1, std.lv = TRUE, estimator = "ML")
fitmeasures(model3,c("npar","df","gfi","tli","rmsea","srmr","chisq","aic"))
## gfi 0.965 rmsea 0.072   srmr 0.031
summary(model3, standardized = TRUE, fit.measures = TRUE)

lavResiduals(model3)



# Model 4: Items 1:6,8 ----------------------------------------------------
new = "TQ =~ teaching_01+teaching_02+teaching_03+teaching_04+
teaching_05+teaching_06+teaching_08"

model4 = cfa(new, data = teaching1, std.lv = TRUE, estimator = "ML")
fitmeasures(model4,c("npar","df","gfi","tli","rmsea","srmr","chisq","aic"))
## gfi 0.975 rmsea 0.069 srmr 0.029 
summary(model4, standardized = TRUE, fit.measures = TRUE)

lavResiduals(model4)



# Model 5: Items 1,3,4,6,8 ------------------------------------------------
new = "TQ =~ teaching_01+teaching_03+teaching_04+
teaching_06+teaching_08"

model5 = cfa(new, 
             data = teaching1,
             std.lv = TRUE)
fitmeasures(model5,c("npar","df","gfi","tli","rmsea","srmr","chisq","aic"))
## gfi 0.991 rmsea 0.057 srmr 0.017
summary(model5, standardized = TRUE, fit.measures = TRUE)

lavResiduals(model5)

## Plots
semPaths(model5, what = "paths",
         esize = 1, edge.label.cex=1.5, "std")

## Reliability
psych::alpha(teaching1[,c(1,3,4,6,8)])#0.86

## Omega model 5
my_coef2 <- c(coef(model5))
loadings2 <- my_coef2[1:5]
unique_var2 <- my_coef2[6:10]

my_omega2 <- sum(loadings2)^2/(sum(loadings2)^2+sum(unique_var2))

