
# PART 0 Packages and functions -------------------------------------------
install.packages("haven")
install.packages("lavaan")
install.packages("tidyverse")
install.packages("ggplot2")
library(haven) 
library(tidyverse) 
library(lavaan) 
library(ggplot2)

# Fit measures function
fit.m <- function(model) {
  fit_indices <- fitMeasures(model, c("CFI", "TLI", "RMSEA", "SRMR"), output = "matrix")
  fit_indices <- round(fit_indices, digits = 3)
  fit_indices2 <- data.frame(fit_indices)
  return(t(fit_indices2))
}

# PART 1 data management --------------------------------------------------------------------
SWE.DATA <- read_sav("Student data_master project.sav")
PILOT.DATA <- read_sav("SCQ_SPACS_DEMO_studentprosjekt.sav")
NOR.DATA <- read_sav("hoveddata_elev_analysefil_26.05.21.sav")

## NOR HOVED ####
# Removing unwanted columns 
NOR.DATA <- select(NOR.DATA, kjonn, bae08:bae16)

# Renaming  
NOR.DATA <- NOR.DATA %>%
  rename(KAP2 = bae08, KAP3 = bae09, KAP4 = bae10, COI1 = bae11, COI2 = bae12, COI3 = bae13, WTA1 = bae14, WTA2 = bae15, WTA3 = bae16)

## PILOT #### 
# Removing unwanted columns 
PILOT.DATA <- select(PILOT.DATA, kjonn, bae001:bae012)

# Renaming  
PILOT.DATA <- PILOT.DATA %>%
  rename(KAP1 = bae001, KAP2 = bae002, KAP3 = bae003, KAP4 = bae004, COI1 = bae005, COI2 = bae006, COI3 = bae007, COI4 = bae008, WTA1 = bae009, WTA2 = bae010, WTA3 = bae011, WTA4 = bae012)

# Removing NA 
PILOT.DATA <- PILOT.DATA[complete.cases(PILOT.DATA[, c("KAP1","KAP2", "KAP3", "KAP4", "COI1", "COI2", "COI3", "COI4", "WTA1", "WTA2", "WTA3", "WTA4")]), ]

## SWE #### 
# Removing unwanted columns 
SWE.DATA <- select(SWE.DATA, Gender, KAP1:WTA4)

## COMBINDING ####
NOR.DATA$KAP1 <- NA
NOR.DATA$WTA4 <- NA
NOR.DATA$COI4 <- NA
NOR.DATA$source <- "NOR"
PILOT.DATA$source <- "PILOT"
SWE.DATA$source <- "SWE"

# Rename kjonn and Gender to Gender in each data frame
names(NOR.DATA)[names(NOR.DATA) == "kjonn"] <- "gender"
names(PILOT.DATA)[names(PILOT.DATA) == "kjonn"] <- "gender"
names(SWE.DATA)[names(SWE.DATA) == "Gender"] <- "gender"

# Combine the data frames into one
Combined.DATA <- rbind(NOR.DATA, PILOT.DATA, SWE.DATA)
Combined.DATA <- select(Combined.DATA, gender, KAP1, KAP2, KAP3, KAP4, COI1, COI2, COI3, COI4, WTA1, WTA2, WTA3, WTA4, source)

# Write data file
write.csv2(Combined.DATA, "SPACSQ.csv", row.names = FALSE)

## Final data ####
data <- read.csv2("SPACSQ.csv")

## subsets ####
cols <- grep("KAP2|KAP3|KAP4|COI1|COI2|COI2|COI3|WTA1|WTA2|WTA3|source", names(data), value = TRUE)
cols2 <- grep("^KAP|^COI|^WTA", names(data), value = TRUE)
subset.nor.swe <- data[data$source == "NOR"| data$source == "SWE",]
subset.nor.swe.12 <- data[data$source == "NOR"| data$source == "SWE",cols]
subset.nor <- data[data$source == "NOR", cols]
subset.swe <- data[data$source == "SWE", cols]
subset.swe.12 <- data[data$source == "SWE", cols2]
subset.pilot <- data[data$source == "PILOT", cols2]

# PART 2 model structures --------------------------------------------------------
### Three sub-factors correlated ####
M <-     "      KAP =~ KAP2 + KAP3 + KAP4 
                COI =~ COI1 + COI2 + COI3 
                WTA =~ WTA1 + WTA2 + WTA3"

### One factor ####
M.ONE <- "SPACS =~ KAP2 + KAP3 + KAP4 + COI1 + COI2 + COI3 + WTA1 + WTA2 + WTA3"

### One factor for each sub-factor ####
KAP.FULL <- "KAP =~ KAP1 + KAP2 + KAP3 + KAP4" 
KAP.9 <-    "KAP =~ KAP2 + KAP3 + KAP4"
COI.FULL <- "COI =~ COI1 + COI2 + COI3 + COI4"
COI.9 <-    "COI =~ COI1 + COI2 + COI3" 
WTA.FULL <- "WTA =~ WTA1 + WTA2 + WTA3 + WTA4"
WTA.9 <-    "WTA =~ WTA1 + WTA2 + WTA3"



# PART 3 cfa models --------------------------------------------------------------
### One Factor Nine Items ####
SQ.ONE.SWE <- cfa(M.ONE, subset.swe, estimator = "MLR") 

SQ.ONE.NOR <- cfa(M.ONE, subset.nor, estimator = "MLR") 

### Individual one-factor models, 12 items ####
# SWE
#KAP 
SWE.KAP.FULL <- cfa(KAP.FULL, subset.swe.12, estimator = "MLR")
#COI 
SWE.COI.FULL <- cfa(COI.FULL, subset.swe.12, estimator = "MLR")
#WTA 
SWE.WTA.FULL <- cfa(WTA.FULL, subset.swe.12, estimator = "MLR")

# NOR
#KAP 
NOR.KAP.FULL <- cfa(KAP.FULL, subset.pilot, estimator = "MLR")
#COI 
NOR.COI.FULL <- cfa(COI.FULL, subset.pilot, estimator = "MLR")
#WTA 
NOR.WTA.FULL <- cfa(WTA.FULL, subset.pilot, estimator = "MLR")

# Modification indices single factor models
# Swedish data
SWE.WTA.EPC <- modindices(SWE.WTA.FULL, sort = TRUE, maximum.number = 10)

WTA.M.2 <- "WTA=~ WTA1 + WTA2 + WTA3 + WTA4
            WTA3 ~~ WTA4" 

SWE.WTA.FULL.2 <- cfa(WTA.M.2, subset.swe.12, estimator = "MLR")
fit.m(SWE.WTA.FULL.2)

# Pilot data 
NOR.WTA.EPC <- modindices(NOR.WTA.FULL, sort = TRUE)

NOR.WTA.FULL.2 <- cfa(WTA.M.2, subset.pilot, estimator = "MLR")
fit.m(NOR.WTA.FULL.2)

NOR.KAP.EPC <- modindices(NOR.KAP.FULL, sort = TRUE)

KAP.M.2 <- "KAP =~ KAP1 + KAP2 + KAP3 + KAP4 
            KAP1 ~~ KAP3"

NOR.KAP.FULL.2 <- cfa(KAP.M.2, subset.pilot, estimator = "MLR")
fit.m(NOR.KAP.FULL.2)

### Three sub-factors correlated models ####
SQ.FULL <- cfa(M, subset.nor.swe, estimator = "MLR") 
SQ.SWE <- cfa(M, subset.swe, estimator = "MLR") 
SQ.NOR <- cfa(M, subset.nor, estimator = "MLR") 

# PART 4 measurement invariance --------------------------------------------------
CONFIG <- cfa(model = M, subset.nor.swe, estimator = "MLR", group = "source")
fit.m(CONFIG)

METRIC <- cfa(model = M, subset.nor.swe, estimator = "MLR", group = "source", group.equal = c("loadings"))
fit.m(METRIC)

SCALAR <- cfa(M, subset.nor.swe, estimator = "MLR", group = "source", group.equal = c("loadings","intercepts"))
fit.m(SCALAR)

# PART 5 plots and tables ------------------------------------------------------------------
### plotting item distribution ####
data.subset <- data[data$source == "NOR"| data$source == "SWE",cols]
data.subset$source <- factor(data.subset$source, levels = c("SWE","NOR"))
data.melted <- reshape2::melt(data.subset, id.vars = "source")

ggplot(data.melted, aes(x = value, fill = factor(source))) +
  geom_histogram(aes(y = after_stat(density)), bins = 5, position = "dodge", alpha = 0.5) +
  facet_wrap(~variable, scales = "free_x") +
  labs(x = NULL, y = NULL, fill = NULL) +
  scale_fill_manual(values = c("#ffa600", "#003f5c"), name = NULL) + 
  theme(panel.background = element_rect(fill = "white"),
        axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(size = 16, face = "bold"),
        strip.text = element_text(size = 14, face = "bold"))

### Descriptive Statistics ####
# NOR
# Mean, Standard Deviation and Skewness 
mean.data.nor <- subset.nor %>% select(all_of(cols)) %>% 
  summarise(across(everything(), mean))
sd.data.nor <- subset.nor %>% select(all_of(cols)) %>% 
  summarise(across(everything(), sd))
skewness.data.nor <- subset.nor %>% select(all_of(cols)) %>% 
  summarise(across(everything(), psych::skew))

# Creating table
nor.desc <- mean.data.nor %>% pivot_longer(cols = everything(), names_to = "Item", values_to = "Mean") %>% 
  left_join(sd.data.nor %>% pivot_longer(cols = everything(), names_to = "Item", values_to = "SD"), by = c("Item")) %>% 
  left_join(skewness.data.nor %>% pivot_longer(cols = everything(), names_to = "Item", values_to = "Skewness"), by = c("Item"))
nor.desc <- nor.desc %>% mutate_at(vars(Mean, SD, Skewness), ~ round(., 2))
write.table(nor.desc, "nor.desc.csv", quote = F, sep = ";", col.names=NA)

# SWE
# Mean, Standard Deviation and Skewness 
mean.data.swe <- subset.swe %>% select(cols) %>% 
  summarise(across(everything(), mean))
sd.data.swe <- subset.swe %>% select(cols) %>% 
  summarise(across(everything(), sd))
skewness.data.swe <- subset.swe %>% select(cols) %>% 
  summarise(across(everything(), psych::skew))

# Creating table
swe.desc <- mean.data.swe %>% pivot_longer(cols = everything(), names_to = "Item", values_to = "Mean") %>% 
  left_join(sd.data.swe %>% pivot_longer(cols = everything(), names_to = "Item", values_to = "SD"), by = c("Item")) %>% 
  left_join(skewness.data.swe %>% pivot_longer(cols = everything(), names_to = "Item", values_to = "Skewness"), by = c("Item"))
swe.desc <- swe.desc %>% mutate_at(vars(Mean, SD, Skewness), ~ round(., 2))
write.table(swe.desc, "swe.desc.csv", quote = F, sep = ";", col.names=NA)

### Correlation Table ####
correlation.nor <- round(cor(subset.nor), 2)
rownames(correlation.nor) <- colnames(correlation.nor)
write.table(correlation.nor, "correlation.nor.csv", quote = F, sep = ";", col.names=NA)

correlation.swe <- round(cor(subset.swe), 2)
rownames(correlation.swe) <- colnames(correlation.swe)
write.table(correlation.swe, "correlation.swe.csv", quote = F, sep = ";", col.names=NA)

### Fit measures, one factor models #### 
SWE.ONEFACTOR.TABLE <- data.frame(
  Model = c("SWE.KAP.FULL", "SWE.COI.FULL", "SWE.WTA.FULL"),
  CFI = round(c(fitMeasures(SWE.KAP.FULL, "CFI"), fitMeasures(SWE.COI.FULL, "CFI"), fitMeasures(SWE.WTA.FULL, "CFI")),3),
  TLI = round(c(fitMeasures(SWE.KAP.FULL, "TLI"), fitMeasures(SWE.COI.FULL, "TLI"), fitMeasures(SWE.WTA.FULL, "TLI")),3),
  RMSEA = round(c(fitMeasures(SWE.KAP.FULL, "RMSEA"), fitMeasures(SWE.COI.FULL, "RMSEA"), fitMeasures(SWE.WTA.FULL, "RMSEA")),3),
  SRMR = round(c(fitMeasures(SWE.KAP.FULL, "SRMR"), fitMeasures(SWE.COI.FULL, "SRMR"), fitMeasures(SWE.WTA.FULL, "SRMR")),3)
)

NOR.ONEFACTOR.TABLE <- data.frame(
  Model = c("SWE.KAP.FULL", "SWE.COI.FULL", "SWE.WTA.FULL"),
  CFI = round(c(fitMeasures(NOR.KAP.FULL, "CFI"), fitMeasures(NOR.COI.FULL, "CFI"), fitMeasures(NOR.WTA.FULL, "CFI")),3),
  TLI = round(c(fitMeasures(NOR.KAP.FULL, "TLI"), fitMeasures(NOR.COI.FULL, "TLI"), fitMeasures(NOR.WTA.FULL, "TLI")),3),
  RMSEA = round(c(fitMeasures(NOR.KAP.FULL, "RMSEA"), fitMeasures(NOR.COI.FULL, "RMSEA"), fitMeasures(NOR.WTA.FULL, "RMSEA")),3),
  SRMR = round(c(fitMeasures(NOR.KAP.FULL, "SRMR"), fitMeasures(NOR.COI.FULL, "SRMR"), fitMeasures(NOR.WTA.FULL, "SRMR")),3)
)
write.table(SWE.ONEFACTOR.TABLE, "SWE.ONEFACTOR.TABLE.csv", quote = F, sep = ";", col.names=NA)
write.table(NOR.ONEFACTOR.TABLE, "NOR.ONEFACTOR.TABLE.csv", quote = F, sep = ";", col.names=NA)

### Factor loadings table, three sub-factors model #### 
FL.TABLE <- inspect(SQ.FULL, what = "std")$lambda
FL.TABLE <- as.data.frame(FL.TABLE)
FL.TABLE <- round(FL.TABLE, 3)
write.table(FL.TABLE, "FL.TABLE.csv", quote = F, sep = ";", col.names=NA)

### Factor variance and correlations table #### 
# factor variance
COV.MATRIX <- lavInspect(SQ.FULL, "cov.lv")
# factor correlation
COR.MATRIX <- lavInspect(SQ.FULL, "cor.lv")

COMBINED.MATRIX <- diag(diag(COV.MATRIX))
COMBINED.MATRIX[lower.tri(COMBINED.MATRIX)] <- round(COR.MATRIX[lower.tri(COR.MATRIX)], 2)

COMBINED.MATRIX <- round(COMBINED.MATRIX, 3)
rownames(COMBINED.MATRIX) <- c("KAP","COI","WTA")
colnames(COMBINED.MATRIX) <- c("KAP","COI","WTA")

write.table(COMBINED.MATRIX, "COMBINED.MATRIX.csv", quote = F, sep = ";", col.names=NA)

### Measurement Invariance Table ####
mi.fit.table <- function(model, table) { 
  table <- round(fitMeasures(model, c("RMSEA", "CFI", "SRMR")),3)
  table <- as.data.frame(table)
  table <- tibble::rownames_to_column(table, "fit")
  table <- t(table)
  table <- as.data.frame(table)
  names(table) <- table %>% slice(1) %>% unlist()
  table <- table %>% slice(-1)
  table <- table %>% mutate_if(is.character, as.numeric)
  return(table)
}
mi.fit <- mi.fit.table(SQ.SWE)
mi.fit[2,] <- mi.fit.table(SQ.NOR)
mi.fit[3,] <- mi.fit.table(CONFIG)
mi.fit[4,] <- mi.fit.table(METRIC)
mi.fit[5,] <- mi.fit.table(SCALAR)

# Calculate the change in RMSEA, CFI, and SRMR
mi.fit$RMSEA_change <- c(NA, diff(mi.fit$rmsea))
mi.fit$CFI_change <- c(NA, diff(mi.fit$cfi))
mi.fit$SRMR_change <- c(NA, diff(mi.fit$srmr))

# Select and reorder the columns in the desired order
mi.fit <- mi.fit[, c("rmsea", "RMSEA_change", "cfi", "CFI_change", "srmr", "SRMR_change")]
rownames(mi.fit) <- c("Sweden","Norway","CONFIG", "METRIC", "SCALAR")

write.table(mi.fit, "mifit.csv", quote = F, sep = ";", col.names=NA)

## Modification indexes table 
SWE.WTA.EPC <- SWE.WTA.EPC[1:5]
SWE.WTA.EPC <- SWE.WTA.EPC %>% mutate_if(is.numeric, ~round(., 3))

NOR.WTA.EPC <- NOR.WTA.EPC[1:5]
NOR.WTA.EPC <- NOR.WTA.EPC %>% mutate_if(is.numeric, ~round(., 3))

NOR.KAP.EPC <- NOR.KAP.EPC[1:5]
NOR.KAP.EPC <- NOR.KAP.EPC %>% mutate_if(is.numeric, ~round(., 3))

write.table(SWE.WTA.EPC, "SWE.WTA.EPC.csv", quote = F, sep = ";", col.names=NA)
write.table(NOR.WTA.EPC, "NOR.WTA.EPC.csv", quote = F, sep = ";", col.names=NA)
write.table(NOR.KAP.EPC, "NOR.KAP.EPC.csv", quote = F, sep = ";", col.names=NA)