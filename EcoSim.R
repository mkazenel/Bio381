# EcoSimR
# Morgan Southgate
# 19 April 2017

# Null model analysis of ecological data

# Diamond (1975): 
# 1) Forbidden species combinations
# 2) Checkerboard pairs of species

# Connor and Simberloff (1975)
# Introduced null models
# presence-absence matrix -- rows are species, columns are sites

# Co-orrurence indices in EcoSimR
# Checker index
# C-score
# V-ratio
# Combo index

# Null model algorithms
# 3 constraints ^ 2 dimensions -> 9 basic null models
# Columns: equiprobably, proportional, fixed; rows: equiprobable, prop., fixed


# read in associated species data 
sppDat <- read.table("AssociatedSppData_Serp.csv",header=TRUE,sep=",",stringsAsFactors = FALSE)
head(sppDat)

# reshape data using dcast function in reshape2 package
library(reshape2)
PA <- dcast(sppDat,formula=SpeciesName~SitePatch)
head(PA)


library(EcoSimR)
library(MASS)
# Run null model with SIM9 algorithm & CHECKER index
adMod1 <- cooc_null_model(PA,algo= "sim9",metric="checker",nReps=1000,suppressProg=T)

# Summary and plots
summary(adMod1)

mean(adMod1$Sim)

plot(adMod1,type="hist")

plot(adMod1,type="cooc")

plot(adMod1,type="burn_in")


## Run null model with SIM9 algorithm and C score index
adMod2 <- cooc_null_model(PA,algo= "sim9",metric="c_score",nReps=1000)

# Summary and plots
summary(adMod2)
plot(adMod2,type="hist")
plot(adMod2,type="cooc")
plot(adMod2,type="burn_in")


# Run null model with SIM9 algorithm and COMBO index
adMod3 <- cooc_null_model(PA,algo= "sim9",metric="species_combo",nReps=1000)

# Summary and plots
summary(adMod3)
plot(adMod3,type="hist")
plot(adMod3,type="cooc")
