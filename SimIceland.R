# PHOENIX - Micro simulation model
# SimIceland

# Loading data with directory
setwd("G:/My Drive/PHOENIX/WP3 Tangram design/Micro-simulation models for each pilot/Iceland")


#################### Constraint variables: Age Sex crosstab #################
age_sex.const <- read.csv("age_sex_iceland.csv", header=TRUE, sep=";") # Read the file with aggregate results for each municipality (n=18) regarding age and sex

head(age_sex.const) 

age_sex.msim <- array(dim=c(nrow(age_sex.const), 18))

for(i in 1:nrow(age_sex.const)) {
  #males
  m10_19 <- age_sex.const[i,]$m10_19
  m20_29 <- age_sex.const[i,]$m20_29
  m30_39 <- age_sex.const[i,]$m30_39
  m40_49 <- age_sex.const[i,]$m40_49
  m50_59 <- age_sex.const[i,]$m50_59
  m60_69 <- age_sex.const[i,]$m60_69
  m70_79 <- age_sex.const[i,]$m70_79
  m80_89 <- age_sex.const[i,]$m80_89
  m90_over <- age_sex.const[i,]$m90_over
  
  #females
  f10_19 <- age_sex.const[i,]$f10_19
  f20_29 <- age_sex.const[i,]$f20_29
  f30_39 <- age_sex.const[i,]$f30_39
  f40_49 <- age_sex.const[i,]$f40_49
  f50_59 <- age_sex.const[i,]$f50_59
  f60_69 <- age_sex.const[i,]$f60_69
  f70_79 <- age_sex.const[i,]$f70_79
  f80_89 <- age_sex.const[i,]$f80_89
  f90_over <- age_sex.const[i,]$f90_over
  
  age_sex.msim[i,] <- cbind(m10_19, m20_29, m30_39,	m40_49,	m50_59,	m60_69,	m70_79, m80_89, m90_over, f10_19, f20_29, f30_39,	f40_49,	f50_59,	f60_69,	f70_79, f80_89,	f90_over) 
} 

colnames(age_sex.msim) <- c("m10_19", "m20_29", "m30_39", "m40_49",	"m50_59", "m60_69",	"m70_79", "m80_89", "m90_over", "f10_19", "f20_29", "f30_39", "f40_49",	"f50_59", "f60_69",	"f70_79", "f80_89",	"f90_over")
head(age_sex.msim) #shows the first 6 rows for 18 sex and age categories in 8 regions
rowSums(age_sex.msim) #shows N inhabitants per region

####### Create unit matrix with linking variables age and sex from European social survey ##########
ESS_IS <- read.csv("ESS_age_sex_IS_1.csv", header=TRUE, sep = ";") #sex=1 male, sex=2 female

ESS_IS.cat <- array(0, dim=c(nrow(ESS_IS), ncol(age_sex.msim))) 
ESS_IS.cat[which(ESS_IS$age >= 10 & ESS_IS$age <= 19 & ESS_IS$sex==1),1] <- 1 #"m10_19"
ESS_IS.cat[which(ESS_IS$age >= 20 & ESS_IS$age <= 29 & ESS_IS$sex==1),2] <- 1 #"m20_29"
ESS_IS.cat[which(ESS_IS$age >= 30 & ESS_IS$age <= 39 & ESS_IS$sex==1),3] <- 1 #"m30_39"
ESS_IS.cat[which(ESS_IS$age >= 40 & ESS_IS$age <= 49 & ESS_IS$sex==1),4] <- 1 #"m40_49"
ESS_IS.cat[which(ESS_IS$age >= 50 & ESS_IS$age <= 59 & ESS_IS$sex==1),5] <- 1 #"m50_59"
ESS_IS.cat[which(ESS_IS$age >= 60 & ESS_IS$age <= 69 & ESS_IS$sex==1),6] <- 1 #"m60_69"
ESS_IS.cat[which(ESS_IS$age >= 70 & ESS_IS$age <= 79 & ESS_IS$sex==1),7] <- 1 #"m70_79"
ESS_IS.cat[which(ESS_IS$age >= 80 & ESS_IS$age <= 89 & ESS_IS$sex==1),8] <- 1 #"m80_89"
ESS_IS.cat[which(ESS_IS$age >= 90 & ESS_IS$sex==1),9] <- 1 #"m90_over"

ESS_IS.cat[which(ESS_IS$age >= 10 & ESS_IS$age <= 19 & ESS_IS$sex==2),10] <- 1 #"f10_29"
ESS_IS.cat[which(ESS_IS$age >= 20 & ESS_IS$age <= 29 & ESS_IS$sex==2),11] <- 1 #"f20_29"
ESS_IS.cat[which(ESS_IS$age >= 30 & ESS_IS$age <= 39 & ESS_IS$sex==2),12] <- 1 #"f30_39"
ESS_IS.cat[which(ESS_IS$age >= 40 & ESS_IS$age <= 49 & ESS_IS$sex==2),13] <- 1 #"f40_49"
ESS_IS.cat[which(ESS_IS$age >= 50 & ESS_IS$age <= 59 & ESS_IS$sex==2),14] <- 1 #"f50_59"
ESS_IS.cat[which(ESS_IS$age >= 60 & ESS_IS$age <= 69 & ESS_IS$sex==2),15] <- 1 #"f60_69"
ESS_IS.cat[which(ESS_IS$age >= 70 & ESS_IS$age <= 79 & ESS_IS$sex==2),16] <- 1 #"f70_79"
ESS_IS.cat[which(ESS_IS$age >= 80 & ESS_IS$age <= 89 & ESS_IS$sex==2),17] <- 1 #"f80_89"
ESS_IS.cat[which(ESS_IS$age >= 90 & ESS_IS$sex==2),18] <- 1 #"f90_over"

colnames(ESS_IS.cat) <- c("m10_19", "m20_29", "m30_39", "m40_49",	"m50_59", "m60_69",	"m70_79", "m80_89", "m90_over", "f10_19", "f20_29", "f30_39", "f40_49",	"f50_59", "f60_69",	"f70_79", "f80_89",	"f90_over")#names
head(ESS_IS.cat)
ESS_IS.cat <- data.frame(ESS_IS.cat) 

#################################################
#### Create weights for age_sex
#################################################

#Creating aggregates for ESS_IT.cat
colSums(ESS_IS.cat) # counts per category do add up to total of survey (n=874, it is 6 are missing in gender)
ESS_IS_agg <- colSums(ESS_IS.cat) 
rbind(age_sex.msim[1,], ESS_IS_agg) #checking compatibility between census and individual data. 

#creating weight objects
weights0 <- array(dim=c(nrow(ESS_IS),nrow(age_sex.msim))) 
weights1 <- array(dim=c(nrow(ESS_IS),nrow(age_sex.msim)))
weights0[,] <- 1 # sets initial weights to 1

# Convert ESS data into aggregates to compare with census
ESS_IS_agg <- array(dim=c(nrow(age_sex.msim),ncol(age_sex.msim)))
ESS_IS_agg1 <- array(dim=c(nrow(age_sex.msim),ncol(age_sex.msim)))
colnames(ESS_IS_agg1) <- colnames(age_sex.msim)

for (i in 1:nrow(age_sex.msim)){
  ESS_IS_agg[i,]   <- colSums(ESS_IS.cat) * weights0[i,1]
}

#################################################
#### Re-weighting for age_sex constraint via IPF 
#################################################
for (j in 1:nrow(age_sex.msim)){
  
  weights1[which(ESS_IS$age >= 10 & ESS_IS$age <= 19 & ESS_IS$sex==1),j] <- age_sex.msim[j,1] /ESS_IS_agg[j,1] #"m10_19"
  weights1[which(ESS_IS$age >= 20 & ESS_IS$age <= 29 & ESS_IS$sex==1),j] <- age_sex.msim[j,2] /ESS_IS_agg[j,2] #"m20_29"
  weights1[which(ESS_IS$age >= 30 & ESS_IS$age <= 39 & ESS_IS$sex==1),j] <- age_sex.msim[j,3] /ESS_IS_agg[j,3] #"m30_39"
  weights1[which(ESS_IS$age >= 40 & ESS_IS$age <= 49 & ESS_IS$sex==1),j] <- age_sex.msim[j,4] /ESS_IS_agg[j,4] #"m40_49"
  weights1[which(ESS_IS$age >= 50 & ESS_IS$age <= 59 & ESS_IS$sex==1),j] <- age_sex.msim[j,5] /ESS_IS_agg[j,5] #"m50_59"
  weights1[which(ESS_IS$age >= 60 & ESS_IS$age <= 69 & ESS_IS$sex==1),j] <- age_sex.msim[j,6] /ESS_IS_agg[j,6] #"m60_69"
  weights1[which(ESS_IS$age >= 70 & ESS_IS$age <= 79 & ESS_IS$sex==1),j] <- age_sex.msim[j,7] /ESS_IS_agg[j,7] #"m70_79"
  weights1[which(ESS_IS$age >= 80 & ESS_IS$age <= 89 & ESS_IS$sex==1),j] <- age_sex.msim[j,8] /ESS_IS_agg[j,8] #"m80_89"
  weights1[which(ESS_IS$age >= 90 & ESS_IS$sex==1),j] <- age_sex.msim[j,9] /ESS_IS_agg[j,9] #"m90_over" 
  
  weights1[which(ESS_IS$age >= 10 & ESS_IS$age <= 19 & ESS_IS$sex==2),j] <- age_sex.msim[j,10] /ESS_IS_agg[j,10] #"f10_19"
  weights1[which(ESS_IS$age >= 20 & ESS_IS$age <= 29 & ESS_IS$sex==2),j] <- age_sex.msim[j,11] /ESS_IS_agg[j,11] #"f20_29"  
  weights1[which(ESS_IS$age >= 30 & ESS_IS$age <= 39 & ESS_IS$sex==2),j] <- age_sex.msim[j,12] /ESS_IS_agg[j,12] #"f30_39" 
  weights1[which(ESS_IS$age >= 40 & ESS_IS$age <= 49 & ESS_IS$sex==2),j] <- age_sex.msim[j,13] /ESS_IS_agg[j,13] #"f40_49"
  weights1[which(ESS_IS$age >= 50 & ESS_IS$age <= 59 & ESS_IS$sex==2),j] <- age_sex.msim[j,14] /ESS_IS_agg[j,14] #"f50_59"
  weights1[which(ESS_IS$age >= 60 & ESS_IS$age <= 69 & ESS_IS$sex==2),j] <- age_sex.msim[j,15] /ESS_IS_agg[j,15] #"f60_69"
  weights1[which(ESS_IS$age >= 70 & ESS_IS$age <= 79 & ESS_IS$sex==2),j] <- age_sex.msim[j,16] /ESS_IS_agg[j,16] #"f70_79"
  weights1[which(ESS_IS$age >= 80 & ESS_IS$age <= 89 & ESS_IS$sex==2),j] <- age_sex.msim[j,17] /ESS_IS_agg[j,17] #"f80_89"
  weights1[which(ESS_IS$age >= 90 & ESS_IS$sex==2),j] <- age_sex.msim[j,18] /ESS_IS_agg[j,18]#"f90_over"  
}

head(weights1)


############################################
#### Convert weights age_sex back into aggregate
#### values for each zone
############################################
for (i in 1:nrow(age_sex.msim)){
  ESS_IS_agg1[i,]   <- colSums(ESS_IS.cat * weights0[,i] * weights1[,i])
}

# Test results for first row
ESS_IS_agg1[2,1:18] 
age_sex.msim[2,1:18]

#checking if weights add up to the total inhabitants for each area.
total <- colSums(weights1) #sum of weights per zone
total
rowSums(age_sex.msim) # shows sum inhabitants per zone


###############################################
#### testing goodness of fit age_sex
###############################################
vec <- function(x) as.numeric(as.matrix(x))
cor(vec(ESS_IS_agg), vec(age_sex.msim)) # # tests correlation between the aggregate actual data and the constraints
cor(vec(ESS_IS_agg1), vec(age_sex.msim)) # tests correlation between correct total population for each zone and the new weights
weights1[, 1] 
age_sex.msim[1, ]


##############################################
##### saving weights1 age_sex as csv
##############################################
weights1.iceland <- data.frame(weights1)
weights1.iceland
write.csv2(weights1.iceland, "G:/My Drive/PHOENIX/WP3 Tangram design/Micro-simulation models for each pilot/Iceland/weights1.iceland.csv", row.names=FALSE,) 

##############################################
##### integerization weights 
##############################################
install.packages("wrswoR")
library("wrswoR")

int_trs <- function(x){
  
  # For generalisation purpose, x becomes a vector
  xv <- as.vector(x) # allows trs to work on matrices
  xint <- floor(xv) # integer part of the weight
  r <- xv - xint # decimal part of the weight
  def <- round(sum(r)) # the deficit population
  
  # the weights be 'topped up' (+ 1 applied)
  topup <- sample(length(x), size = def, prob = r)
  xint[topup] <- xint[topup] + 1
  dim(xint) <- dim(x)
  dimnames(xint) <- dimnames(x)
  xint
  
}

int_weight1 <- int_trs(weights1)

head(int_weight1)
head(weights1)

##############################################
##### saving intergerized weights
##############################################

int_weights1.france <- data.frame(int_weight1)
int_weights1.france
write.csv2(int_weights1.france, "G:/My Drive/PHOENIX/WP3 Tangram design/Micro-simulation models for each pilot/Iceland/int_weights1.iceland.csv", row.names=FALSE, )

