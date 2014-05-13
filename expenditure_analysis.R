#Tesco study

library(plyr) # for rename function
library(XLConnect)

setwd("~/Dropbox/Bewerbungen/Tesco")

#create data frame with age data
rm(age)
for (s in c("0-19", "20-29", "30-44", "45-59", "60-74", "75plus")) {
  fname <- paste("Var-age", s, ".csv", sep="")
  data <- read.csv(fname)
  if (exists("age")==FALSE) {  age <- data }
  else { 
    if (fname == "Var-age75plus.csv") { data <- rename(data, c("Area"="OA")) }
    age <- merge(age, data) 
  }
}

# load total population numbers
# error in data here! total population does not match sum of age groups in output areas!
totpop <- read.csv("Total_Pop.csv")

#load percentage of expenditure per household per age group
pctExpAge <- readWorksheet(loadWorkbook("pct_total_exp_per_agegroup.xls"),sheet=1)
pctExpAge <- pctExpAge[rowSums(is.na(pctExpAge))!=ncol(pctExpAge),] # delete rows with only NA's
pctExpAge <- pctExpAge[,colSums(is.na(pctExpAge))!=nrow(pctExpAge)] # delete columns with only NA's
pctExpAge <- pctExpAge[10:23,]
rownames <- pctExpAge[1:14,2]
colnames <- c("<30", "30-49","50-64", "65-74", "75+", "All")
pctExp <- data.frame(pctExpAge[1:14,3:8], row.names = rownames)
names(pctExp) <- colnames

#load age distribution of england and wales 2007
ageDist <- readWorksheet(loadWorkbook("EnglandWales_AgeDistribution.xls"),sheet=1)
ageDist <- ageDist[3:nrow(ageDist),1:2]
ageRange <- c(sum(ageDist[4:10,2]), sum(ageDist[11:14,2]), sum(ageDist[15:17,2]), 
              sum(ageDist[18:19,2]), sum(ageDist[20:23,2]))

# age range conversion of Percentage of Expenditures
ageConversion <- matrix( c( c(1,0,0,0,0) * sum(ageDist[4:8,2]) / sum(ageDist[4:10,2]), # 0 - 19
                    c(1,0,0,0,0) * sum(ageDist[9:10,2]) / sum(ageDist[4:10,2]), # 20 - 29
                    c(0,1,0,0,0) * sum(ageDist[11:13,2]) / sum(ageDist[11:14,2]), # 30 - 44
                    c(0,1,0,0,0) * ageDist[14,2] / sum(ageDist[11:14,2]) + c(0,0,1,0,0) * sum(ageDist[15:16,2]) / sum(ageDist[15:17,2]), # 45 - 59 
                    c(0,0,1,0,0) * ageDist[17,2] / sum(ageDist[15:17,2]) + c(0,0,0,1,0) * 1, # 60 - 74
                    c(0,0,0,0,1) * 1 ), nrow=5, ncol=6) # 75+

#calculate percentage of expenditures per age group
fracExp <- data.matrix(pctExp[,1:5]) %*% ageConversion
fracExp <- fracExp[-13,]
for(i in c(1:6)) { fracExp[,i] <- 100 * fracExp[,i] / sum(fracExp[,i]) }
fracExp <- data.frame(fracExp)
colnamesOA <- c("0-19", "20-29", "30-44", "45-59", "60-74", "75plus")
names(fracExp) <- colnamesOA

# compute percentage of expenditures per category in output areas
# !!! careful - sum(age[1,2:7]) != totpop[1,2] !!!! for all rows; don't use it
expCat <- data.frame(data.matrix(age[,2:7]) %*% t(data.matrix(fracExp)) / rowSums(age[,2:7]))
expCat <- cbind(age[,1], expCat)
colnames(expCat)[1] <- "OA11CD"

# save data frame in file
write.csv(expCat, file="exp_per_category.csv")

# save only one category with output area for plotting
write.csv(expCat[,1:2], file="exp_for_food.csv")
