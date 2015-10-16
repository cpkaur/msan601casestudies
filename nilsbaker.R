nils <- read.csv("41330723.csv", stringsAsFactors = FALSE)
nils <- nils[1:120,]
nils <- nils[-1]

n <- length(nils)
for (i in 1:2){
  nils[[i]] <- gsub("\\,", "", nils[[i]])
  nils[[i]] <- as.numeric(nils[[i]])
  }
for (i in 1:length(nils$Inside.Outside.Footprint)){
  if (nils$Inside.Outside.Footprint[i] == "Inside"){
    nils$Inside.Outside.Footprint[i] = 1
  } else {
    nils$Inside.Outside.Footprint[i] = 0
  }
}

nils$Inside.Outside.Footprint <- as.numeric(nils$Inside.Outside.Footprint)


### models ####
m_1 <- lm(nils$Households.with.Account ~., data = nils)
summary(m_1)
library(lmtest)
resettest(m_1 , power=2, type="regressor")
resettest(m_1 , power=2, type="fitted") 

m_2 <- lm(nils$Households.with.Account ~ nils$Total.Households.in.Area, data = nils)
summary(m_2)

m_3 <- lm(nils$Households.with.Account ~ nils$Total.Households.in.Area*nils$Inside.Outside.Footprint, data = nils)
summary(m_3)

m_4 <- lm(nils$Households.with.Account ~ nils$Total.Households.in.Area + I(nils$Total.Households.in.Area^2) , data = nils)
summary(m_4)

percentage <- nils$Households.with.Account/nils$Total.Households.in.Area

nils <- cbind(nils, percentage)
names(nils)[1] <- "total_h"
names(nils)[2] <- "account_h"
names(nils)[3] <- "i_o"
m_5 <- lm(nils$percentage ~ nils$i_o, data = nils)
summary(m_5)

## exploring things 
cor(nils)
i_o.freq = table(nils$i_o)
barplot(i_o.freq)
boxplot(nils$percentage ~ nils$i_o)
by(nils$percentage, nils$i_o, summary)
mosaicplot(table(nils$percentage, nils$i_o))
barplot(table(nils$i_o))
