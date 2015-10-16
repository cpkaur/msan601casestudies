library(car)
library(MASS)

d <- read.csv("41330723.csv", header = TRUE, stringsAsFactors = FALSE)
d <- d[1:120, ] # last two rows contain no data
names(d) <- c("ID", "Total.Households", "Accounts", "Footprint")
View(d)
for (i in 2:3) {
  d[[i]] <- gsub(",", "", d[[i]])
  d[[i]] <- as.numeric(d[[i]])
}
d[["Footprint"]][d[["Footprint"]] == "Outside"] <- 0
d[["Footprint"]][d[["Footprint"]] == "Inside"] <- 1
head(d[["Footprint"]])
d[["Footprint"]] <- as.numeric(d[["Footprint"]])
d[["ID"]] <- as.numeric(d[["ID"]])
d[["Accts.Hsehld"]] <- d[["Accounts"]] / d[["Total.Households"]]
cor(d)

# SLR models Accounts
lm_1 <- lm(Accounts ~ Total.Households, data = d)
summary(lm_1)

lm_2 <- lm(Accounts ~ Footprint, data = d)
summary(lm_2)

lm_3 <- lm(Accounts ~ Accts.Hsehld, data = d)
summary(lm_3)

# SLR models Accts.Hsehld
lm_4 <- lm(Accts.Hsehld ~ Total.Households, data = d)
summary(lm_4)

lm_5 <- lm(Accts.Hsehld ~ Accounts, data = d)
summary(lm_5)

lm_6 <- lm(Accts.Hsehld ~ Footprint, data = d)
summary(lm_6)

# MLR Models
lm_7 <- lm(Accts.Hsehld ~ . - ID, data = d)
summary(lm_7)

lm_8 <- lm(Accts.Hsehld ~ (. - ID + I(Accounts^2) + I(Total.Households^2))^2,
           data = d)
summary(lm_8)

lm_9 <- lm(Accts.Hsehld ~ (. - ID + I(Accounts^2) + I(Total.Households^2))^2 -
             Total.Households:Accounts,
           data = d)
summary(lm_9)

summary(lm(Accts.Hsehld ~ . - ID + Footprint:Accounts +
             Total.Households:Footprint + I(Accounts^2) +
             I(Total.Households^2) + Footprint:I(Accounts^2) +
             Footprint:I(Total.Households^2),
           data = d))

summary(lm(Accts.Hsehld ~ Footprint + Total.Households +
             Footprint:Total.Households + I(Total.Households^2) +
             Footprint:I(Total.Households^2),
           data = d))

# Ramsey
library(lmtest)
resettest(lm_7, power = 2)

lm_3 <- lm(Accounts ~ Total.Households + Footprint, data = d)
summary(lm_3)

lm_4 <- lm(Accounts ~ Total.Households + Footprint +
           Total.Households:Footprint, data = d)
summary(lm_4)



