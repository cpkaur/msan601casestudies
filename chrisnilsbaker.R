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
cor(d)

# SLR models
lm_1 <- lm(Accounts ~ Total.Households, data = d)
summary(lm_1)

lm_2 <- lm(Accounts ~ Footprint, data = d)
summary(lm_2)

# MLR Models
lm_3 <- lm(Accounts ~ Total.Households + Footprint, data = d)
summary(lm_3)

lm_4 <- lm(Accounts ~ Total.Households + Footprint +
           Total.Households:Footprint, data = d)
summary(lm_4)



