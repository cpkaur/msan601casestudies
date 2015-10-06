# Creat Rmarkdown stub file by Monday


d <- read.csv("41330727.csv", header = TRUE, stringsAsFactors = FALSE)
View(d)
for (i in 2:length(d)) {
  d[[i]] <- gsub("\\$", "", d[[i]])
  d[[i]] <- as.numeric(d[[i]])
}

# Question 1
# do four separate slr models where mailbox is X and predict others.
lm_Classiv <- lm(Class.IV ~ Mailbox, data = d)
summary(lm_Classiv)
coef(lm_Classiv)
price_iv <- coef(lm_Classiv)[[1]] + coef(lm_Classiv)[[2]] * 12.5
## 12.18741

lm_Classiii <- lm(Class.III ~ Mailbox, data = d)
summary(lm_Classiii)
price_iii <- coef(lm_Classiii)[[1]] + coef(lm_Classiii)[[2]] * 12.5
## 13.22015

lm_Butter <- lm(Butter ~ Mailbox, data = d)
summary(lm_Butter)
price_but <- coef(lm_Butter)[[1]] + coef(lm_Butter)[[2]] * 12.5
## 1.431444

lm_Nfdm <- lm(NFDM ~ Mailbox, data = d)
summary(lm_Nfdm)
price_nfdm <- coef(lm_Nfdm)[[1]] + coef(lm_Nfdm)[[2]] * 12.5
## 1.017998

# Question 2
# class III is clearly the most related to Mailbox
lm_milk <- lm(Mailbox ~ . - Month, data = d)
summary(lm_milk)

lm_classiv <- lm(Mailbox ~ Class.IV, data = d)
summary(lm_classiv)

lm_classiii <- lm(Mailbox ~ Class.III, data = d)
summary(lm_classiii)
coef(lm_classiii)

lm_butter <- lm(Mailbox ~ Butter, data = d)
summary(lm_butter)

lm_nfdm <- lm(Mailbox ~ NFDM, data = d)
summary(lm_nfdm)

# Question 3
# put option: right to sell at a certain price. Thus, they are better when the
# price falls since you can still sell at the higher price.
# call option: right to buy at a certain price. Thus, they are better when the
# price falls since you can still buy at the lower price.

# at the money: strike price, at which option was purchased, is roughly equal to
# underlying value.
# in the money: will make money. for put option, strike price is above.
# out of money: will not make money, so do not exercise option and only pay
# premium. 

# Question 4

# 12.64 Mailbox if we purchase a put option at 13.34 for Class.III
# 11.50 Mailbox. so 11.89 for Class.III
# 13.34 - 11.89 = 1.45

# Question 5
# if we want 13.34, then just purchase at 13.34 + premium and trading fees
# (e.g. 13.34+.45=13.89)
