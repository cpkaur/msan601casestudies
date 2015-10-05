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
summary(lm_classiv)

lm_Classiii <- lm(Class.III ~ Mailbox, data = d)
summary(lm_classiii)

lm_Butter <- lm(Butter ~ Mailbox, data = d)
summary(lm_Butter)

lm_Nfdm <- lm(NFDM ~ Mailbox, data = d)
summary(lm_Nfdm)

# Question 2
# class III is clearly the most related to Mailbox
lm_milk <- lm(Mailbox ~ . - Month, data = d)
summary(lm_milk)

lm_classiv <- lm(Mailbox ~ Class.IV, data = d)
summary(lm_classiv)

lm_classiii <- lm(Mailbox ~ Class.III, data = d)
summary(lm_classiii)

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

