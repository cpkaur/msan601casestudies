# Creat Rmarkdown stub file by Monday


d <- read.csv("41330727.csv", header = TRUE, stringsAsFactors = FALSE)
View(d)
for (i in 2:length(d)) {
  d[[i]] <- gsub("\\$", "", d[[i]])
  d[[i]] <- as.numeric(d[[i]])
}

# Question 1
# do four separate slr models where mailbox is X and predict others.

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
