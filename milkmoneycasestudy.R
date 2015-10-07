# Milk and Money Case Study
# Chaman Preet Kaur and Chris Atterbury
# MSAN 601

# Codeblocks

# Codeblock 1
milk <- read.csv("41330727.csv", header = TRUE, stringsAsFactors = FALSE)
head(milk)

# Codeblock 2
for (i in 2:length(milk)) {
  milk[[i]] <- gsub("\\$", "", milk[[i]])
  milk[[i]] <- as.numeric(milk[[i]])
}

# Codeblock 3
cor(milk[2:length(milk)])

# Codeblock 4
lm_classiv <- lm(Mailbox ~ Class.IV, data = milk)
print (summary(lm_classiv))

lm_classiii <- lm(Mailbox ~ Class.III, data = milk)
print (summary(lm_classiii))

lm_butter <- lm(Mailbox ~ Butter, data = milk)
print (summary(lm_butter))

lm_nfdm <- lm(Mailbox ~ NFDM, data = milk)
print (summary(lm_nfdm))

# Codeblock 5

lm_full <- lm(Mailbox ~ ., data = milk[-1])
print (summary(lm_full))

# Codeblock 6

s_2 <- anova(lm_classiii)[["Mean Sq"]][[2]]
x_bar <- mean(milk[["Class.III"]])
x_h <- (12.50 - coef(lm_classiii)[[1]]) / coef(lm_classiii)[[2]]
num <- (x_h - x_bar) ^ 2
denom <- sum((milk[["Class.III"]] - x_bar) ^ 2)
val <- 1 / 41 + num / denom
s <- s_2 * val
mbox_upper <- 12.50 + s * qt(0.95, 39)
print (mbox_upper)

# Codeblock 7
b_0 <- coef(lm_classiii)[[1]]
b_1 <- coef(lm_classiii)[[2]]
classiii_price <- (mbox_upper - b_0) / b_1
print (classiii_price)

# Codeblock 8
lm_lclassiii <- lm(Mailbox ~ log(Class.III), data = milk)
print (summary(lm_lclassiii))

lm_classiiil <- lm(log(Mailbox) ~ Class.III, data = milk)
print (summary(lm_classiiil))

# Codeblock 9
x_classiv <- (12.50 - coef(lm_classiv)[[1]]) / coef(lm_classiv)[[2]]
print (x_classiv)

x_classiii <- (12.50 - coef(lm_classiii)[[1]]) / coef(lm_classiii)[[2]]
print (x_classiii)

x_butter <- (12.50 - coef(lm_butter)[[1]]) / coef(lm_butter)[[2]]
print (x_butter)

x_nfdm <- (12.50 - coef(lm_nfdm)[[1]]) / coef(lm_nfdm)[[2]]
print (x_nfdm)

# Codeblock 10
b_0 <- coef(lm_classiii)[[1]]
b_1 <- coef(lm_classiii)[[2]]
classiii_newprice <- (11.50 - b_0) / b_1
print (classiii_newprice)
diff_classiii <- classiii_price - classiii_newprice
print (diff_classiii)
diff_mbox <- mbox_upper - 11.50
print (diff_mbox)

# Figures

# Figure 1
pairs(milk[2:length(milk)], main = "Pairs Plots for Dairy Products")

# Figure 2
library(car)
scatterplot(Mailbox ~ Class.III, data = milk,
            main = "Scatterplot of Mailbox vs. Class.III",
            xlab = "Class.III Dairy Products Price (dollars)",
            ylab = "Mailbox Price (dollars)",
            boxplot = FALSE)

# Figure 3
scatterplot(Mailbox ~ log(Class.III), data = milk,
            main = "Scatterplot of Mailbox vs. log(Class.III)",
            xlab = "log of Class.III Dairy Products Price (dollars)",
            ylab = "Mailbox Price (dollars)",
            boxplot = FALSE)



