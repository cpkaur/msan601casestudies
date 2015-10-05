milk <- read.csv("41330727.csv", stringsAsFactors = FALSE)
str(milk)
n <- length(milk)
for (i in 2:n){
  milk[[i]] <- gsub("\\$", "", milk[[i]])
  milk[[i]] <- as.numeric(milk[[i]])
}
  
# Complete model R2 = .9608, intercept, class IV, 
# class II significant p values
model <- lm(Mailbox ~ . , data = milk[2:n])
summary(model)

pairs(milk[2:n])
cor(milk[2:n])
library(lmtest)
resettest(model , power=2, type="regressor") # p-value = 0.00498
resettest(model , power=3, type="regressor") # p-value = 0.004712

# R2 = 0.9268
model_1 <- lm(Mailbox ~ Class.III , data = milk[2:n])
summary(model_1)

# R2 =  0.7121
model_2 <- lm(Mailbox ~ Class.IV , data = milk[2:n])
summary(model_2)

# R2 = 0.7562, intercept negative 
model_2t <- lm(Mailbox ~ log(Class.IV) , data = milk[2:n])
summary(model_2t)

# R2 = 0.6178
model_3 <- lm(Mailbox ~ Butter, data = milk[2:n])
summary(model_3)

# R2 = 0.02961, p value > 0.05
model_4 <- lm(Mailbox ~ NFDM, data = milk[2:n])
summary(model_4)

# R2 = 0.9268
model_5 <- lm(Class.III ~ Mailbox , data = milk[2:n])
summary(model_5)

# R2 = 0.9643, class IV, III, nfdm significant, nfdm negative
model_6 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III +
                milk$Butter + milk$NFDM + I(milk$Class.IV^2))
summary(model_6)

# R2 = 0.9698, (nfdm, III^2, intercept) = negative,
# all except butter sign p values 
model_7 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III +
                milk$Butter + milk$NFDM + I(milk$Class.III^2))
summary(model_7)

# R-squared:  0.9666, (intercept, nfdm, butter^2) negative 
model_8 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III +
                milk$Butter + milk$NFDM + I(milk$Butter^2))
summary(model_8)

# Multiple R-squared:  0.9611, nfdm, nfdm^2 -- negative
# classIV and III sign p value

model_9 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III +
                milk$Butter + milk$NFDM + I(milk$NFDM^2))
summary(model_9)


# Multiple R-squared:  0.983, no sign p value
model_10 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III +
                milk$Butter + milk$NFDM + 
                 milk$Class.IV*milk$Class.III*milk$Butter*milk$NFDM)
summary(model_10)

# R-squared:  0.9542, intercept, interaction term negative, all terms sign p
model_12 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Class.IV:milk$Class.III:milk$Butter:milk$NFDM)
summary(model_12)

# R-squared:  0.9502, interaction negative
model_13 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Class.IV:milk$Class.III)
summary(model_13)

# R-squared:  0.9459, inercept, interaction negative
model_14 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Butter:milk$NFDM)
summary(model_14)

# R-squared:  0.9436, all terms sign p
model_15 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Class.IV:milk$Class.III:milk$Butter)
summary(model_15)

# R-squared:  0.9593, all signficant p, intercept  & two interactiion negative
model_16 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Class.IV:milk$Class.III + 
                 milk$Class.IV:milk$Class.III:milk$Butter)
summary(model_16)

#R-squared:  0.9619, intercept, interaction terms negative, two way interaction insign
model_17 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Class.IV:milk$Class.III + 
                 milk$Class.IV:milk$Class.III:milk$NFDM)
summary(model_17)

# R-squared:  0.9504, all terms positive and with significant p value
model_18 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Butter:milk$Class.III)
summary(model_18)
# R-squared:  0.9623, class IV, ineraction terms negative, all sinificant p values 
model_19 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Butter:milk$Class.III +
                 milk$Butter:milk$Class.IV)
summary(model_19)


#### best model:
model_18 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Butter:milk$Class.III)
summary(model_18)

confint(model_18, level=0.95)
anova(model_18)
library(car)
Anova(model_18)
vcov(model_18)  
residualPlots(model_18)
plot(model_18)
