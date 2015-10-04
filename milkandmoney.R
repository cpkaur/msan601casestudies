milk <- read.csv("41330727.csv", stringsAsFactors = FALSE)
str(milk)
n <- length(milk)
for (i in 2:n){
  milk[[i]] <- gsub("\\$", "", milk[[i]])
  milk[[i]] <- as.numeric(milk[[i]])
}
  
model <- lm(Mailbox ~ . , data = milk[2:n])
summary(model)

model_1 <- lm(Mailbox ~ Class.III , data = milk[2:n])
summary(model_1)
model_2 <- lm(Mailbox ~ Class.IV , data = milk[2:n])
summary(model_2)
model_2t <- lm(Mailbox ~ log(Class.IV) , data = milk[2:n])
summary(model_2t)
model_3 <- lm(Mailbox ~ Butter, data = milk[2:n])
summary(model_3)
model_4 <- lm(Mailbox ~ NFDM, data = milk[2:n])
summary(model_4)

model_5 <- lm(Class.III ~ Mailbox , data = milk[2:n])
summary(model_5)

pairs(milk[2:n])

library(lmtest)
resettest(model , power=2, type="regressor")
resettest(model , power=3, type="regressor")

model_6 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III +
                milk$Butter + milk$NFDM + I(milk$Class.IV^2))
summary(model_6)

model_7 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III +
                milk$Butter + milk$NFDM + I(milk$Class.III^2))
summary(model_7)

model_8 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III +
                milk$Butter + milk$NFDM + I(milk$Butter^2))
summary(model_8)

model_9 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III +
                milk$Butter + milk$NFDM + I(milk$NFDM^2))
summary(model_9)

model_10 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III +
                milk$Butter + milk$NFDM + 
                 milk$Class.IV*milk$Class.III*milk$Butter*milk$NFDM)
summary(model_10)

model_11 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Class.IV*milk$Class.III*milk$Butter*milk$NFDM)
summary(model_11)

model_12 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Class.IV:milk$Class.III:milk$Butter:milk$NFDM)
summary(model_12)

model_13 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Class.IV:milk$Class.III)
summary(model_13)

model_14 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Butter:milk$NFDM)
summary(model_14)

model_15 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Class.IV:milk$Class.III:milk$Butter)
summary(model_15)

model_16 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Class.IV:milk$Class.III + 
                 milk$Class.IV:milk$Class.III:milk$Butter)
summary(model_16)

model_17 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Class.IV:milk$Class.III + 
                 milk$Class.IV:milk$Class.III:milk$NFDM)
summary(model_17)

model_18 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Butter:milk$Class.III)
summary(model_18)

cor(milk[3:6])

model_19 <- lm(milk$Mailbox ~ milk$Class.IV + milk$Class.III + 
                 milk$Butter:milk$Class.III +
                 milk$Butter:milk$Class.IV)
summary(model_19)
