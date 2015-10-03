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
model_2 <- lm(Mailbox ~ (Class.IV) , data = milk[2:n])
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



