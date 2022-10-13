library(openintro)
lego <- read.csv("lego_sample.csv")


Q1 <- cov(lego$Amazon_Price, lego$Pieces) 
  
Q2 <- cor(lego$Amazon_Price, lego$Pieces)

Q3 <- lm(Amazon_Price ~ Pieces, data = lego)

Q3a <- Q3$coefficients

Q3b <- Q3$coefficients[1] + Q3$coefficients[2] * 350
  
Q3c <- summary(Q3)$r.squared

#Q4
plot(lego$Amazon_Price ~ lego$Pieces)
abline(Q3)

Q5 <- Q3$residuals
Q5a <- lego$Set_Name[which.max(Q5)]

#Q6
plot(fitted(Q3), Q5)
abline(0,0, col="red")

qqnorm(Q5)
abline(0,0, col="red")

Q7 <- shapiro.test(Q5)$p.value

#Set mu1 - mu2, with mu1= being mean amzon price with additional piece and mu2 being mean amazong price.
#Use model to predict Amazon price as is +ve R^2
# Assume to sample

Q8a <- summary(Q3)$coef[2,1]
pe <- summary(Q3)$coef[2,1]
se <- summary(Q3)$coef[2,2]
Q8b <- (pe - 2) / se
df <- lin_mod$df.residual
Q8c <- 2 * pt(((pe - 2) / se), df, lower.tail = TRUE)


#Q9 
plot(lego$Amazon_Price,lego$Pieces, col = ifelse(lego$Theme == 'Friends','red','green'))

#Q10
LC <- subset(lego, lego$Theme=="City")
LF <- subset(lego, lego$Theme=="Friends")
LC_lm <- lm(Amazon_Price ~ Pieces, data= LC)
LF_lm <- lm(Amazon_Price ~ Pieces, data= LF)
Q10_1 <- unname(LF_lm$coefficients[2])
Q10_2 <- unname(LC_lm$coefficients[2])


fr <- subset(lego, Theme == "Friends")
ci <- subset(lego, Theme == "City")

lrmfr <- lm(fr$Amazon_Price ~ fr$Pieces)
lrmci <- lm(ci$Amazon_Price ~ ci$Pieces)
Q10_1 <- as.numeric(lrmfr$coefficients[2])
Q10_2 <- as.numeric(lrmci$coefficients[2])

#Q11
Theme_Ind <- ifelse(lego$Theme == "City", 1,0)

#Q12
Q12_lm <- lm(Amazon_Price ~ Theme ,data=lego)
Q12 <- unname(Q12_lm$coefficients)
f_stat <- summary(Q12_lm)$fstatistic
Q12a <- as.numeric(lm(lego$Amazon_Price ~ Theme_ind_t)$coefficients)

Q12b <- as.numeric(anova(lm(lego$Amazon_Price ~ Theme_ind_t))[1,5])
