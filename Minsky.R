library(plm)
library(car)
library(dplyr)
library(MASS)
library(texreg)

## HYPOTHESIS 2 ## 

# First try - no luck # 


vol <- nfundpanelu %>%
  group_by(name) %>%
  summarise(vola = sd(rtn1, na.rm=TRUE))

minsky <- filter(nfundpanelu, fy == 2013)
minsky <- cbind(liab, vol[2])

Minsky <- minsky
Minsky[3] <- log(Minsky[3])
Minsky[6] <- log(Minsky[6])
Minsky[8] <- log(Minsky[8])
Minsky[9] <- log(Minsky[9])
Minsky[15] <- log(Minsky[15])
Minsky[16] <- log(Minsky[16])

# Second try - linear model
Minskyc <- mutate(Minskyc,
                  discperf = ass_inv-rtn5)
hyman <- lm(UAALpc ~ financ + rtn5 + vola + lpcdebt + lintcost + ass_inv + benhouse + activeratio, data=Minskyc)
hyman2 <- lm(UAALpc ~ financ + discperf + vola + benhouse+ lpcdebt + lintcost + activeratio, data=Minskyc)

# Robustness checks

# general plots

plot(hyman)
plot(hyman2)

#normal?

shapiro.test(residuals(hyman))
shapiro.test(residuals(hyman2))

# Outliers?

outlierTest(hyman)
outlierTest(hyman2)
# Create figure

texreg(hyman, stars=c(0.001, 0.01, 0.05, 0.1), symbol="\\odot", caption.above=TRUE, digits=3, 
       fontsize = 'tiny', caption="OLS Regression Using 2013 Data")

# Testing for robustness - subset that hurts normality removed

Minskysub <- Minskyc[-104,]
Minskysub <- Minskysub[-94,]
Minskysub <- Minskysub[-51,]
Minskysub <- Minskysub[-34,]
Minskysub <- Minskysub[-5,]
rownames(Minskysub) <- NULL

hymansub <- lm(UAALpc ~ financ + rtn5 + vola + lpcdebt + lintcost + ass_inv + benhouse + activeratio, data=Minskysub)
hymansub2 <- lm(UAALpc ~ financ + discperf + vola + benhouse+ lpcdebt + lintcost + activeratio, data=Minskysub)

plot(hymansub)
shapiro.test(residuals(hymansub2))
hist(residuals(hymansub2))
outlierTest(hymansub)

texreg(hymansub, stars=c(0.001, 0.01, 0.05, 0.1), symbol="\\odot", caption.above=TRUE, caption="Robustness Check of OLS Regression Using 2013 Data")


texreg(list(hyman2, hymansub2), stars=c(0.001, 0.01, 0.05), digits=3, symbol="\\odot", 
       caption.above=FALSE, bold=0.05)


## First Version: Plot
coefplot(hyman2, col.pts="black", 
         varnames=c("Intercept", "Financialization", "5yr Return", "Investment\nVolatility", "Debt", 
                    "Interest Cost", "Discount\nRate", "Benefit\nGenerosity", "Demographics"), 
         xlim=c(-6,4), cex.var=0.95,
         main="2013 OLS Regression", CI=2, cex.pts=1.4)

## Second Version: Plot
coefplot(hyman2, col.pts="black", 
         varnames=c("Intercept", "Financialization", "Performance Gap", "Investment\nVolatility", "Benefit Generosity", 
                    "Debt", "Interest Cost", "Demographics"), 
         xlim=c(-6,4), xlab='',
         CI=2, cex.pts=1.3, cex.var=0.95)
coefplot(hymansub2, add=TRUE, CI=2, col.pts = "blue", cex.pts=1.8)


