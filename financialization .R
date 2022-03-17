# Trials of financialization: setup

fundpanelnocal <- mutate(fundpanelnocal, discperf=ass_inv-rtn5)
fp1 <- filter(fundpanelnocal, tier==1)
fp2 <- filter(fundpanelnocal, tier==2)
fp3 <- filter(fundpanelnocal, tier==3)

finpanellag <- dynformula(financ ~  lag(UAALpc, 1) + lag(discperf,1) + lag(benhouse,1) + lintcost + lag(lintcost, 1) + 
                            lag(lpcdebt,1) + lag(activeratio,1) + lag(aa_risk,1) + rtn1)


# Trials of financialization: lag RE model specification with Hausman test

fintestlag <- plm(finpanellag, data=fundpanelnocal, index=c("name", "fy"), model='random', effect='individual') 
  summary(fintestlag)
  
try <- plm(finpanellag, data=fundpanelnocal, index=c("name", "fy"), model='within', effect='individual') 

phtest(try, fintestlag, data=fundpanelnocal)

trypool <- plm(finpanellag, data=fundpanelnocal, model='pooling') 
plmtest(trypool, effect="twoways", type="ghm")


pcdtest(fintestlag)
pcdtest(fintestlag1)
pcdtest(fintestlag2)
pcdtest(fintestlag3)

pdwtest(fintestlag)
pdwtest(fintestlag1)
pdwtest(fintestlag2)
pdwtest(fintestlag3)


# Trials - using FE 

fintestlag <- plm(finpanellag, data=fundpanelnocal, index=c("name", "fy"), model='random', effect='individual', within=TRUE) 
summary(fintestlag)

  fintestlag1 <- plm(finpanellag, data=fp1, index=c("name", "fy"), model='random', effect='individual') 
  fintestlag2 <- plm(finpanellag, data=fp2, index=c("name", "fy"), model='random', effect='individual') 
  fintestlag3 <- plm(finpanellag, data=fp3, index=c("name", "fy"), model='random', effect='individual') 
  
    fincoeflag <- coeftest(fintestlag, vcovHC(fintestlag, method="arellano"))
      fincoeflag1 <- coeftest(fintestlag1, vcovHC(fintestlag1, method="arellano", type="sss"))
      fincoeflag2 <- coeftest(fintestlag2, vcovHC(fintestlag2, method="arellano", type="sss"))
      fincoeflag3 <- coeftest(fintestlag3, vcovHC(fintestlag3, method="arellano", type="sss"))
      
texreg(list(fincoeflag, fincoeflag3, fincoeflag2, fincoeflag1), stars=c(0.001, 0.01, 0.05, 0.1), digits=4, symbol="\\odot", caption.above=TRUE, caption="R Effects Panel Regressions 2001 - 2013", use.booktabs=TRUE)
texreg(list(fintestlag, fintestlag3, fintestlag2, fintestlag1), stars=c(0.001, 0.01, 0.05, 0.1), digits=3, symbol="\\odot", caption.above=TRUE, caption="R Effects Panel Regressions 2001 - 2013", use.booktabs=TRUE)


# 1b. Making object for ggplot
      
finance <- as.data.frame(cbind(fincoeflag[,1], 1.96*fincoeflag[,2], fincoeflag1[,1], 1.96*fincoeflag1[,2], 
                         fincoeflag3[,1], 1.96*fincoeflag3[,2]))
finance[5:6,] <- finance[5:6,]/100
finance[1,] <- finance[1,]*1000
finance[7,] <- finance[7,]/100
rownames(finance) <- c("Funding Gap", "Interest", "Debt", "Generosity", "Return", "Lagged Return", 'Missed Target',  'Demographics')
colnames(finance) <- rep(c("Estimate", "CI"),3)

totalfin <- ggplot(finance[,1:2], aes(x=Estimate, y=rownames(finance))) +  
  geom_errorbarh(aes(xmax=Estimate+CI, xmin=Estimate-CI, colour=ifelse(Estimate-CI < 0 & Estimate+CI > 0, "blue", "grey")), height=0.01) + 
  geom_point(aes(x=Estimate, colour=ifelse(Estimate-CI < 0 & Estimate+CI > 0, "blue", "grey"))) + geom_vline(aes(xintercept=0), lty=2, col="grey") + 
  ylab('') + xlab('') + scale_y_discrete(limits=c("Lagged Return", "Return", "Debt", "Interest", "Demographics", "Generosity", 
                                                  "Missed Target", "Funding Gap")) + 
  theme_bw() + theme(legend.position="none") 
totalfin

bottomfin <- ggplot(finance[,3:4], aes(x=Estimate, y=rownames(finance))) +  
  geom_errorbarh(aes(xmax=Estimate+CI, xmin=Estimate-CI, colour=ifelse(Estimate-CI < 0 & Estimate+CI > 0, "blue", "grey")), height=0.01) + 
  geom_point(aes(x=Estimate, colour=ifelse(Estimate-CI < 0 & Estimate+CI > 0, "blue", "grey"))) + geom_vline(aes(xintercept=0), lty=2, col="grey") + 
  ylab('') + xlab('') +   ylab('') + xlab('') + scale_y_discrete(limits=c("Lagged Return", "Return", "Debt", "Interest", "Demographics", "Generosity", 
                                                                          "Missed Target", "Funding Gap")) + 
  theme_bw() + theme(legend.position="none") 
bottomfin

bestfin <- ggplot(finance[,5:6], aes(x=Estimate, y=rownames(finance))) +  
  geom_errorbarh(aes(xmax=Estimate+CI, xmin=Estimate-CI, colour=ifelse(Estimate-CI < 0 & Estimate+CI > 0, "blue", "grey")), height=0.01) + 
  geom_point(aes(x=Estimate, colour=ifelse(Estimate-CI < 0 & Estimate+CI > 0, "blue", "grey"))) + geom_vline(aes(xintercept=0), lty=2, col="grey") + 
  ylab('') + xlab('') +   ylab('') + xlab('') + scale_y_discrete(limits=c("Lagged Return", "Return", "Debt", "Interest", "Demographics", "Generosity", 
                                                                          "Missed Target", "Funding Gap")) + 
  theme_bw() + theme(legend.position="none") 
bestfin




###### Not used for now: financialization with no lag


finpanel <- dynformula(financ ~ UAALpc + lintcost + lpcdebt +  
                         benhouse + rtn1 + activeratio)

fintest <- plm(finpanel, data=fundpanelnocal, model='random', effect='individual') 
summary(fintest)

fintest1 <- plm(finpanel, data=fp1, model='random', effect='individual') 
fintest2 <- plm(finpanel, data=fp2, model='random', effect='individual') 
fintest3 <- plm(finpanel, data=fp3, model='random', effect='individual') 

fincoef <- coeftest(fintest, vcovHC(fintest, method="arellano"))
fincoef1 <- coeftest(fintest1, vcovHC(fintest1, method="arellano"))
fincoef2 <- coeftest(fintest2, vcovHC(fintest2, method="arellano"))
fincoef3 <- coeftest(fintest3, vcovHC(fintest3, method="arellano"))

### @ @ @ @ USE ABOVE @ @ @ @ @ ###


#### ARC

arcpanel <- dynformula(ARCpct ~  lag(UAALpc, 1) + lag(lintcost, 1) + lag(lpcdebt,1) + benhouse + 
                         lag(benhouse,1) + rtn1 + lag(rtn1, 1) + activeratio + lag(activeratio, 1))
arctestlag <- plm(arcpanel, data=fundpanelnocal, model='within', effect='twoways') 
  summary(arctestlag)


arctestlag1 <- plm(arcpanel, data=fp1, model='within', effect='twoways') 
arctestlag2 <- plm(arcpanel, data=fp2, model='within', effect='twoways') 
arctestlag3 <- plm(arcpanel, data=fp3, model='within', effect='twoways') 
    summary(arctestlag1)
    summary(arctestlag2)
    summary(arctestlag3)

#### Residualizing??
    
resids <- plm(financ~rtn1, data=fundpanelnocal, index=c("name", "fy"), model='within', effect="twoways")
plot(resids)
    
