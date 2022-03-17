library(lmtest)
library(plm)
library(dplyr)
library(pdR)
library(texreg)
library(stargazer)

# Creating panel structure with log transformed

newfundingsub3 <- mutate(newfundingsub3, 
                         discperf = ass_inv - rtn5)

fundpanelcal <- dplyr::select(newfundingsub2, name, fy, tier, UAALpc, financ, ideol2, 
                       lpcdebt, lintcost, ARCpct, benhouse, ass_inv, rtn5, rtn1, aa_risk, activeratio)

fundpanelnocal <- dplyr::select(newfundingsub3, name, fy, tier, UAALpc, financ, ARCpct, benhouse, discperf, ideol2, 
                         lpcdebt, lintcost, rtn5, rtn1, ass_inv, aa_eq, aa_alt, aa_risk, activeratio)
fundpanelcrazy <- dplyr::select(newfundingsub3, name, fy, tier, UAALpc, actassets, actliabs, financ, ARCpct, benhouse, avgbenefit, household,
                                discperf, ideol2, lpcdebt, lintcost, interest, genrev, rtn5, rtn1, ass_inv, aa_eq, aa_alt, aa_risk, activeratio)

# Descriptive statistics for Tables 1 & 3

descrips <- dplyr::select(newfundingsub3, UAALpc, actassets, actliabs, UAAL, financ, ARCpct, benhouse, avgbenefit, household, discperf, ass_inv, rtn5, ideol2, debtpc, intcost, interest, genrev, aa_risk, aa_eq, aa_alt, activeratio)
desc1 <- filter(descrips, tier==1)
desc2 <- filter(descrips, tier==2)
desc3 <- filter(descrips, tier==3)

stargazer(descrips)
stargazer(desc3, desc, desc1)

# Correlations for Table 2

descrips <- select(fundpanelnocal, UAALpc, financ, ARCpct, discperf, ideol2, debtpc, intcost, benhouse, rtn5, ass_inv, aa_risk, activeratio)
corr.matrix <- cor(correls, use="complete.obs")
stargazer(corr.matrix, title="Correlation Matrix", float.env = "sidewaystable", font.size = "small")


# Creating good bad ugly
# Tier 3 are lowest UAAL #####

fp1 <- filter(fundpanelnocal, tier==1)
fp2 <- filter(fundpanelnocal, tier==2)
fp3 <- filter(fundpanelnocal, tier==3)


fundpanelnocal <- plm.data(fundpanelnocal, index = c("name", "fy"))
fp1 <- plm.data(fp1, index = c("name", "fy"))
fp2 <- plm.data(fp2, index = c("name", "fy"))
fp3 <- plm.data(fp3, index = c("name", "fy"))


# Functional forms

#abpanel <- dynformula(UAALpc ~ financ + ideol2 + lpcdebt + lintcost + ARCpct + benhouse + 
#                        rtn5 + rtn1 + ass_inv + aa_risk + activeratio, lag=list(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
#diffpanel <- dynformula(UAALpc ~ financ + ideol2 + lpcdebt + lintcost + ARCpct + benhouse + 
#                        rtn5 + rtn1 + ass_inv + aa_risk + activeratio, diff=list(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))


lagpanelcrazy <- dynformula(UAALpc ~ actassets + actliabs + financ + ARCpct + benhouse + avgbenefit + household + discperf + ass_inv + rtn5 + 
                              ideol2 + lpcdebt + lintcost + interest + + genrev +  aa_risk + aa_eq + aa_alt + activeratio, diff=list(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
diffpanel <- dynformula(UAALpc ~ financ + discperf + ARCpct + benhouse + ideol2 + lintcost + lpcdebt + aa_risk + activeratio, diff=list(1, 0, 0, 0, 0, 0, 0, 0, 0, 0))

lagpanel <- dynformula(UAALpc ~  lag(financ,1) + lag(discperf,1) + lag(ARCpct,1) + lag(benhouse,1) + lintcost + lag(lintcost,1) + lag(lpcdebt,1) + lag(activeratio,1))

# Unused - AB Version

#abtest <- pgmm(abpanel, data=fundpanelnocal, effect="twoways", model="twosteps", transformation="d", gmm.inst=~UAALpc+ARCpct, gmm.inst=c(2,10))

# USE - Checking specifications, first pooling vs. fe, then random vs. fe

        trypool1 <- plm(diffpanel, data=fundpanelnocal, model='pooling') 
        plmtest(trypool1, effect="twoways", type="ghm")
        #### @@ => Results point to twoway fixed effects, when compared to pooling

            difftestrandom <- plm(lagpanel, data=fundpanelnocal, index=c("name", "fy"), model='random', effect='individual') 
            try1 <- plm(lagpanel, data=fundpanelnocal, index=c("name", "fy"), model='within', effect='individual') 
            phtest(try1, difftestrandom, data=fundpanelnocal)


                  difftest <- plm(lagpanel, data=fundpanelnocal, model='random', effect = "individual")
                    difftest1 <- plm(lagpanel, data=fp1, model='random', effect = "individual")
                    difftest2 <- plm(lagpanel, data=fp2, model='random', effect = "individual")
                    difftest3 <- plm(lagpanel, data=fp3, model='random', effect = "individual")

                  diffcoef <- coeftest(difftest, vcovHC(difftest, method='arellano', type="HC1"))
                    diffcoef1 <- coeftest(difftest1, vcovHC(difftest1, method="arellano", type="sss"))
                    diffcoef2 <- coeftest(difftest2, vcovHC(difftest2, method="arellano", type="sss"))
                    diffcoef3 <- coeftest(difftest3, vcovHC(difftest3, method="arellano", type="sss"))


stargazer(diff3, diff2, diff1, difftot)
texreg(list(diffcoef, diffcoef3, diffcoef2, diffcoef1), stars=c(0.001, 0.01, 0.05), digits=3, symbol="\\odot", caption.above=TRUE, caption="R Effects Panel Regressions 2001 - 2013", use.booktabs=TRUE)
texreg(list(difftest, difftest3, difftest2, difftest1), stars=c(0.001, 0.01, 0.05), digits=3, symbol="\\odot", caption.above=TRUE, caption="R Effects Panel Regressions 2001 - 2013", use.booktabs=TRUE)


#### Checking lag model with all underlying components

lagpanelcrazy <- dynformula(UAALpc ~ lag(financ,1) + lag(ARCpct,1) + lag(benhouse,1) + 
                              lag(avgbenefit,1) + lag(household,1) + lag(discperf,1) + lag(ass_inv,1) + lag(rtn5, 1) + 
                              ideol2 + lag(lpcdebt,1) + lintcost + lag(lintcost,1) + lag(interest,1) + + lag(genrev,1) +  lag(aa_risk,1) + 
                              lag(aa_eq,1) + lag(aa_alt,1) + lag(activeratio,1))

lagpanelcrazyass <- dynformula(actassets ~ lag(financ,1) + lag(ARCpct,1) + lag(benhouse,1) + 
                              lag(avgbenefit,1) + lag(household,1) + lag(discperf,1) + lag(ass_inv,1) + lag(rtn5, 1) + rtn5 + rtn1 + 
                              ideol2 + lag(lpcdebt,1) + lintcost + lag(lintcost,1) + lag(interest,1) + + lag(genrev,1) +  lag(aa_risk,1) + 
                              lag(aa_eq,1) + lag(aa_alt,1) + lag(activeratio,1))

lagpanelcrazyliab <- dynformula(actliabs ~ lag(financ,1) + lag(ARCpct,1) + lag(benhouse,1) + 
                              lag(avgbenefit,1) + lag(household,1) + lag(discperf,1) + lag(ass_inv,1) + lag(rtn5,1) + 
                              ideol2 + lag(lpcdebt,1) + lintcost + lag(lintcost,1) + lag(interest,1) + + lag(genrev,1) +  lag(aa_risk,1) + 
                              lag(aa_eq,1) + lag(aa_alt,1) + lag(activeratio,1))



crazytest <- plm(lagpanelcrazy, data=fundpanelcrazy, model='random', effect = "individual")
crazyasstest <- plm(lagpanelcrazyass, data=fundpanelcrazy, model='random', effect = "individual")
crazyliabtest <- plm(lagpanelcrazyliab, data=fundpanelcrazy, model='random', effect = "individual")


crazytest1 <- plm(lagpanelcrazy, data=fp1, model='random', effect = "individual")
crazytest2 <- plm(lagpanelcrazy, data=fp2, model='random', effect = "individual")
crazytest3 <- plm(lagpanelcrazy, data=fp3, model='random', effect = "individual")

crazyceof <- coeftest(crazytest, vcovHC(crazytest, method='arellano', type="HC1"))
crazyceof1 <- coeftest(crazytest1, vcovHC(crazytest1, method="arellano", type="sss"))
crazyceof2 <- coeftest(crazytest2, vcovHC(crazytest2, method="arellano", type="sss"))
crazyceof3 <- coeftest(crazytest3, vcovHC(crazytest3, method="arellano", type="sss"))


# Just general ideology scores

polo <- fundpanelnocal %>%
  group_by(fy) %>%
  summarise(polvol = sd(ideol2, na.rm=TRUE),
            polmean = mean(ideol2, na.rm=TRUE),
            cv = polvol/polmean)

