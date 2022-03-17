library(dplyr)
library(reshape)
library(ggplot2)
library(scales)
library(Hmisc)
years <- (2001:2013)


###### Density plots for UAALpc, with no Cal

temp <- select(newfundingsub3, fy, UAALpc)
temp <- melt(temp, id="fy")
temp <- select(temp, fy, value)


fundingpre <- ggplot(subset(temp, !is.na(value) & fy < 2009), aes(x = value, colour = as.factor(fy),
                    fill = as.factor(fy))) + geom_density(alpha = 0.05) +  scale_color_hue(l=50, c=30) + 
                    scale_fill_hue(l=80, c=50) + theme_minimal() + theme(panel.grid(v=0)) +
                    scale_x_continuous(label = dollar) + labs(x = "", y = "", 
                              title = "Funding Gap Per Capita: Pre Crisis") +
                                    theme(legend.title=element_blank())
fundingpre

funding01 <- ggplot(subset(temp, !is.na(value) &  fy==2001), aes(x = value, colour = as.factor(fy),
                    fill = as.factor(fy))) + geom_density(alpha = 0.05) + scale_color_hue(l=50, c=30) + 
                    scale_fill_hue(l=80, c=50) + 
                    theme_minimal() + geom_vline(xintercept=0) +  
                    scale_x_continuous(label = dollar, limits=c(-3000, 7500)) + labs(x = "", y = "") +
  theme(legend.position="none")
funding01

funding <- ggplot(subset(temp, !is.na(value)), aes(x = value, colour = as.factor(fy),
                    fill = as.factor(fy))) + geom_density(alpha = 0.05) + scale_color_hue(l=50, c=30) + 
                    scale_fill_hue(l=80, c=50) +   
                    theme_minimal() + geom_vline(xintercept=0) + 
                    scale_x_continuous(label = dollar, limits=c(-3000, 7500)) + labs(x = "", y = "") +
                    theme(legend.position="none")
funding

funding + annotate("text", x = 370, y=0.00122, label = "2001", size=3) + 
  annotate("text", x = 480, y=0.00095, label = "2003", size=3) +
  annotate("text", x = 750, y=0.00077, label = "2005", size=3) +
  annotate("text", x = 900, y=0.00067, label = "2007", size=3) +
  annotate("text", x = 1150, y=0.00046, label = "2009", size=3) +
  annotate("text", x = 1380, y=0.00040, label = "2011", size=3) +
  annotate("text", x = 1650, y=0.00035, label = "2013", size=3) 

funding + annotate("rect", xmin=-Inf, xmax=0, ymin=0, ymax=Inf, alpha=0.3)


fundingends <- ggplot(subset(temp, !is.na(value) & fy == 2001 | fy==2009 | fy == 2013), aes(x = value, colour = as.factor(fy),
                            fill=as.factor(fy))) + geom_density(position="identity", alpha=0.2)

######### UAAL unadjusted

tempunadj <- select(newfundingsub2, fy, UAAL)
tempunadj <- melt(tempunadj, id="fy")
tempunadj <- select(tempunadj, fy, value)

fundingunadj <- ggplot(subset(tempunadj, !is.na(value)), aes(x = value, colour = as.factor(fy),
                      fill = as.factor(fy))) + geom_density(alpha = 0.05) + scale_color_hue(l=50, c=30) + 
                scale_fill_hue(l=80, c=50) +  theme_minimal() + theme(panel.grid(v=0)) +
                scale_x_continuous(label = dollar) + labs(x = "", y = "", 
                                                          title = "Funding Gap, Unadjusted: 2001 - 2013") +
                theme(legend.title=element_blank())
fundingunadj

fundingendsunadj01 <- ggplot(subset(tempunadj, !is.na(value) & fy == 2001), aes(x = value, colour = as.factor(fy),
  fill = as.factor(fy))) + geom_density(alpha = 0.1) + scale_color_hue(l=50, c=30) + 
  scale_fill_hue(l=80, c=50) +  theme_minimal() + theme(panel.grid(v=0)) +
  scale_x_continuous(label = dollar) + labs(x = "", y = "", 
                                            title = "Funding Gap in 2001") +
  theme(legend.title=element_blank())
fundingendsunadj01

fundingendsunadj0109 <- ggplot(subset(tempunadj, !is.na(value) & fy == 2001 | fy == 2009), aes(x = value, colour = as.factor(fy),
                                                                              fill = as.factor(fy))) + geom_density(alpha = 0.1) + scale_color_hue(l=50, c=30) + 
  scale_fill_hue(l=80, c=50) +  theme_minimal() + theme(panel.grid(v=0)) +
  scale_x_continuous(label = dollar) + labs(x = "", y = "", 
                                            title = "Funding Gap in 2001, 2009") +
  theme(legend.title=element_blank())
fundingendsunadj0109

fundingendsunadj0113 <- ggplot(subset(tempunadj, !is.na(value) & fy == 2001 | fy == 2009 | fy == 2013), aes(x = value, colour = as.factor(fy),
                                                                              fill = as.factor(fy))) + geom_density(alpha = 0.1) + scale_color_hue(l=50, c=30) + 
  scale_fill_hue(l=80, c=50) + theme_minimal() + theme(panel.grid(v=0)) +  
  scale_x_continuous(label = dollar) + labs(x = "", y = "", 
                                            title = "Funding Gap in 2001, 2009, 2013") +
  theme(legend.title=element_blank())
fundingendsunadj0113

###### Quantiles per capita 

iles <- matrix(,13, 5)
for(i in 1:13){
  t <- filter(newfundingsub3, fy == (2000 + i))
  iles[i,] <- quantile(-t$UAALpc, probs = c(.9, .75, .5, .25, .1), na.rm=TRUE)
}
rownames(iles) <- years
colnames(iles) <- c("90th", "75th", "50th", "25th", "10th")
ilesmelt <- melt(iles)
colnames(ilesmelt) <- c("year", "Percentile", "value")

  
quants <- ggplot(ilesmelt, aes(x=as.factor(year), group=Percentile)) + scale_color_hue(l=50, c=50) + 
  theme_minimal() + theme(panel.grid(v=0)) +  
  geom_line(aes(y = value, col=Percentile), size=1) + guides(colour = guide_legend(reverse=T)) + 
  geom_point(aes(y = value, col=Percentile), size=2, shape = 21) + 
  scale_y_continuous(label = dollar) + labs(x = "", y = "", 
                                        title = "Funding Surplus/Gap per capita 2001 - 2013")
quants
  
quants + annotate("rect", xmin = as.factor(2001), xmax = as.factor(2013), ymin = 0, ymax = Inf, alpha = .2) + 
          annotate("text", x=as.factor(2007), y=350, label="Surplus", fontface=2)

###### Quantiles for actratio, unweighted

ileratio <- matrix(,13, 5)
for(i in 1:13){
  t <- filter(newfundingsub3, fy == (2000 + i))
  ileratio[i,] <- quantile(t$actratio, probs = c(.9, .75, .5, .25, .1), na.rm=TRUE)
}
rownames(ileratio) <- years
colnames(ileratio) <- c("90th", "75th", "50th", "25th", "10th")
ileratiomelt <- melt(ileratio)
colnames(ileratiomelt) <- c("year", "Percentile", "value")


quantratios <- ggplot(ileratiomelt, aes(x=as.factor(year), group=Percentile)) + scale_color_hue(l=50, c=50) + 
  theme_minimal() +   
  geom_line(aes(y = value, col=Percentile), size=1) + guides(colour = guide_legend(reverse=T)) + 
  geom_point(aes(y = value, col=Percentile), size=2, shape = 21) + 
  labs(x = "", y = "", title = "Actuarial Funded Ratio 2001 - 2013\nUnweighted") #+ ylim(0.5, 1.3)
quantratios

###### Quantiles for actratio, popn weighted

ileratiowtd <- matrix(,13, 5)
for(i in 1:13){
  t <- filter(newfundingsub3, fy == (2000 + i))
  ileratiowtd[i,] <- wtd.quantile(t$actratio, weights=t$population, probs = c(.9, .75, .5, .25, .1), na.rm=TRUE)
}
rownames(ileratiowtd) <- years
colnames(ileratiowtd) <- c("90th", "75th", "50th", "25th", "10th")
ileratiowtdmelt <- melt(ileratiowtd)
colnames(ileratiowtdmelt) <- c("year", "Percentile", "value")

quantratioswtd <- ggplot(ileratiowtdmelt, aes(x=as.factor(year), group=Percentile)) + scale_color_hue(l=50, c=50) + 
  theme_minimal() +   
  geom_line(aes(y = value, col=Percentile), size=1) + guides(colour = guide_legend(reverse=T)) + 
  geom_point(aes(y = value, col=Percentile), size=2, shape = 21) + 
  labs(x = "", y = "", title = "Actuarial Funded Ratios 2001 - 2013\nWeighted by Population") #+ ylim(0.5, 1.3)
quantratioswtd

quantratioswtd + annotate("rect", xmin = as.factor(2001), xmax = as.factor(2013), ymin = -Inf, ymax = 1, alpha = .2) + 
                  annotate("rect", xmin = as.factor(2001), xmax = as.factor(2013), ymin = -Inf, ymax = 0.8, alpha = .2, fill = 'red')


####### Distributions of financialization


finc <- select(newfundingsub3, fy, financ)
finc <- melt(finc, id="fy")
finc <- select(finc, fy, value)

financialneg <- ggplot(subset(finc, !is.na(value) & fy == 2001 | fy == 2002 | fy ==2009), aes(x = value, colour = as.factor(fy),
                    fill = as.factor(fy))) + geom_density(alpha = 0.05) + scale_color_hue(l=50, c=30) + 
                    scale_fill_hue(l=80, c=50) +theme_minimal() + geom_vline(xintercept=0) + 
                    labs(x = "", y = "", title = "Financialization: Negative Years") #+
                    #theme(legend.position="none")
financialneg
  

ilefinc <- matrix(,13, 5)
for(i in 1:13){
  t <- filter(newfundingsub3, fy == (2000 + i))
  ilefinc[i,] <- quantile(t$financ, probs = c(.9, .75, .5, .25, .1), na.rm=TRUE)
}
rownames(ilefinc) <- years
colnames(ilefinc) <- c("90th", "75th", "50th", "25th", "10th")
ilefincmelt <- melt(ilefinc)
colnames(ilefincmelt) <- c("year", "Percentile", "value")


quantfinc <- ggplot(ilefincmelt, aes(x=as.factor(year), group=Percentile)) + scale_color_hue(l=50, c=50) + 
  theme_minimal() +   
  geom_line(aes(y = value, col=Percentile), size=1) + guides(colour = guide_legend(reverse=T)) + 
  geom_point(aes(y = value, col=Percentile), size=2, shape = 21) + 
  labs(x = "", y = "", title = "Financialization 2001 - 2013") #+ ylim(0.5, 1.3)
quantfinc

##### Looking for persistence: Stanky Charts

topsgap <- list()
botsgap <- list()
for(k in 1:13){
  t <- filter(newfundingsub3, fy == (2000 + k))
  t1 <- filter(t, UAALpc > iles[k,1] | UAALpc == iles[k,1])
  b1 <- filter(t, UAALpc < iles[k,5] | UAALpc == iles[k,5])
  topsgap[[k]] <- t1$name
  botsgap[[k]] <- b1$name
}

topfins <- as.data.frame(cbind(sapply(tops, "[", 1:11)))
botfins <- as.data.frame(cbind(sapply(bots, "[", 1:11)))


worstfunds <- as.data.frame(cbind(sapply(topsgap, "[", 1:11)))
bestfunds <- as.data.frame(cbind(sapply(botsgap, "[", 1:11)))

##### Persistence and alluvial chart

alluvs <- as.data.frame(t$name)
for(a in 1:13){
  r <- filter(newfundingsub3, fy == (2000 + a))
  r <-mutate(r, 
             allu = ifelse(UAALpc >= iles[a,2], 4, 
                           ifelse(UAALpc < iles[a,2] & UAALpc >= iles[a,3], 3, 
                                  ifelse(UAALpc < iles[a,3] & UAALpc >= iles[a,4], 2, 1))))
  alluvs <- cbind(alluvs, r$allu)
}
colnames(alluvs) <- c("id", letters[1:13])

alls <- read.csv("~/Dropbox/Pensions/R Code/alls.csv")

alluvial(alls[,1:4], freq=alls$freq,
         col=ifelse(alls$X2001 == 1, "red3", 
                    ifelse(alls$X2001 == 4, "darkgreen", 
                           ifelse(alls$X2001 == 2, "blue", "orange"))),
         hide = alls$freq==0,
         cex=0.8,
         axis_labels=c(2001, 2005, 2009, 2013)
)

alluvial(alls[,1:4], freq=alls$freq,
         col=ifelse(alls$X2001 == 1, "red3", 
                    ifelse(alls$X2001 == 4, "darkgreen", "lightgray")),
                        #   ifelse(alls$X2001 == 2, "blue", "orange"))),
         hide = alls$freq==0,
         cex=0.8,
         axis_labels=c(2001, 2005, 2009, 2013),
         layer=(alls$X2001==2 | alls$X2001==3)
)


alluvial(alls[,1:4], freq=alls$freq,
         col=ifelse(alls$X2013 == 1, "red3", 
                    ifelse(alls$X2013 == 4, "darkgreen", "lightgray")),
                           #ifelse(alls$X2013 == 2, "blue", "orange"))),
         hide = alls$freq==0,
         cex=0.8,
         axis_labels=c(2001, 2005, 2009, 2013),
         layer=(alls$X2013==2 | alls$X2013==3)
)

## NExt - axis labels

nodes <- read.csv("~/Dropbox/Pensions/R Code/nodes.csv")
edges <- read.csv("~/Dropbox/Pensions/R Code/edges.csv")

riv <- makeRiver(nodes, edges, node_xpos = nodes$x)

