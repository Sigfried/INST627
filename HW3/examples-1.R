#---------------------- Example: Birth weight ---------------------------------- 
US_bw = 3.3
Poverty_bw = 2.8
N = 25
x_bar = 3.075
sd = 0.5
alpha = 0.05
  
df = N - 1
t = (x_bar - Poverty_bw)/(sd/sqrt(N))
p = pt(abs(t),df,lower.tail=FALSE)*2

# p < alpha
d = (x_bar - Poverty_bw)/sd

# p > alpha
library(pwr)
pwr.t.test(n=25,d=0.2,sig.level=0.05,type="one.sample")
pwr.t.test(n=25,d=0.4,sig.level=0.05,type="one.sample")
pwr.t.test(n=25,d=0.8,sig.level=0.05,type="one.sample")



#----------------------Example: A B testing ----------------------------------
N_A = 14
N_B = 16

x_A = 3.64 
sd_A = 0.55

x_B = 5.63
sd_B = 1.04

alpha = 0.05

df = min((N_A - 1), (N_B - 1))
t = (x_A - x_B)/sqrt(sd_A^2/N_A + sd_B^2/N_B)
pt(abs(t),df,lower.tail=FALSE)

# p < alpha
d = (x_A - x_B)/sqrt((sd_A^2 + sd_B^2)/2)

# p > alpha
library(pwr)
pwr.t.test(n=14,d=0.2,sig.level=0.05,type="two.sample", alternative="less")
pwr.t.test(n=14,d=0.4,sig.level=0.05,type="two.sample", alternative="less")
pwr.t.test(n=14,d=0.8,sig.level=0.05,type="two.sample", alternative="less")


# what if we had the data
website = read.csv("Dweownloads/ObamaWebsiteAB.csv")
w_or = website[website$webpage=="Original", "visitmin"]
w_fam = website[website$webpage=="Family", "visitmin"]
t.test(w_or, w_fam, alternative = "less")


#-------------------------- Example: ANOVA ---------------------------
library(plyr)
cdata <- ddply(website, c("webpage"), summarise,
               N    = length(visitmin),
               mean = mean(visitmin),
               sd   = sd(visitmin),
               se   = sd / sqrt(N),
               ci.lower   = mean - 2*se,
               ci.upper   = mean + 2*se
)
               
library(ggplot2)
ggplot(cdata,aes(x=webpage,y=mean))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=ci.lower,ymax=ci.upper),width=.2)

fit <- aov(visitmin~webpage, data=website)
summary(fit)

pf(6.14, df1 = 2, df2 = 27, lower.tail = FALSE)

# post hoc test
# Tukey Honest Significant Differences
TukeyHSD(fit)
plot(TukeyHSD(fit))
