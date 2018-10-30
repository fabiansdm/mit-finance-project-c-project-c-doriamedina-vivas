library(tidyverse)
library(readr)
library(readxl)
library(xts)
library(moments)
library(stargazer)
MSFT <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/returns.xlsx")
rb <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/rollingbeta.xlsx")
sp <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/sp500.xlsx")
names(sp) <- c("d","id_bench","nt","domain","sp")
sptr <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/sptr.xlsx")
names(sptr) <- c("d","id_bench","nt","domain","sptr")
vix <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/vix.xlsx")
names(vix) <- c("d","id_bench","nt","domain","vix")
ff <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/famafrench.xlsx")
q23 <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/q23.xlsx")
q3e <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/q3e.xlsx")
q3f <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/q3f.xlsx")
q4 <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/q4.xlsx")
wei <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/Weights.xlsx")
index <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/indexmember.xlsx")
#Fix date
MSFT <- cbind(MSFT[,1],MSFT[,5]) %>% mutate(d=as.Date.character(d,format="%Y-%m-%d"))
rb <- cbind(rb[,1],rb[,5]) %>% mutate(d=as.Date.character(d,format="%Y-%m-%d"))
market <- cbind(sp[,1],sp[,5],sptr[,5],vix[,5]) %>% mutate(d=as.Date.character(d,format="%Y-%m-%d"))
rm(sp,sptr,vix)
ff <- ff %>% mutate(d=as.Date.character(d,format="%Y-%m-%d"))
ff <- ff[-(1:252),]
q23 <- q23 %>% mutate(`Row Labels`=as.Date.character(`Row Labels`,format="%m/%d/%Y"))
names(q23) <- c("d","Long","Short","Total")
q3e <- q3e %>% mutate(d=as.Date.character(d,format="%m/%d/%Y"))
q3f <- q3f %>% mutate(d=as.Date.character(d,format="%m/%d/%Y"))
q4 <- q4 %>% mutate(d=as.Date.character(d,format="%m/%d/%Y"))
index <- index[,-(2:3)] %>% mutate(d=as.Date.character(d,format="%Y-%m-%d"))
wei <- cbind(wei[,1],wei[,4]) %>% mutate(d=as.Date.character(d))
#Q1
#A
beta <- cbind(rb[rb>"2007-12-31",],market[market>"2007-12-31",][1:505,2:4])
names(beta) <- c("d","beta","sp","sptr","vix")
D1A <- ggplot(beta,mapping=aes(x=d))+geom_line(mapping=aes(y=beta,color="Beta"))
#B
beta <- cbind(beta,MSFT[MSFT>"2007-12-31",][,2])
names(beta) <- c("d","beta","sp","sptr","vix","MSFT")
D1B <- ggplot(beta,mapping=aes(x=sptr))+geom_point(mapping=aes(y=MSFT))+geom_smooth(mapping=aes(y=MSFT),method=lm)
#C
D1C <- ggplot(beta,mapping=aes(x=vix))+geom_point(mapping=aes(y=MSFT))+geom_smooth(mapping=aes(y=MSFT),method=lm)
mmv <- summary(lm(MSFT~sptr+vix,beta))
#Q2
dq2 <- q23[1:1004,]
stats <- function(X){
  a <- mean(X)*252
  b <- sd(X)*sqrt(252)
  c <- a/b
  matrix(c(a,b,c),nrow=3,ncol=1)
}
#A
pma <- as.data.frame(cbind(c("Mean","SD","Sharpe Ratio"),stats(dq2$Long),
                             stats(dq2$Short),stats(dq2$Total)))
names(pma) <- c("","Long","Short","Total")
#B
dq2 <- cbind(dq2,ff[1:1004,2:6])
dq2 <- dq2 %>% mutate(lrf=Long-rf) %>% 
  mutate(srf=Short-rf) %>% mutate(trf=Total-rf)
l <- summary(lm(lrf~mktrf,dq2))
s <- summary(lm(srf~mktrf,dq2))
t <- summary(lm(trf~mktrf,dq2))
l;s;t
#C
lff <- summary(lm(lrf~mktrf+smb+hml+umd,dq2))
sff <- summary(lm(srf~mktrf+smb+hml+umd,dq2))
tff <- summary(lm(trf~mktrf+smb+hml+umd,dq2))
lff;sff;tff
#D
dq2d <- dq2[,1:4] %>% arrange(Total)
dq2d <- cbind(seq(1,1004,1),dq2d)
names(dq2d) <- c("Sorting","d","Long","Short","Total")
ggplot(dq2d,mapping=aes(x=Sorting))+
  geom_col(mapping=aes(y=Total))
sum(ifelse(dq2d$Total>0,1,0))
sum(ifelse(dq2d$Total<0,1,0))
sum(ifelse(dq2d$Total==0,1,0))
medwin <- median(subset(dq2d,Total>0)[,5])
medlos <- median(subset(dq2d,Total<0)[,5])
#E

#Q3
dq3 <- q23[1005:2263,]
#A
paa <- as.data.frame(cbind(c("Mean","SD","Sharpe Ratio"),
                           stats(dq3$Long),
                           stats(dq3$Short),stats(dq3$Total)))
names(paa) <- c("","Long","Short","Total")
#B
dq3 <- cbind(dq3,ff[1005:2263,2:6])
dq3 <- dq3 %>% mutate(lrf=Long-rf) %>%
  mutate(srf=Short-rf) %>% mutate(trf=Total-rf)
names(dq3)
l3 <- summary(lm(lrf~mktrf,dq3))
s3 <- summary(lm(srf~mktrf,dq3))
t3 <- summary(lm(trf~mktrf,dq3))
l3;s3;t3
#C
dq3c <- dq3[,1:4] %>% arrange(Total)
dq3c <- cbind(seq(1,2263-1005+1,1),dq3c)
names(dq3c) <- c("Sorting","d","Long","Short","Total")
ggplot(dq3c,mapping=aes(x=Sorting))+
  geom_col(mapping=aes(y=Total))
sum(ifelse(dq3c$Total>0,1,0))
sum(ifelse(dq3c$Total<0,1,0))
sum(ifelse(dq3c$Total==0,1,0))
medwin3 <- median(subset(dq3c,Total>0)[,5])
medlos3 <- median(subset(dq3c,Total<0)[,5])
#D
#E
q3e[is.na(q3e)] <- 0
d3e <- q3e %>% subset(format(as.Date(d),"%Y")==2006)
sic <- abs(cumsum(d3e[,-1])[251,])
sic <- sic[,-21]/as.numeric(sic[,21])
sic <- round(as.matrix(sic[,1:10])+as.matrix(sic[,11:20]),4)
sic <- as.tibble(cbind(c("Agriculture","Construction", 
                         #Agriculture, Forestry, and Fishing
                         "Finance","Manufacturing", 
                         #Finance, Insurance, and Real Estate
                         "Mining","Retail","Services",
                         "Transportation",
                         #Transportation, Communications, 
                         #Electric, Gas, and Sanitary Services
                         "Wholesale","Total"),t(sic)))
names(sic) <- c("SIC","Percent")
sic <- sic[-10,]
ggplot(sic,mapping=aes(SIC))+geom_col((mapping=aes(y=Percent)))
#F
q3f[is.na(q3f)] <- 0
d3f <- q3f %>% subset(format(as.Date(d),"%Y")==2006)
gics <- abs(cumsum(d3f[,-1])[251,])
gics <- gics[,-25]/as.numeric(gics[,25])
gics <- round(as.matrix(gics[,1:12])+as.matrix(gics[,13:24]),4)
gics <- as.tibble(cbind(c("NA","Energy","Materials",
                          "Industrials","Consumer Discretionary",
                          "Consumer Staples","Health Care",
                          "Financials","IT","Telecommunication",
                          "Utilities","Total"),t(gics)))
names(gics) <- c("GICS","Percent")
gics <- gics[-12,]
gics <- gics %>% arrange(Percent)
ggplot(gics,mapping=aes(GICS))+geom_col((mapping=aes(y=Percent)))
#Q4
q4[is.na(q4)] <- 0
q4 <- q4[1005:2263,]
#A
mq4 <- matrix(c(max(q4$L_NA+q4$S_NA),max(q4$L_Energy+q4$S_Energy),
                max(q4$L_Materials+q4$S_Materials),
                max(q4$L_Industrials+q4$S_Industrials),
                max(q4$`L_Consumer Discretionary`+q4$`S_Consumer Discretionary`),
                max(q4$`L_Consumer Staples`+q4$`S_Consumer Staples`),
                max(q4$`L_Health Care`+q4$`S_Health Care`),
                max(q4$L_Financials+q4$S_Financials),
                max(q4$`L_Information Technology`+q4$`S_Information Technology`),
                max(q4$`L_Telecommunication Services`+q4$`S_Telecommunication Services`),
                max(q4$L_Utilities+q4$S_Utilities),
                min(q4$L_NA+q4$S_NA),min(q4$L_Energy+q4$S_Energy),
                min(q4$L_Materials+q4$S_Materials),
                min(q4$L_Industrials+q4$S_Industrials),
                min(q4$`L_Consumer Discretionary`+q4$`S_Consumer Discretionary`),
                min(q4$`L_Consumer Staples`+q4$`S_Consumer Staples`),
                min(q4$`L_Health Care`+q4$`S_Health Care`),
                min(q4$L_Financials+q4$S_Financials),
                min(q4$`L_Information Technology`+q4$`S_Information Technology`),
                min(q4$`L_Telecommunication Services`+q4$`S_Telecommunication Services`),
                min(q4$L_Utilities+q4$S_Utilities)),nrow=11,ncol=2)
#B
lb <- as.matrix(q4[q4=="2008-9-15",])
lb <- cbind(lb[,2:13],lb[,14:25])
lb <- cbind(c("NA","Energy","Materials","Industrials",
                          "Consumer Discretionary","Consumer Staples","Health Care",
                          "Financials","IT","Telecommunication","Utilities","Total"),lb)
lb <- as.tibble(lb)
names(lb) <- c("GICS","Long","Short")
lb <- lb %>% 
  mutate(Long=as.numeric(Long)) %>% mutate(Short=as.numeric(Short)) %>%
  mutate(Total=Long+Short)
#C
lbc <- as.matrix(q4[q4=="2007-2-27",])
lbc <- cbind(lbc[,2:13],lbc[,14:25])
lbc <- cbind(c("NA","Energy","Materials","Industrials",
              "Consumer Discretionary","Consumer Staples","Health Care",
              "Financials","IT","Telecommunication","Utilities","Total"),lbc)
lbc <- as.tibble(lbc)
names(lbc) <- c("GICS","Long","Short")
lbc <- lbc %>% 
  mutate(Long=as.numeric(Long)) %>% mutate(Short=as.numeric(Short)) %>%
  mutate(Total=Long+Short)
#D

#Q5
#Data
dj <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/djia.xlsx")
dj1 <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/
                  Financial Data Science and Computing I/
                  Projects/djia9.xlsx")
dj3 <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/djia11.xlsx")
dj12 <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/dj92.xlsx")
dj32 <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/dj112.xlsx")
dj1 <- rbind(dj1,dj12)
dj3 <- rbind(dj3,dj32)
in1 <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/nt9.xlsx")
in3 <- read_excel("~/Desktop/MIT/MIT Classes/Fall 2018/Financial Data Science and Computing I/Projects/nt11.xlsx")
#Fix
dj1 <- dj1[,-(2:4)] %>% mutate(d=as.Date.character(d))
dj3 <- dj3[,-(2:4)] %>% mutate(d=as.Date.character(d))
volp <- cbind(dj1,dj3[,2])
names(volp) <- c("d","v1","v3")
rm(dj1,dj3,dj12,dj32)
in1 <- cbind(in1[,5],in1[,(2:3)])
in3 <- cbind(in3[,5],in3[,(2:3)])
dj <- cbind(dj[,1],dj[,3:4])
names(in1) <- c("d","v1","id")
names(in3) <- c("d","v3","id")
names(dj) <- c("d","id","v")
in1 <- in1 %>% mutate(d=as.Date.character(d)) %>% spread(id,v1)
in3 <- in3 %>% mutate(d=as.Date.character(d)) %>% spread(id,v3)
dj <- dj %>% mutate(d=as.Date.character(d)) %>% spread(id,v)
in1[is.na(in1)] <- 0
in3[is.na(in3)] <- 0
dj[is.na(dj)] <- 0

#ABCD
weights <- as.matrix(dj[,-1])
vol1 <- as.matrix(in1[,-1])
vol3 <- as.matrix(in3[,-1])

sigma1_0 <- dj[,1:2]
sigma1_1 <- dj[,1:2]
sigma3_0 <- dj[,1:2]
sigma3_1 <- dj[,1:2]
names(sigma1_0) <- c("d","sigma1_0")
names(sigma1_1) <- c("d","sigma1_1")
names(sigma3_0) <- c("d","sigma3_0")
names(sigma3_1) <- c("d","sigma3_1")
for (i in 1:504){
  sigma1_0[i,2] <- (vol1[i,])^2%*%(weights[i,]^2)
  sigma1_1[i,2] <- (vol1[i,]%*%weights[i,])^2
  sigma3_0[i,2] <- (vol3[i,])^2%*%(weights[i,]^2)
  sigma3_1[i,2] <- (vol3[i,]%*%weights[i,])^2
}
all <- right_join(volp,sigma1_0,by="d") %>% 
  right_join(sigma1_1,by="d") %>%  right_join(sigma3_0,by="d") %>% 
  right_join(sigma3_1,by="d") %>% mutate(v1=v1*v1) %>%
  mutate(v3=v3*v3) %>% mutate(rho_1=(v1-sigma1_0)/(sigma1_1-sigma1_0)) %>%
  mutate(rho_3=(v3-sigma3_0)/(sigma3_1-sigma3_0))

month1 <- ggplot(all,mapping=aes(x=d))+
  geom_line(mapping=aes(y=v1,color="Realized Variance"))+
  geom_line(mapping=aes(y=rho_1,color="Average Correlation"))+
  geom_line(mapping=aes(y=sigma1_1,color="Variance 1"))+
  geom_line(mapping=aes(y=sigma1_0,color="Variance 0"))

month3 <- ggplot(all,mapping=aes(x=d))+
  geom_line(mapping=aes(y=v3,color="Realized Variance"))+
  geom_line(mapping=aes(y=rho_3,color="Average Correlation"))+
  geom_line(mapping=aes(y=sigma3_1,color="Variance 1"))+
  geom_line(mapping=aes(y=sigma3_0,color="Variance 0"))

#E
e <- matrix(c(0,"2008-11-21","1month",
              max(all$rho_1),"3month",
              max(all$rho_3)),nrow=2,ncol=3)
stargazer(e)
