#############Math 189/289C - HW1
#############Date: Jan 24, 2017

###Using graphical methods to compare the two distributions of birth weight.

data_file = "hw1/baby_data_simple.txt"
library("ggplot2")
dat <- read.table(data_file, header=T)

#clean data e.g remove missing info
dat <- dat[dat$smoke <= 1 & 
             dat$bwt < 999 & 
             dat$gestation < 999 &  
             dat$age < 99 & 
             dat$weight < 999 &
             dat$height < 99,]
dat$smoke <- factor(dat$smoke)

##box plot
p1 <- ggplot(dat, aes(x=smoke, y=bwt, group=smoke, fill=smoke)) +
  geom_boxplot() + #to ignore outliers use: outlier.shape = NA
  scale_fill_discrete(name="Experimental\nCondition",
                      breaks=c("0", "1" ),
                      labels=c("Non-smoker", "Smoker")) +
  scale_x_discrete(name="Mother's smoking status", labels=c("Non-smoker", "Smoker")) +
  scale_y_continuous(name="Baby birth weight (ounces)") +
  ggtitle("Box-Plot of Baby Birth Weight Distributions")
p1
ggsave("hw1/baby_data_simple_boxplot.jpeg", plot=p1, width = 6, height = 4)

# q-q plot against normal distribution
p2 <- ggplot(dat, aes(sample=bwt, group=smoke, color=smoke))+
  stat_qq() +
  scale_color_discrete(name="Mother's smoking status",
                       breaks=c("0", "1" ),
                       labels=c("Non-smoker", "Smoker")) +
  labs(title="Quantile-Quantile Plot of Baby Birth Weight Distributions",
       x = "Theoretical quantiles",
       y = "Baby birth weight (ounces)") +
  geom_abline(slope=1)
p2
ggsave("hw1/baby_data_simple_qqplot.jpeg", plot=p2, width = 6, height = 4)

# q-q plot of smoker and non-smoker samples
jpeg("hw1/baby_data_simple_qqplot2.jpeg")
qqplot(x=dat[dat$smoke==0,]$bwt, dat[dat$smoke==1,]$bwt, pch=20,
       main="Quantile-Quantile Plot of Baby Birth Weight Distributions",
       xlab = "Baby birth weight (ounces) from non-smokers",
       ylab = "Baby birth weight (ounces) from smokers") 
abline(c(0,1), col="red")
dev.off()

#data setup
data<-read.table('babies..txt', header=TRUE)
dataMinusIrregular <-subset(data, gestation < 999 & smoke < 9 & height <99 & weight <999 & age<99)
newData.nonsmoker <-subset(dataMinusIrregular, smoke ==0)
newData.smoker<-subset(dataMinusIrregular, smoke==1)
set.seed(5)
mySample <-data.smoker[sample(1:nrow(data.smoker), 459, replace=FALSE),]
newCombo<- list(mySample$bwt, newData.smoker$bwt)

#comparison histogram
multhist(newCombo, freq=TRUE, main="Comparison Birth Weight", ylim = c(0, 200), xlab="Birth Weight(Oz)", ylab = "Frequency", col=c("red", "yellow"), 
         breaks=seq(45, 200, by=10), names.arg=ranges)
legend(x='topright', c("Smokers", "Non-Smokers"), fill=c("yellow", "red"))
ranges <-c("50-59", "60-69", "70-79", "80-89", "90-99", "100-109", "110-119", "120-129", "130-139", "140-149", "150-159", "160-169", "170-179", "180-189", "190-199")

#Histogram of non-smokers
ggplot(mySample, aes(bwt), binwidth=5) + geom_histogram(aes(fill='red')) + ylab('Frequency') + xlab('Birth Weight(Oz)') + ggtitle('Birth Weight of Babies With Mothers Who Do Not Smoke') 
+ guides(fill=FALSE) + scale_x_continuous(limits = c(50, 200)) + scale_y_continuous(limits=c(0, 75))

#histogram of smokers
ggplot(newData.smoker, aes(bwt), binwidth=5) + geom_histogram(aes(fill='red')) + ylab('Frequency') + xlab('Birth Weight(Oz)') + ggtitle('Birth Weight of Babies With Mothers Who Do Smoke') 
+ guides(fill=FALSE) + scale_x_continuous(limits = c(50, 200)) + scale_y_continuous(limits=c(0, 75))

  
# Import Simple Babies Dataset - Data cleanup done in excel
  df <- read.csv("babies1.csv")

# Segment dataset into two, nonsmokers and smokers
dfnonSmoke <- df[which(df$smoke == 0),]
dfSmoke <- df[which(df$smoke == 1),]

# Average Birthweight of both subsets
avgbwtSmoke <- mean(dfSmoke$bwt)
avgbwtnonSmoke <- mean(dfnonSmoke$bwt)

# Median Birthweight of both subsets
medbwtSmoke <- median(dfSmoke$bwt)
medbwtnonSmoke <- median(dfnonSmoke$bwt)

# Variance of Birthweight of both subsets
varbwtSmoke <- var(dfSmoke$bwt)
varbwtnonSmoke <- var(dfnonSmoke$bwt)

# Standard Deviation of Birthweight of both subsets
stdbwtnonSmoke <- sqrt(varbwtnonSmoke)
stdbwtsmoke <- sqrt(varbwtSmoke)

# Max, Min, amd Mode of Birthweight of both subsets
maxbwtnonSmoke <- max(dfnonSmoke$bwt)
maxbwtSmoke <- max(dfSmoke$bwt)
minbwtSmoke <- min(dfSmoke$bwt)
minbwtnonSmoke <- min(dfnonSmoke$bwt)
modebwtSmoke <- mode(dfSmoke$bwt)
modebwtnonSmoke <- mode(dfnonSmoke$bwt)

# Import library for skewness and kurtosis
library(moments)

# Skewness of Birthweight of both subsets
skewbwtnonSmoke <- skewness(dfnonSmoke$bwt)
skewbwtSmoke <- skewness(dfSmoke$bwt)

# Kurtosis of Birthweight of both subsets
kurtosisbwtnonSmoke <- kurtosis(dfnonSmoke$bwt)
kurtosisbwtSmoke <- kurtosis(dfSmoke$bwt)

# Quantiles of Birthweight of both subsets
quantilebwtnonSmoke <- quantile(dfnonSmoke$bwt)
quantilebwtSmoke <- quantile(dfSmoke$bwt)

library(ggplot2)
library(moments)
dat<-read.table("Documents/graduate school/2017winter/math289/hw1/babies..txt", header = T)
head(dat)
dim(dat)
summary(dat$smoke) #ten sample labeled as 9
summary(dat$gestation)
dat.v2<-dat[!(dat$smoke == 9 | dat$gestation == 999 | dat$age == 99 | dat$height == 99 | dat$weight == 999),]
dim(dat.v2)
#Separate data into two groups based on if the mothers smoked or not during pregnancy
dat.nonsmoke<-dat.v2[dat.v2$smoke == 0, 1]
length(dat.nonsmoke)#715
dat.smoke<-dat.v2[dat.v2$smoke == 1, 1]
length(dat.smoke) #459
#Obtain the kurtosis coefficients for two groups separately
smoke.kurtosis<-kurtosis(dat.smoke)
smoke.kurtosis #2.953799
nonsmoke.kurtosis<-kurtosis(dat.nonsmoke) #4.016843
#Perform simulation for 459 and 715 data from normal distribution by 1000 times
B = 1000
normal_kurtosis1<-sample(B)
normal_kurtosis2<-sample(B)
#t_kurtosis<-sample(B)
for (i in 1:B){
  normal_kurtosis1[i]<-kurtosis(rnorm(length(dat.smoke)))
  normal_kurtosis2[i]<-kurtosis(rnorm(length(dat.nonsmoke)))
}
#Use ggplot2 to plot the distribution of kurtosis coefficients
merged.kurtosis<-data.frame(normal_kurtosis1, normal_kurtosis2)
p1<-ggplot(merged.kurtosis) + geom_histogram(aes(normal_kurtosis1)) + labs(title = "Distribution of kurtosis coefficients from 459 normally disributed sample") + xlab("Kurtosis Coefficient") + theme()
p2<-ggplot(merged.kurtosis) + geom_histogram(aes(normal_kurtosis2), fill = "blue") +labs(title = "Distribution of kurtosis coefficients from 715 normally disributed sample") + xlab("Kurtosis Coefficient")
#Simulation to compare incidence
nonsmoker<-dat[dat$smoke==0,]
freq<-numeric(1000)
for (i in 1:1000){
  subset.nonsmoke<-sample((1:length(dat.nonsmoke)), 300)
  sim.bwt<-nonsmoker[subset.nonsmoke,1]
  freq[i]<-sum(sim.bwt<88)/300
}
freq.output<-data.frame(freq)
p1<-ggplot(freq.output) + geom_histogram(aes(freq)) + labs(title = "Distribution of proportions of low birth weights") + xlab("Percentage") + theme(plot.title = element_text(size = 15))


data1<-read.table("babies.txt",header=TRUE)
length(data1)

irreg.index1<-which(data1$gestation==999)
irreg.index2<-which(data1$age==9)
irreg.index3<-which(data1$height==99)
irreg.index4<-which(data1$weight==999)
irreg.index5<-which(data1$smoke==9)
data.irreg<-data1[irreg.index1+irreg.index2+irreg.index3+irreg.index4+irreg.index5,]

data.irreg=data1[data1$gestation<999,]+data1[data1$age<9,]

data.irreg=subset(data1, gestation!=999)
data.irreg=subset(data.irreg, age!=99)
data.irreg=subset(data.irreg, height!=99)
data.irreg=subset(data.irreg, weight!=999)
data.irreg=subset(data.irreg, smoke!=9)

data.smoker=subset(data.irreg,smoke==1)
data.nonsmoker=subset(data.irreg,smoke==0)

original.smoker=subset(data1,smoke==1)
original.nonsmoker=subset(data1,smoke==0)
set.seed(5)
normal_kurtosis=NULL
for(i in 1:1000)
  normal_kurtosis[i]=kurtosis(rnorm(459))
mean(normal_kurtosis)
summary(data.nonsmoker)
summary(data.smoker)
set.seed(5)
normal_kurtosis=NULL
for(i in 1:1000)
  normal_kurtosis[i]=kurtosis(rnorm(715))
mean(normal_kurtosis)
smoker.uw=data.smoker[data.smoker$bwt<88,]
length(smoker.uw$bwt)/length(data.smoker$bwt)

nonsmoker.uw=data.nonsmoker[data.nonsmoker$bwt<88,]
length(nonsmoker.uw$bwt)/length(data.nonsmoker$bwt)
