############# Math 189/289C
############# Homework #2
############# Date: Feb. 7th, 2017
############# Authors: Emma Roth, Ileena Mitra, Megan Lee, Jing Gu, Keven Nguyen, Adithya Bharadwaj Balaji


##Case 6:
library(reshape2)
library(ggplot2)
dat <- read.table("/Users/ileenamitra/Google Drive/UCSD/Winter2017/ExpDataAnalysis/Projects/hw2/videodata.txt",header=T)
dat[dat == 99] <- NA
head(dat)
N=nrow(dat)
target = data.frame(Var1 = c(1, 2, 3, 4), grade = c("D", "C", "B", "A"), exp_freq = c(.1, .4, .3, .2))
target$exp_count = target$exp_freq*N

obs = as.data.frame(table(dat$grade), responseName = "count", stringsAsFactors = F)
obs = rbind(obs, c("1", 0))
obs$count = as.numeric(obs$count)
obs$freq = obs$count/N

df <- merge(target, obs, by="Var1")
chisq.test(df$count, y = NULL, correct = TRUE,
       p = df$exp_freq, rescale.p = FALSE,
       simulate.p.value = T, B = 2000)

df.melt = melt(df[,c("grade", "exp_freq", "freq")])

plot = ggplot(df.melt, aes(x=grade, y=value, fill=variable)) + 
geom_bar(position="dodge", stat="identity") +
ggtitle("Grade Distribution Comparision") + 
xlab("Grade") + ylab("Proportion of Students") +
scale_fill_hue(labels = c("Target Grade Assigned", "Student Expected Grade"))
print(plot)

levels(dat$grade) = c("4"="A", "3"="B", "2"="C", "1"="D", "0"="F")

g1 = range(0, 9)
g2 = range(10, 19)
g3 = range(20, 29)
g4 = range(30, 39)
g5 = range(40, 49)
g6 = range(50, 59)
dat$work_group <- with(dat, ifelse(is.na(work), "NA",
                               ifelse(work %in% g1, "0-9", 
                                      ifelse(work %in% g2, "10-19", 
                                             ifelse(work %in% g3, "20-29", 
                                                    ifelse(work %in% g4, "30-39", 
                                                           ifelse(work %in% g5, "40-49",
                                                                  ifelse(work %in% g6, "50-59", "Other"))))))))

#pro
p = ggplot(dat = dat, aes(x = as.factor(grade))) + # dat & aesthetics
geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) + 
ylab("Proportion of Students (per group)") + 
scale_x_discrete(name ="Grade", labels=c("4"="A", "3"="B", "2"="C", "1"="D", "0"="F"), limits = c("4", "3", "2", "1"))

#educ+sex+age+home+math+work+own+cdrom+grade
p + facet_grid(. ~ educ, labeller = as_labeller(c('0'="No", '1'="Yes"))) + ggtitle("Playing educational games")

p + facet_grid(. ~ sex, labeller = as_labeller(c('0'="Males", '1'="Females"))) + ggtitle ("Sex") # add geom

p + facet_grid(. ~ age) + ggtitle("Age")# add geom

p + facet_grid(. ~ home, labeller = as_labeller(c('0'="No", '1'="Yes"))) + ggtitle("Computer at home")# add geom

p + facet_grid(. ~ math, labeller = as_labeller(c('0'="No", '1'="Yes"))) + ggtitle("Hates Math") # add geom

p + facet_grid(. ~ work_group) + ggtitle("# of hours worked prior to the survey") # add geom

p + facet_grid(. ~ own, labeller = as_labeller(c('0'="No", '1'="Yes"))) + ggtitle("Owns personal computer") # add geom

p + facet_grid(. ~ like, labeller = as_labeller(c("1"="Never played","2"="Very much", "3"="Somewhat",  "4"="Not really", "5"="Not at all"))) + ggtitle("Likes to play video games") # add geom

##feature tree
library(tree)
dat['grade_binary'] <- rep(NA, dim(dat)[1])
for(i in 1:dim(dat)[1]){
g <- dat[i, 'grade']
if(!is.na(g)){
if(g == 4){
  dat[i, 'grade_binary'] = 1
}
else{
  dat[i, 'grade_binary'] = 0
}
}
}
dat.tree <- tree(grade_binary~math+sex+age+educ+work+like, dat=dat)
plot(dat.tree, type="uniform")
text(dat.tree)

##section 1
allData <- read.table('videodata.txt', header=TRUE)


allData[allData == 99] <- NA
sum(is.na(allData))

male <- allData[which(allData$sex=='1'),]
female <- allData[which(allData$sex=='0'),]
hateMath <-allData[which(allData$math=='1'),]
likeMath <-allData[which(allData$math=='0'),]
likeVG <-allData[which(allData$like=='2'),]
somewhatLikeVG <-allData[which(allData$like=='3'),]

play <-subset(allData, time >0)

male.percentage <- mean(allData$sex)
male.percentage
female.percentage <-nrow(female)/nrow(allData)
female.percentage

hateMath.percentage <-nrow(hateMath) /nrow(allData)
likeMath.percentage <-nrow(likeMath)/nrow(allData)
likeVG.percentage <- nrow(likeVG) / nrow(allData)
somewhatLikeVG.percentage <-nrow(somewhatLikeVG) /nrow(allData)

getMode <-function(v) {
uniqv <-unique(v)
uniqv[which.max(tabulate(match(v, uniqv)))]
}

getMode(allData$like)

played.percentage <- nrow(play) / nrow(allData)
played.percentage

females.played <-subset(female, time >0)
femalesPlayed.percentage <- nrow(females.played) / nrow(female)
males.played <-subset(male, time >0)
malesPlayed.percentage <-nrow(males.played) /nrow(male)


hateMathAndPlayed <-subset(hateMath, time >0)
likeMathAndPlayed <- subset(likeMath, time >0)

hateMathPlayed.percentage <-nrow(hateMathAndPlayed) / nrow(hateMath)
likeMathPlayed.percentage <-nrow(likeMathAndPlayed) /nrow(likeMath)

likeVGAndPlayed <-subset(likeVG, time >0)
likeVGPlayed.percentage <-nrow(likeVGAndPlayed) / nrow(likeVG)


somewhatLikeVGAndPlayed <-subset(somewhatLikeVG, time >0)
somewhatLikeVGPlayed.percentage <-nrow(somewhatLikeVGAndPlayed) / nrow(somewhatLikeVG)

fpc <- sqrt(250) / sqrt(341)
seAllStudents <- 2*(sqrt(played.percentage*(1-played.percentage))/sqrt(90)) *fpc
confidenceIAllStudents = played.percentage + c(-1, 1)*seAllStudents

seMale <-2*(sqrt(malesPlayed.percentage*(1-malesPlayed.percentage))/sqrt(90)) *fpc
confidenceIMale = malesPlayed.percentage + c(-1, 1)*seMale

seFemale <-2*(sqrt(femalesPlayed.percentage*(1-femalesPlayed.percentage))/sqrt(90)) *fpc
confidenceIfemale <- femalesPlayed.percentage + c(-1, 1)*seFemale
confidenceIfemale

seHateMath <-2*(sqrt(hateMathPlayed.percentage*(1-hateMathPlayed.percentage))/sqrt(90)) *fpc
seHateMath
confidenceIHateMath<-hateMathPlayed.percentage + c(-1, 1)*seHateMath
confidenceIHateMath

seLikeMath <-2*(sqrt(likeMathPlayed.percentage*(1-likeMathPlayed.percentage))/sqrt(90)) *fpc
confidenceILikeMath<-likeMathPlayed.percentage + c(-1, 1)*seLikeMath
confidenceILikeMath

seLikeVG <-2*(sqrt(likeVGPlayed.percentage*(1-likeVGPlayed.percentage))/sqrt(90)) *fpc
confidenceILikeVG<-likeVGPlayed.percentage + c(-1, 1)*seLikeVG
confidenceILikeVG

seSomewhat <-2*(sqrt(somewhatLikeVGPlayed.percentage*(1-somewhatLikeVGPlayed.percentage))/sqrt(90)) *fpc
confidenceISomewhat<-somewhatLikeVGPlayed.percentage + c(-1, 1)*seSomewhat
confidenceISomewhat

#histogram 
bars <-c(played.percentage, femalesPlayed.percentage, malesPlayed.percentage, likeVGPlayed.percentage, somewhatLikeVGPlayed.percentage, hateMathPlayed.percentage, likeMathPlayed.percentage)
labels1 <-c("All Students", "Female", "Male", "Like VG", "Somewhat Like VG", "Hate Math", "Like Math")
myPlot <-barplot(bars, ylim =c(0,1), main='Proportion of Students in Sample that Played VG in Last Week', xlab='Population', ylab='Proportion that Played',names.arg = labels1)

#error bar graph done in google sheets using same data as above


############# R Code by Megan #############

##Section 2 R code by Megan
#clean data
data <- read.table("videodata.txt", header=TRUE)
head(data)
data[data ==99] <-NA
sum(is.na(data))

#plot of time and frequency 
install.packages("ggplot2", dep=T)
install.packages("reshape", dep=T)
library(ggplot2)
library(reshape)
plot(data.frame(data[1], data[4]))
title(main="Time Versus Frequency Plot")

#compare various variables between males and females
male.ind <- which(data['sex'] == 1)
male.ind
data.male <- data[male.ind,]
female.ind <- setdiff(rownames(data), male.ind)
data.female <- data[female.ind,]
summary(data.male)
summary(data.female)


setwd("/Users/jing0824/Documents/graduate school/2017winter/math289/hw2")
dat<-read.table("videodata.txt", header = T)
head(dat)
View(dat)
table(dat$time)
dim(dat)
dat[dat == 99] <- NA
sum(is.na(dat))

library(ggplot2)
set.seed(145)
m<-qplot(dat$time, geom="histogram") 
m + geom_histogram(aes(fill = ..count..)) + labs(title = "Distribution of number of hours played in the week prior to survey") + xlab("Number of hours") + theme_bw(base_size = 12)
est.avg<-mean(dat$time)
est.avg
#We first created a bootstrap population of this size by repeating every sample for {314}\{91} = 3.45 times.
boot.pop<- rep(dat$time, length.out = 314)
length(boot.pop)
head(boot.pop)
#Then we will chose 91 samples from the bootstrap population and repeat this process for 1000 times.
B <- 1000
boot.mean <- numeric(B)
for (i in 1:B){
boot.sample<-sample(boot.pop, size = 91, replace = FALSE)
boot.mean[i]<-mean(boot.sample)
}
hist(boot.mean,breaks = 20, probability = TRUE, density = 30, col = 3, border = 3, main = "Distribution of 1000 bootstrap sample averages", xlab = "Bootstrap sample averages")
lines(density(boot.mean, adjust = 2), col = 2)
##qqplot for means of boostrap samples
par(pty = 's')
qqnorm(boot.mean)
qqline(boot.mean, xlab = "" )
#Shapiro Test
shapiro.test(boot.mean) #failed the test

#Kurtosis Coefficient to check for normality
library(ggplot2)
require(moments)
kurtosis_b<-kurtosis(boot.mean)
kurtosis_b #2.569 (Deviate from normal distribution?)
skewness_b<-skewness(boot.mean) #0.3933
kurtosis <- NULL
skewness <- NULL
for (i in 1:1000){
kurtosis[i]<-kurtosis(rnorm(1000))
skewness[i]<-skewness(rnorm(1000))
}
m<-qplot(kurtosis, geom="histogram") 
m + geom_histogram(aes(fill = ..count..)) + labs(title = "A simulated distribution of kurtosis for samples of size 1000 from a normal distribution") + xlab("Kurtosis coefficients") + theme_minimal(base_size = 12)

n<-qplot(skewness, geom="histogram") 
n + geom_histogram(aes(fill = ..count..)) + labs(title = "A simulated distribution of skewness for samples of size 1000 from a normal distribution") + xlab("Skewness coefficients") + theme_minimal(base_size = 12)


hist(kurtosis)
CI.interval<-quantile(boot.mean, probs = c(0.025, 0.975))
CI.interval #0.5790934 1.7825000   #0.9-1.6
mean(boot.mean)

bootstrap.sim<-function(inmatrix, n){
boot.pop<- rep(inmatrix, length.out = 314)
B<-1000
boot.mean<-numeric(B)
for (i in 1:B){
boot.sample<-sample(boot.pop, size = n, replace = FALSE)
boot.mean[i]<-mean(boot.sample)
} 
CI.interval<-round(quantile(boot.mean, probs = c(0.025, 0.975)),3)
return(c(CI.interval))


set.seed(123)
attach(dat)
table(sex)
male<-dat$time[sex==1]
mean.m<-mean(male)
mean.m
female<-dat$time[sex==0]
mean.f<-mean(female)
mean.f
m.CI.interval<-bootstrap.sim(male, length(male))
f.CI.interval<-bootstrap.sim(female, length(female))
m.CI.interval #1.585 0.681 2.821 
f.CI.interval #0.727 0.184 1.487  

set.seed(128)
table(dat$like)
like<-dat$time[which(dat$like== 2 | dat$like == 3)]
not_like<-dat$time[which(dat$like == 1 | dat$like == 4| dat$like == 5)]
like.mean<-mean(like)
not_like.mean<-mean(not_like)
like.mean
not_like.mean
m.CI.interval<-bootstrap.sim(like, length(like))
f.CI.interval<-bootstrap.sim(not_like, length(not_like))
m.CI.interval #1.515 0.793 2.384 
f.CI.interval #0.023 0.000 0.071

set.seed(150)
table(dat$math)
like.math<-split(dat, dat$math)[[2]][,1]
lm.mean<-mean(like.math)
lm.mean
length(like.math)
hate.math<-split(dat, dat$math)[[1]][,1]
hm.mean<-mean(hate.math)
hm.mean
length(hate.math)
like.m.boot.mean<-bootstrap.sim(like.math, length(like.math))
hate.m.boot.mean<-bootstrap.sim(hate.math, length(hate.math))
like.m.boot.mean #1.139 0.052 3.259  
hate.m.boot.mean #1.276 0.749 1.952 

set.seed(150)
like.dat<-dat[which(dat$like== 2 | dat$like == 3),]
dim(like.dat)
play.busy<-split(like.dat, like.dat$busy)[[2]][,1]
play.busy
length(play.busy)
no.play.busy<-split(like.dat, like.dat$busy)[[1]][,1]
no.play.busy
length(no.play.busy)
play.busy.b<-bootstrap.sim(play.busy, length(play.busy))
no.play.busy.b<-bootstrap.sim(no.play.busy, length(no.play.busy))
play.busy.b #4.638 1.735 8.353
no.play.busy.b #0.617 0.370 0.890

set.seed(175)
table(dat$educ)
dim(like.dat)
play.educ<-split(like.dat, like.dat$educ)[[2]][,1]
length(play.educ)
play.not.educ<-split(like.dat, like.dat$educ)[[1]][,1]
length(play.not.educ)
play.educ.b<-bootstrap.sim(play.educ, length(play.educ))
play.not.educ.b<-bootstrap.sim(play.not.educ, length(play.not.educ))
play.educ.b     #1.3877143 0.7512857 2.2742857 
play.not.educ.b #2.0195161 0.4516129 4.2754032 


set.seed(200)
table(like.dat$work)
quantile(like.dat$work, na.rm = TRUE) #Median number of hours for working is 5
dim(like.dat)
less.work<-like.dat$time[which(like.dat$work <= 5)]
length(less.work)
more.work<-like.dat$time[which(like.dat$work > 5)]
length(more.work)
more.work
less.work.b<-bootstrap.sim(less.work, length(less.work))
more.work.b<-bootstrap.sim(more.work, length(more.work))
less.work.b     #1.9273176 0.5586765 3.8387500  
more.work.b     #1.390906 0.703125 2.312500  

##section 4

## Read and Import data
df <- read.table('videodata.txt', header = TRUE)
df[df == 99] <- NA

#Reformat Data and Add/ Congregate Features
df['dislike'] <- 0
df[49,'like'] <- 1
for(i in 1:91){
  if(df[i,'like']==2 || df[i,'like']==3){
    df[i, 'dislike'] = 0
  }else{
    df[i, 'dislike'] = 1
  }
}

df['likeGames'] <- 0
for(i in 1:91){
  if(df[i,'dislike']==0){
    df[i, 'likeGames'] = 1
  }else{
    df[i, 'likeGames'] = 0
  }
}

df['a'] <- 0
df['b'] <- 0
df['c'] <- 0

for(i in 1:91){
  if(df[i,'grade']==4){
    df[i, 'a'] = 1
  }
}
for(i in 1:91){
  if(df[i,'grade']==3){
    df[i, 'b'] = 1
  }
}
for(i in 1:91){
  if(df[i,'grade']==2){
    df[i, 'c'] = 1
  }
}

## Factor Categorical Variables
cols <- c("busy","educ","sex","home","math","own","cdrom","email","dislike","likeGames","a","b","c")
df[cols] <- lapply(df[cols], factor)

## Logit Model and Coefficients' Odd Ratios (likesGames)
logit <- glm(likeGames~sex+home+math+own+cdrom+email+a+b, family=binomial (link="logit"), data=df)
#logit <- glm(dislike~factor(math), family=binomial (link="logit"), data=df)
summary(logit)
odds <- exp(logit$coefficients)
odds

## Logit Model and Coefficients' Odd Ratios
logit <- glm(dislike~sex+home+math+own+cdrom+email+a+b, family=binomial (link="logit"), data=df)
#logit <- glm(dislike~factor(math), family=binomial (link="logit"), data=df)
summary(logit)
odds <- exp(logit$coefficients)
odds

## Pseudo R2
logit0 <- update(logit, formula = dislike ~ 1, data=df)
McFaddenR2 <- 1-(logLik(logit)/logLik(logit0))
McFaddenR2


data<-read.table("videodata.txt",header=TRUE)

like.data<-subset(data, like==2 | like==3)
dislike.data<-subset(data, like==1 | like==4 | like==5)

like.data$like<-"Like"
dislike.data$like<-"Dislike"

data.new<-data
data.new$like[which(data.new$like == 99)]<-"Dislike"
data.new$like[which(data.new$like == 1)]<-"Dislike"
data.new$like[which(data.new$like >3)]<-"Dislike"
data.new$like[which(data.new$like == 2)]<-"Like"
data.new$like[which(data.new$like == 3)]<-"Like"

#comparison between male and female students

like.sex<-table(data.new$like, data.new$sex)
colnames(like.sex)<-c("Female","Male")
barplot(like.sex, main = "Video Game Preference vs. Gender",
        ylab= "Count",
        names.arg=c("Female","Male"),
        beside=TRUE,
        col=c("red","darkblue"))
legend("topleft", fill = c("red", "darkblue"), legend = c("Dislike", "Like"),cex=0.5)
round(prop.table(like.sex) * 100, 2)

#comparison those who work for pay and those who don't
data.new$work[which(data.new$work == 99)]<-0
data.new$work[which(data.new$work >0)]<-1
like.work<-table(data.new$like, data.new$work)
colnames(like.work)<-c("Nonworking","Working")
barplot(like.work, main = "Video Game Preference vs. Work status",
        ylab= "Count",
        names.arg=c("Nonworking","Working"),
        beside=TRUE,
        col=c("red","darkblue"))
legend("topleft", fill = c("red", "darkblue"), legend = c("Dislike", "Like"),cex=0.5)
round(prop.table(like.work) * 100, 2)

#comparison between those who own a computer vs those who don't

like.own<-table(data.new$like, data.new$own)
colnames(like.own)<-c("Don't Own","Own")
barplot(like.own, main = "Video Game Preference vs. Owning a computer",
        ylab= "Count",
        names.arg=c("Don't Own","Own"),
        beside=TRUE,
        col=c("red","darkblue"))
legend("topleft", fill = c("red", "darkblue"), legend = c("Dislike", "Like"),cex=0.5)
round(prop.table(like.own) * 100, 2)

#comparison between those who are playing educational games
data.new$educ[which(data.new$educ == 99)]<-0
like.educ<-table(data.new$like, data.new$educ)
colnames(like.educ)<-c("No EDG","EDG")
barplot(like.educ, main = "Video Game Preference vs. Playing EDG",
        ylab= "Count",
        names.arg=c("No EDG","EDG"),
        beside=TRUE,
        col=c("red","darkblue"))
legend("topleft", fill = c("red", "darkblue"), legend = c("Dislike", "Like"),cex=0.5)
round(prop.table(like.educ) * 100, 2)

#comparison between those who like math
data.new$math[which(data.new$math == 99)]<-0
like.math<-table(data.new$like, data.new$math)
colnames(like.math)<-c("Dislike Math","Like Math")
barplot(like.math, main = "Video Game Preference vs. Liking Math",
        ylab= "Count",
        names.arg=c("Dislike Math","Liking Math"),
        beside=TRUE,
        col=c("red","darkblue"))
legend("topleft", fill = c("red", "darkblue"), legend = c("Dislike", "Like"),cex=0.5)
round(prop.table(like.math) * 100, 2)
#Additional stuff
nextdata<-data
nextdata[which(nextdata$time==99),]<-NA
nextdata[which(nextdata$like==99),]<-NA
nextdata[which(nextdata$where==99),]<-NA
nextdata[which(nextdata$freq==99),]<-NA
nextdata[which(nextdata$busy==99),]<-NA
nextdata[which(nextdata$educ==99),]<-NA
nextdata[which(nextdata$sex==99),]<-NA
nextdata[which(nextdata$age==99),]<-NA
nextdata[which(nextdata$home==99),]<-NA
nextdata[which(nextdata$math==99),]<-NA
nextdata[which(nextdata$work==99),]<-NA
nextdata[which(nextdata$own==99),]<-NA
nextdata[which(nextdata$cdrom==99),]<-NA
nextdata[which(nextdata$email==99),]<-NA
nextdata[which(nextdata$grade==99),]<-NA
dat1p = na.omit(nextdata)
image(is.na(nextdata), main = "Visualization for Missing Values", xlab = "Observation", ylab = "Variable", xaxt = "n", yaxt = "n", bty = "n")
axis(1, seq(0, 1, length.out = nrow(nextdata)), 1:nrow(nextdata), col = "white")
axis(2, (0:(length(names(nextdata))-1))/(length(names(nextdata))-1), names(nextdata), col = "white", las = 2)

dat1p = na.omit(nextdata)
image(is.na(nextdata), main = "Visualization for Missing Values", xlab = "Observation", ylab = "Variable", xaxt = "n", yaxt = "n", bty = "n")
axis(1, seq(0, 1, length.out = nrow(nextdata)), 1:nrow(nextdata), col = "white")
axis(2, (0:(length(names(nextdata))-1))/(length(names(nextdata))-1), names(nextdata), col = "white", las = 2)


dat_clean <- data[which(data$like != 99),]

dat_clean[which(dat_clean$like==1),]$like=0
dat_clean[which(dat_clean$like==4),]$like=0
dat_clean[which(dat_clean$like==5),]$like=0
dat_clean[which(dat_clean$like==2),]$like=1
dat_clean[which(dat_clean$like==3),]$like=1
require(tree)

## Create Tree
# overfitting

tree.fit = tree(dat_clean$like ~ (dat_clean$own) + (dat_clean$work) + (dat_clean$sex), data=dat_clean, mindev = 0.001)
plot(tree.fit) # Plot the tree
text(tree.fit, cex=0.75) # Text
title('CART (Leaf Class: Like (TRUE),Dislike(FALSE))')
summary(tree.fit)

