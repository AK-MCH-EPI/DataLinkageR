#### Meta Information ##############################################
#
# Project: Basic data linkage in R, a worked example
# Author: Jared W. Parrish Phd
# Data Source: All data is fake
# Purpose: demonstrate using R for data linkages 
# Date Created: 8/1/2018
# Restrictions: Use at own risk

## Load libraries ####

library(RecordLinkage)
library(lubridate)

## Basic String Comps ####
s1<-c("CHILD")
s2<-c("CHLID")
s3<-c("CILHD")
s4<-c("JOHN-ADAMS")
s5<-c("ADAMS-JOHN")

# Simple comparisons
jarowinkler(s1,s2);jarowinkler(s1,s3);jarowinkler(s4,s5)
levenshteinSim(s1,s2);levenshteinSim(s1,s3);levenshteinSim(s4,s5)

library(stringdist)
qgrams(s4,s5)
stringdist(s1,s2,method='qgram',q=2)
stringdist(s1,s3,method='jw')
stringdist(s1,s3,method='osa')
stringdist(s1,s3,method='lv')
stringdist(s1,s3,method='dl')
stringdist(s1,s3,method='jaccard')
stringdist(s1,s3,method='cosine')
stringdist(s1,s3,method='soundex')

## Worked example #####

#Read in data sets to be linked for this example
git1<-'https://raw.githubusercontent.com/AK-MCH-EPI/DataLinkageR/master/DataSetA.csv'
dat1<-read.csv(git1)

git2<-'https://raw.githubusercontent.com/AK-MCH-EPI/DataLinkageR/master/DataSetB.csv'
dat2<-read.csv(git2)

#remove url sets
rm('git1','git2')

#Data cleaning and organization
#Inspect data
names(dat1)
names(dat2)

names(dat2)<-names(dat1) #variable names MUST be identical, modify dat2 to dat1
names(dat2) #Verify names
#Can change single column name with
#names(dat2)[names(dat2)=='Lname']<-'child_last_child'

#check variable components
str(dat1)
str(dat1)

#lets change the dob to actual dates
dat1$dob<-as.Date(as.character(dat1$dob),format("%Y%m%d"))
dat2$dob<-as.Date(as.character(dat2$dob),format("%Y%m%d"))
#lets create a year variable: this would be nice if you wanted to "block" on it.
dat1$year<-year(dat1$dob)
dat2$year<-year(dat2$dob)

head(dat1,5)
head(dat2,5)



#Load in dataset with the actual truth of the linkages specified 
git3<-'https://raw.githubusercontent.com/AK-MCH-EPI/DataLinkageR/master/MatchTruthFull.csv'
Truth<-read.csv(git3)
rm(git3)
Truth$modification<-factor(Truth$modification,levels = c(0,1,2), labels = c("Exact","Modified","Twin") )
addmargins(table(Truth$modification,useNA = "always"))
#Goal is to correctly match 100 unique individuals and not link the twins!

#Need to recode gender coding in dat2 to match dat1 (or vice versa)
dat2$sex<-substr(dat2$sex,1,1)  

## Deterministic Linkage with no cleaning: ####
#Merge on multiple inputs)
Deter.1<-merge(dat1,dat2,by=c("dob","lname","fname","sex"))
length(rownames(Deter.1))

## Deterministic Linkage with cleaned datasets: ####
#Remove specific special characters and make sure linkage strings are UPCASE: 
# Create two new datasets to work with
dat1c<-dat1
dat2c<-dat2

#clean each data set
dat1c$lname<-toupper(gsub("[[:punct:][:space:]]","",dat1c$lname))
dat1c$fname<-toupper(gsub("[[:punct:][:space:]]", "",dat1c$fname))
dat1c$sex<-toupper(dat1c$sex)

dat2c$lname<-toupper(gsub("[[:punct:][:space:]]", "",dat2c$lname))
dat2c$fname<-toupper(gsub("[[:punct:][:space:]]", "",dat2c$fname))
dat2c$sex<-toupper(dat2c$sex)

#Verify  
head(dat1c,5)
head(dat2c,5)  

#Merge data on multiple elements
Deter.1c<-merge(dat1c,dat2c,by=c("dob","lname","fname","sex")) 
nrow(Deter.1);nrow(Deter.1c)

#combine with truth NO Cleaning
m.1<-merge(Truth,Deter.1[,c(5,9,12)],by.x=c("ID","ID.1"),by.y=c("ID.x","ID.y"),all=T)
m.1$inPairs<-with(m.1,ifelse(is.na(year.y),0,1))
addmargins(table(m.1$inPairs,m.1$modification,useNA = "always"))

#combine with truth Cleaned
m.1c<-merge(Truth,Deter.1c[,c(5,9,12)],by.x=c("ID","ID.1"),by.y=c("ID.x","ID.y"),all=T)
m.1c$inPairs<-with(m.1c,ifelse(is.na(year.y),0,1))
addmargins(table(m.1c$inPairs,m.1c$modification,useNA = "always"))

## Probabilistic aka Fuzzy matching ####

#for the compare.linkage, the dates needs to be in an unambiguous format
dat1$dob<-as.character(dat1$dob)
dat2$dob<-as.character(dat2$dob)

# Create a function for assessment of rpairs by thresholds       

"autopairs"<-function(wts=NULL,upper,lower)
{
  ec1<-(epiClassify(wts,threshold.upper = upper,threshold.lower = lower))
  #subset to auto accepted pairs
  pairs.1<-getPairs(ec1,min.weight=upper,max.weight=1,single.rows=T)
  #Merge with Truth to evaluate
  m.c.1<-merge(Truth,pairs.1[,c(2,11,19)],by.x=c("ID","ID.1"),by.y=c("ID.1","ID.2"),all=T)
  m.c.1$inPairs<-with(m.c.1,ifelse(is.na(Weight),0,1))
  x1<-addmargins(table(m.c.1$inPairs,m.c.1$modification,useNA = "always"))
  se<-(x1[2,1]+x1[2,2])/100
  sp<-x1[1,3]/x1[1,5]
  PPV<-(x1[2,1]+x1[2,2])/x1[2,5]
  Acc<-(x1[2,1]+x1[2,2]+x1[1,3])/x1[4,5]
  list(Table = x1, Sensitivity = se, Specificity = sp, PositivePredictiveValue = PPV, Accuracy = Acc)
}

#no cleaning with Jarowinkler

l1a<-compare.linkage(dat1,dat2,strcmp=TRUE,strcmpfun = jarowinkler,exclude = c(1,5,7,8))
summary(l1a)

#Assign weights 
e1a<-epiWeights(l1a) 
getParetoThreshold(e1a) # use function to identify reasonable starting threshold for acceptance

#Thresh at 0.95 for auto acceptance  
ec1<-(epiClassify(e1a,threshold.upper = 0.95,threshold.lower = 0.75))
summary(ec1)
#Visualize these data
barplot(ec1$frequencies,names.arg=c("DOB","Lname","Fname","Sex"),col="slategray3")
hist(ec1$Wdata,col="slategray",prob=T);lines(density(ec1$Wdata,adjust=3),col="sienna2",lwd=2)

# Impact of using different thresholds
autopairs(e1a,0.99,0.75)
autopairs(e1a,0.97,0.75)
autopairs(e1a,0.95,0.75)
autopairs(e1a,0.93,0.75)
autopairs(e1a,0.91,0.75)
autopairs(e1a,0.89,0.75)


# Thresh at 0.90 for auto accept for inspection of the missing 2 cases... 
ec2<-(epiClassify(e1a,threshold.upper = 0.90,threshold.lower = 0.75))
summary(ec2)
pairs.2<-getPairs(ec2,min.weight=0.90,max.weight=1,single.rows=T)
names(pairs.2)

m.c.2<-merge(Truth,pairs.2[,c(2,11,19)],by.x=c("ID","ID.1"),by.y=c("ID.1","ID.2"),all=T)
m.c.2$inPairs<-with(m.c.2,ifelse(is.na(Weight),0,1))
addmargins(table(m.c.2$inPairs,m.c.2$modification,useNA = "always"))    

#Who are we missing?
View(subset(m.c.2,modification=="Modified" & inPairs==0))

# Determine if iterative linkage or expanded thresholds should be used.


#Cleaned data with Jarowinkler
dat1c$dob<-as.character(dat1c$dob)
dat2c$dob<-as.character(dat2c$dob)
l2a<-compare.linkage(dat1c,dat2c,strcmp=TRUE,strcmpfun = jarowinkler,exclude = c(1,5,7,8))
summary(l1a)

#Assign weights based on the strcomp of elements
e2a<-epiWeights(l2a) 
getParetoThreshold(e2a) # use function to identify reasonable starting threshold for acceptance   

# Impact of using different thresholds
autopairs(e2a,0.99,0.75)
autopairs(e2a,0.97,0.75)
autopairs(e2a,0.95,0.75)
autopairs(e2a,0.93,0.75)
autopairs(e2a,0.91,0.75)

#At 0.90 we capture all the cases.

#Can we do better:
# How to avoid matching the twins
# How to avoid matching the false positives
# Manual Review and Machine Learning...but we need manual review to inform the Machine...
# Only showing one example of manual review can do stepped iterative reviews for large sets.

#manual review within R  
#recall 
ec1.c<-(epiClassify(e2a,threshold.upper = 0.90,threshold.lower = 0.79))
summary(ec1.c)

#Need to update for manual review to avoid matching the twins and reduce manual reviews.
ec1.cm<-(epiClassify(e2a,threshold.upper = 1.0,threshold.lower = 0.85))
summary(ec1.cm)

#Identify possible and linked pairs and set single.rows=F (easier to review)
pairs.1cm<-getPairs(ec1.cm,min.weight=0.85,max.weight=1,single.rows=F)

#create indicator variable for designating match
pairs.1cm$is_match<-with(pairs.1cm,ifelse(Weight=="1.0000000",1,''))
View(pairs.1cm)
#Conduct Manual Review and update indicator to 1 if true for all weights between 0.85-0.99
##IMPORTANT: only make edits if you want to do the manual review process. 
##           you can skip this and load the reviewed dataset to continue the example
pairs.1cm<-edit(pairs.1cm)

#Load reviewed dataset
git4<-'https://raw.githubusercontent.com/AK-MCH-EPI/DataLinkageR/master/pairs.1cm.csv'
pairs.1cm<-read.csv(git4)

#Work with R a bit to organize these data.
#remove blank lines and NA rows
pairs.1cm <- pairs.1cm[!(is.na(pairs.1cm$id)),]
#Split data into two frames
pairs.1cm.a<-subset(pairs.1cm,is.na(Weight))
pairs.1cm.b<-subset(pairs.1cm,!is.na(Weight))
pairs<-cbind(dat1=pairs.1cm.b,dat2=pairs.1cm.a)

table(pairs$dat1.is_match,useNA = "always")

#can create a dataset for export and review if needed
Additional_Review<-subset(pairs,dat1.is_match=="9")
View(Additional_Review)

# If any of the "additonal Reivew cases are matches update the pairs dataset
pairs$dat1.id<-trimws(pairs$dat1.id) #need to trim values to merge (white space)
pairs$dat1.Source_ID<-trimws(pairs$dat1.Source_ID)
pairs$dat2.Source_ID<-trimws(pairs$dat2.Source_ID)

pairs$dat1.is_match<-with(pairs,ifelse(dat1.id %in% c(85,89),1,pairs$dat1.is_match))

#merge with truth to verify correct linkages

m_final<-merge(Truth,pairs[,c(2,13,11)],
               by.x=c("ID.1","ID"),
               by.y=c("dat1.Source_ID","dat2.Source_ID"),all=T)

addmargins(with(m_final,table(dat1.is_match,modification,useNA = "always")))

# We have other options for specifying our review levels.   
#use pareto distribution to specify thresholds given a specified interval
thresh<-getParetoThreshold(e2a,interval=c(0.89,1));thresh 
t1<-(epiClassify(e2a,thresh))
summary(t1)

t1a<-getPairs(t1,min.weight=0.80,max.weight=0.99)
View(t1a)


## Supervised classification ---------------------------------------------

#Create training dataset

#subset true 

#  library(dplyr)
#  set.seed(1)
#  t_sub<- Truth %>%
#        group_by (modification) %>%
#        sample_n(5,replace=T)

# sub.a<-t_sub[,c(1:7)]
# sub.b<-t_sub[,c(9:15)]
#  names(sub.b)<-names(sub.a)

#Run linkage
#p<-compare.linkage(sub.a,sub.b,strcmp=TRUE,strcmpfun = jarowinkler,exclude = c(1,5,7))
#  p.1<-editMatch(p)

### For this part if you want to explore the machine learning parts please download the training 
### list object titled "p.1.RData" @ https://github.com/AK-MCH-EPI/DataLinkageR/blob/master/p.1.RData
### and save it to your working directory path. Once you have it saved, load the list object to continue

p.1<-readRDS("D:\\your path here\\p.1.RData")

#Single-hidden-layer neural network  
classif.nn<-trainSupv(p.1,method="nnet");summary(classif.nn)
result.nn<-classifySupv(model=classif.nn,newdata=l2a); summary(result.nn)
pairs.nn<-getPairs(result.nn,show="links",single.rows=T)    
#Merge with Truth to evaluate
m_nn<-merge(Truth,pairs.nn[,c(1,2,11,19)],by.x=c("ID","ID.1"),by.y=c("ID.1","ID.2"),all=T)
m_nn$inPairs<-with(m_nn,ifelse(is.na(m_nn$id1),0,1))
addmargins(table(m_nn$inPairs,m_nn$modification,useNA = "always"))

#Stochastic boosting 
classif.sb<-trainSupv(p.1,method="ada");summary(classif.sb)
result.sb<-classifySupv(model=classif.sb,newdata=l2a); summary(result.sb)
pairs.sb<-getPairs(result.sb,show="links",single.rows=T)    
#Merge with Truth to evaluate
m_sb<-merge(Truth,pairs.sb[,c(1,2,11,19)],by.x=c("ID","ID.1"),by.y=c("ID.1","ID.2"),all=T)
m_sb$inPairs<-with(m_sb,ifelse(is.na(m_sb$id1),0,1))
addmargins(table(m_sb$inPairs,m_sb$modification,useNA = "always"))

#Bootstrap based classification trees 
classif.bs<-trainSupv(p.1,method="bumping");summary(classif.bs)
result.bs<-classifySupv(model=classif.bs,newdata=l2a); summary(result.bs)
pairs.bs<-getPairs(result.bs,show="links",single.rows=T)    
#Merge with Truth to evaluate
m_bs<-merge(Truth,pairs.bs[,c(1,2,11,19)],by.x=c("ID","ID.1"),by.y=c("ID.1","ID.2"),all=T)
m_bs$inPairs<-with(m_bs,ifelse(is.na(m_bs$id1),0,1))
addmargins(table(m_bs$inPairs,m_bs$modification,useNA = "always"))


