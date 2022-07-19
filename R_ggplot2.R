
#loading the data : 

df<-read.csv("./train.csv")

#libraries to load :

library(ggplot2)
library(dplyr)

#quick overview of the data : 
dim(df)
names(df)
summary(df)

View(df)

#summary of "df" tell us that the "Age " feature has NA values .

#show columns data type  : 
sapply(df , class)

#quick transformation : 

df$Survived<-as.factor(df$Survived)
df$Pclass<-as.factor(df$Pclass)
df$Sex<-as.factor(df$Sex)
df$Embarked<-as.factor(df$Embarked)

#question 1 : what s the distribution of survival rates in general ?

ggplot(df , aes(x=Survived))+theme_bw()+
  geom_bar()+labs(y="number of passengers",title = "survival rates")

# in-depth ( showing propotions of survival) : 
prop.table(table(df$Survived))


#question 2 : how much survived per sex ?
ggplot(df , aes(x=Sex , fill=Survived))+theme_bw()+
  geom_bar()+labs(y="number of passengers",title = "survival rates by sex")

#notice that most females survived where most man didn' t make it alive.


#question 3 : show the distribution of survival by sex and pclass : 

ggplot(df , aes(x=Sex , fill=Survived))+
  theme_bw()+
  geom_bar()+
  facet_wrap(~Pclass)+
  labs(y="number of passengers",title = "survival rates by sex and class")


#notice that more than 90% of females in class 1 and 2 made it alive where
#females of class 3 had 50% chance of survival

#another think important is that the number of man is way bigger than female in
#class3




