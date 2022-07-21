
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

#question 1 : what s the distribution of survival rates in general (by %) ?

ggplot(df , aes(x=Survived, y=after_stat(100*count/sum(count))))+theme_bw()+
  geom_bar()+labs(y="number of passengers by %",title = "survival rates ")+
  transition_states(states = Pclass)

# in-depth ( showing propotions of survival) : 
prop.table(table(df$Survived))

#notice thta after_stat is used to get the computed count (statistics layer)


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


#question 4 : doing a 4 dimensionnal visualisation ( survival rates per (age+sex+class))
ggplot(df , aes(x=Age , fill=Survived))+
  theme_bw()+
  geom_density(alpha=0.5)+
  facet_wrap(Sex~Pclass)+
  labs(y="number of passengers",title = "survival rates by Age")



#question 5 : show the distribution of Age by pclass with mean and median on it :
ggplot(df)+
  theme_bw()+
  geom_jitter(aes(x=Pclass, y=Age))+
  stat_summary(aes(x=Pclass,y=Age),fun = mean ,geom = "point", colour="red")+
  stat_summary(aes(x=Pclass,y=Age),fun = median ,geom = "point", colour="yellow")+
  labs(x="class type",title = "age distribution by class")

#zoom in :

ggplot(df)+
  theme_bw()+
  geom_jitter(aes(x=Pclass, y=Age))+
  stat_summary(aes(x=Pclass,y=Age),fun = mean ,geom = "point", colour="red")+
  stat_summary(aes(x=Pclass,y=Age),fun = median ,geom = "point", colour="yellow")+
  coord_cartesian(ylim = c(0,10))+
  labs(x="class type",title = "age distribution by class")

#multiple plots in a single grid : 

  first_graph<-ggplot(df)+
  theme_bw()+
  geom_jitter(aes(x=Pclass, y=Age))+
  stat_summary(aes(x=Pclass,y=Age),fun = mean ,geom = "point", colour="red")+
  stat_summary(aes(x=Pclass,y=Age),fun = median ,geom = "point", colour="yellow")+
  labs(x="class type",title = "age distribution by class")
  
  
  second_graph<-ggplot(df , aes(x=Sex , fill=Survived))+theme_bw()+
    geom_bar()+labs(y="number of passengers",title = "survival rates by sex")
  
  third_graph<-ggplot(df)+
    geom_boxplot(aes(x=Sex ,y=Age ,fill=Sex))+
    scale_fill_manual(values = c("#999999", "#E69F00"))+
    theme_bw()+
    labs(x="sex categories")
  
  # library to load : 
  library(patchwork)
  
  first_graph + second_graph + third_graph # this will return the graphs on same line
  first_graph / second_graph #this will return graphs top of each other
  (first_graph | second_graph) / third_graph #this is a more suitable presentation
  
  
  #we can move the place of legends if it s in the way , just
  #by adding : 
  
  res<-(third_graph| second_graph) / first_graph
  
  res + plot_layout(
    guides = "collect"
  )
  
  #to edit the space took by each graph we can add : 
  
  res+ plot_layout (
    widths = c(1,2,1)     #this line means devide the space into 4 , the middle one takes 2 ( so double the others)
  )
  
#another variant would be : units(c(5,1,1),c('cm','null','null'))
# in this command above the first graph takes 5 cm the others just fill the remaining

  
######################################################################
  
#part 2 : animation in ggplot 2
  
  library(gifski)
  library(gganimate)
  
#animation of survival rate per class : 
  ggplot(df , aes(x=Survived, y=after_stat(100*count/sum(count))))+theme_bw()+
    geom_bar()+labs(y="number of passengers by %",title = "survival rates in {closest_state}")+
    transition_states(states = Pclass)+
    enter_grow()+
    exit_fade()
  
  
  
  
  
  
  



  
