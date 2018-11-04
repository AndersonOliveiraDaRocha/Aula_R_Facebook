setwd('C:\\Users\\Anderson\\Documents\\UDACITY\\Nanodegree Data Scientist\\Aulas de R\\Facebook\\Aula_R_Facebook\\')
pf <- read.csv('pseudo_facebook.tsv',sep = '\t')

library(dplyr)
library(ggplot2)

#ggplot(aes(x=age,y=friend_count),data=pf)+
#  geom_point(alpha=1/20)+
#  xlim(13,90)+
#  coord_trans(y = 'sqrt')
  
summary(pf$age)

#ggplot(aes(x=age,y=friendships_initiated),data=pf)+
#  geom_point(alpha=1/20,position = position_jitter(h = 0))+
#  xlim(13,113)+
#  coord_trans(y='sqrt')

age_groups <- group_by(pf,age)
pf_group_age <-- summarise(age_groups,
                           friend_count_mean = mean(as.numeric(friend_count)),
                           friend_count_median = median(as.numeric(friend_count)),
                           n = n())
pf_group_age <- arrange(pf_group_age,age)
head(pf_group_age)

cor.test(x = pf$www_likes_received, y = pf$likes_received, method = "pearson")
  
                           