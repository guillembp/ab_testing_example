##############  SQL  ###############

# ```
# -- Retention: players played again in 7 days or more vs total, per AB group
# select assignment.abtest_group, count(distinct(assignment.playerid))
# from abtest.assignment
# inner join abtest.activity 
# on activity.playerid = assignment.playerid
# where DATE_DIFF(CAST(activity_date AS DATE),CAST(install_date AS DATE),day) >= 7
# and install_date = '2017-05-04'
# group by abtest_group
# -- Output
# Row	abtest_group	f0_	 
# 1	  A	            40234
# 2	  B	            10131
# ```

# ```
# -- Daily average of games played in each group.
# select avg(gameends) 
# from abtest.activity 
# inner join abtest.assignment 
# on assignment.playerid = activity.playerid
# group by assignment.abtest_group 
# 
# -- Output
# Row	f0_	 
# 1	13.182046403617402	 
# 2	13.034513117659555
# ```

# ```
# --StdDev of games played in each group before the assignment date
# select STDDEV(gameends)
# from abtest.activity 
# inner join abtest.assignment 
# on assignment.playerid = activity.playerid
# where activity_date <= assignment_date
# group by assignment.abtest_group
# ```

# ```
# -- Daily mean of games played in each group before the assignment date
# select sum(gameends)/count(gameends)
# from abtest.activity 
# inner join abtest.assignment 
# on assignment.playerid = activity.playerid
# where activity_date <= assignment_date
# group by assignment.abtest_group 
# 
# -- Output
# Row	f0_	 
# 1	13.18277654890629	 
# 2	13.17057432894987
# 
# -- Daily mean of games played in each group after the assignment date
# -- Output
# Row	f0_	 
# 1	13.181616387750717	 
# 2	12.93720051557958
# ```

# ```
# -- Daily purchases in each group before the assignment date
# select sum(purchases)/count(purchases)
# from abtest.activity 
# inner join abtest.assignment 
# on assignment.playerid = activity.playerid
# where activity_date <= assignment_date
# group by assignment.abtest_group 
# 
# -- Output
# Row	f0_	 
# 1	0.03115669568129514	 
# 2	0.030293484275634518
# 
# -- Daily purchases in each group after the assignment date
# -- Output
# Row	f0_	 
# 1	0.03094721695875372	 
# 2	0.032233302778233365
# ```

# ```
# -- Age of players in assignment
# select count(playerid), date_diff(cast(assignment_date as date),cast(install_date as date),day) as age
# from abtest.assignment 
# group by age
# order by age asc
# ```


library(ggplot2)

##############################


library(ggplot2)
library(gridExtra)

player_assignments_by_age <- read.csv('data/player_assignment_by_age.csv')
colnames <- c('Number_of_assignments', 'Age_days')
colnames(x = player_assignments_by_age) <- colnames
player_assignments_by_age_removed0 <- rapply(player_assignments_by_age,function(x) ifelse(x>1000000,NA,x), how = "replace")

assignment_date_by_group <- read.csv('data/assignment_date_by_group.csv')
colnames <- c('Number_of_assignments', 'AB_test_group','Assignment_date')
colnames(x = assignment_date_by_group) <- colnames
for(i in 1:nrow(assignment_date_by_group)){
  if(assignment_date_by_group[i,"AB_test_group"]=='B'){
    assignment_date_by_group[i,"Number_of_assignments"]<-assignment_date_by_group[i,"Number_of_assignments"]*(-1)
  }
}

par(mfrow=c(1,2))
ggplot(data=player_assignments_by_age, aes(x=Age_days, y=Number_of_assignments)) +
  geom_bar(stat="identity", color = 'steelblue')+
  labs(x='Age in days', y='Number of assignments')+
  theme_minimal()

ggplot(data=player_assignments_by_age_removed0, aes(x=Age_days, y=Number_of_assignments)) +
  geom_bar(stat="identity", color = 'steelblue')+
  labs(x='Age in days, without age 0', y='Number of assignments')+
  theme_minimal()

age0 <- sum(player_assignments_by_age[1,'Number_of_assignments'])
age0_30 <- sum(player_assignments_by_age[1:30,'Number_of_assignments'])
age_total <- sum(player_assignments_by_age[1:length(player_assignments_by_age$Number_of_assignments),'Number_of_assignments'])
age0/age_total

################################

library(gridExtra)

assignment_date_by_group <- read.csv('data/assignment_date_by_group.csv')
colnames <- c('Number_of_assignments', 'AB_test_group','Assignment_date')
colnames(x = assignment_date_by_group) <- colnames
for(i in 1:nrow(assignment_date_by_group)){
  if(assignment_date_by_group[i,"AB_test_group"]=='B'){
    assignment_date_by_group[i,"Number_of_assignments"]<-assignment_date_by_group[i,"Number_of_assignments"]*(-1)
  }
}

plot1 <- ggplot(data=assignment_date_by_group, aes(x=Assignment_date, y=Number_of_assignments, fill = AB_test_group)) +
  geom_bar(stat="identity", position = "identity")+
  theme_minimal()+
  labs(y='Number of assignments', fill='A/B test group')+
  theme(legend.position = c(0.6, 0.7), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.text.y = element_text(angle = 56, hjust = 1))

plot2 <- ggplot(data=assignment_date_by_group, aes(x=Assignment_date, y=Number_of_assignments, fill = AB_test_group)) +
  geom_bar(stat="identity", position = "identity")+
  theme_minimal()+
  labs(x='Assignment date', y='Number of assignments')+
  theme(legend.position = 'none', axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(-25000, 100000))

grid.arrange(plot1, plot2, nrow=2)



