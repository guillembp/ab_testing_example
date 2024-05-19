library(ggplot2)

install_frequency <- read.csv('data/install_frequency.csv')
start_date <- '2016-01-01'
end_date <- '2017-05-22'
date_list <- seq(as.Date(start_date), as.Date(end_date), "days")
install_frequency_dates <- cbind(install_frequency,date_list)
colnames <- c('Number_of_installs', 'Day')
colnames(x = install_frequency_dates) <- colnames

# sum of every 6 days
sum_installs_mavg_list <- filter(install_frequency_dates$Number_of_installs, rep(1/3,3))
sum_installs_mavg <- as.data.frame(cbind(sum_installs_mavg_list,install_frequency_dates[seq(508,0,by=-6),]))

# sum of every 19 days (Duration of AB test)
sum_installs_19days_list <- colSums(matrix(install_frequency_dates$Number_of_installs, nrow=19))
sum_installs_19days <- as.data.frame(cbind(sum_installs_19days_list,install_frequency_dates[seq(508,0,by=-19),]))


columnames <- c('sum_installs', 'number_of_installs','Day')
colnames(sum_installs_mavg) <- columnames
colnames(sum_installs_19days) <- columnames
sum_installs <- rbind(sum_installs_19days,sum_installs_mavg)
sum_installs$dataset <- c(rep("sum_installs_19days", nrow(sum_installs_19days)), rep("sum_installs_mavg", nrow(sum_installs_mavg)))
sum_installs_cut <- sum_installs[sum_installs$Day>as.Date('2017-2-22'),]
mean_mavg <- mean(sum_installs_cut[sum_installs_cut$dataset == 'sum_installs_mavg',]$sum_installs)
mean_19days <- mean(sum_installs_cut[sum_installs_cut$dataset == 'sum_installs_19days',]$sum_installs)

#Plot
ggplot(data=sum_installs_cut, aes(x=Day, y=sum_installs, color=dataset)) +
  geom_line()+
  geom_point()+
  theme(legend.position = c(0.8, 0.9))+
  labs(x='Time period split in the past 3 months', y='Sum of daily installs in time period', colour = 'Time Period')+
  annotate(geom = 'rect', xmin=as.Date('2017-05-04'), xmax=as.Date('2017-05-22'), ymin=0, ymax=max(sum_installs_cut$sum_installs)*1.2, alpha=0.2, fill="red")+
  theme_minimal()+
  theme(legend.position = c(0.2, 0.9))+
  geom_hline(yintercept = mean_19days, color="maroon")+
  geom_hline(yintercept = mean_mavg, color="green")


##############################


player_assignments_by_age <- read.csv('data/player_assignment_by_age.csv')
colnames <- c('Number_of_assignments', 'Age_days')
colnames(x = player_assignments_by_age) <- colnames
ggplot(data=player_assignments_by_age, aes(x=Age_days, y=Number_of_assignments)) +
  geom_bar(stat="identity")

player_assignments_by_age_removed0 <- player_assignments_by_age[-1,]
ggplot(data=player_assignments_by_age_removed0, aes(x=Age_days, y=Number_of_assignments)) +
  geom_bar(stat="identity")


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
  theme(legend.position = c(0.6, 0.7), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.text.y = element_text(angle = 56, hjust = 1))

plot2 <- ggplot(data=assignment_date_by_group, aes(x=Assignment_date, y=Number_of_assignments, fill = AB_test_group)) +
  geom_bar(stat="identity", position = "identity")+
  theme_minimal()+
  theme(legend.position = 'none', axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(-25000, 100000))

grid.arrange(plot1, plot2, nrow=2)
