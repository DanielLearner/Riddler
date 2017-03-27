#From Steve Simon, an adorable living-room puzzle:

#Your baby is learning to walk. The baby begins by holding onto a couch. 
#Whenever she is next to the couch, 
#there is a 25 percent chance that she will take a step forward and a 75 percent chance that she will stay clutching the couch. 
#If the baby is one or more steps away from the couch, 
#there’s a 25 percent chance that she will take a step forward, a 25 percent chance she’ll stay in place and a 50 percent chance she’ll take one step back toward the couch.

#In the long run, what percent of the time does the baby choose to clutch the couch?




trials_to_run <- 10000

distance<-0
trial_history <- setNames(data.frame(matrix(ncol = 4, nrow = trials_to_run)), c("Trial", "Steps_Away", "At_Couch", "Percentage_Of_Time_At_Couch"))

for (i in 1:trials_to_run){ 
	#Baby Chooses What to Do Next
	if (distance==0){
		distance<-sample(c(0,1), size = 1, prob = c(0.75, 0.25))
	}else{
		distance<-sample(c(distance-1, distance, distance+1), size = 1, prob = c(0.5, 0.25, 0.25))
	}

	#Record State
	trial_history$Trial[i]<-i
	trial_history$Steps_Away[i]<-distance
	if (distance==0){
		trial_history$At_Couch[i]<-1
	}else{
		trial_history$At_Couch[i]<-0
	}

	trial_history$Percentage_Of_Time_At_Couch[i] <- sum(na.omit(trial_history$At_Couch))/i
}

#Print Solution
print (trial_history$Percentage_Of_Time_At_Couch[trials_to_run])

#Plot Trial History
library(ggplot2)
ggplot(trial_history, aes(x=Trial, y=Percentage_Of_Time_At_Couch)) + geom_line()