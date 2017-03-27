
#From Christopher Dierkes, a lazy day puzzle:

#You and I find ourselves indoors one rainy afternoon, with nothing but some loose change in the couch cushions to entertain us. 
#We decide that we’ll take turns flipping a coin, and that the winner will be whoever flips 10 heads first. The winner gets to keep 
#all the change in the couch! Predictably, an enormous argument erupts: We both want to be the one to go first.

#What is the first flipper’s advantage? In other words, what percentage of the time does the first flipper win this game?


trials_to_run <- 100000
number_of_heads_to_win_trial <- 10
#create data frame to hold trial results
trial_history <- setNames(data.frame(matrix(ncol = 4, nrow = trials_to_run)), c("first_player_won", "second_player_won", "percent_first_player_has_won_so_far", "percent_second_player_has_won_so_far"))
trial_history[is.na(trial_history)] <- 0

#main loop, each itteration is 1 trial 
for (i in 1:trials_to_run){ 

	x <- 0
	y <- 0

#flip coins until at least one player reaches 10 heads
	while (x < number_of_heads_to_win_trial & y < number_of_heads_to_win_trial){
		x <- x+sample(0:1, 1)
		y <- y+sample(0:1, 1)
	}

#gives a point to the winner
#note that if both players reached 10, then the player who went first would have reached ten first.  So, the first player gets the point
	if (x>=y){
		trial_history$first_player_won[i] <- 1
	}else{
		trial_history$second_player_won[i] <- 1
	}

	trial_history$percent_first_player_has_won_so_far[i] <- sum(trial_history$first_player_won)/i
	trial_history$percent_second_player_has_won_so_far[i] <- 1-trial_history$percent_first_player_has_won_so_far[i]
	

}


#finds the percentage of time that the first player won
percent_first_player_won <- paste0(round(100*sum(trial_history$first_player_won)/trials_to_run,2),"%")
print(paste0("After ", trials_to_run, " trials, the player who went first won ",percent_first_player_won , " of the time."))


#plot cumulative_win_percentage of each player after each trial
require(ggplot2)
require(reshape)
require(scales)

cumulative_win_percentage <- data.frame(trial = 1:trials_to_run,trial_history[c("percent_first_player_has_won_so_far", "percent_second_player_has_won_so_far")])
ggplot(melt(cumulative_win_percentage2 ,  id.vars = 'trial' , variable.name = "player", value.name = "win percentage"), aes(trial, value)) + geom_line( aes(colour = variable)) + scale_y_continuous(labels = scales::percent)+geom_text(aes(label=ifelse(trial==trials_to_run & variable== "percent_first_player_has_won_so_far",as.character(percent_first_player_won),'')),hjust=1,vjust=-1)
