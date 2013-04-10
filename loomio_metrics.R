require(lubridate)
require(ggplot2)
require(reshape2)
`%ni%`<- Negate(`%in%`)																	#useful custom operator:returns TRUE if string is NOT IN string vector.

# variables  to be set 

events_path <- "~/R/Workspaces/www.loomio.org-events.csv"								#file paths for the csvs
users_path <- "~/R/Workspaces/www.loomio.org-users.csv"
groups_path <- "~/R/Workspaces/www.loomio.org-groups.csv"
req_path <- "~/R/Workspaces/www.loomio.org-GroupRequest.csv"
helper_bot <- "4ffce04d92a4d6cb21c1494cdfcd6dc1"
loomio_core <- c("c81e728d9d4c2f636f067f89cc14862c","3","25") 							#group ids for enspiral,loomio community, 19Tory
na_actions <- c("user_added_to_group", "user_mentioned")								#actions not relevant to engagement
loomio_private_contrib_grp <- "96b9bff013acedfb1d140579e2fbeb63"						#loomio private contributors group: people who perform actions in this group are active Loomiones
last_day <- as.Date("2012-10-01") 														#environmental variable: last day which script was run until.
yesterday <-   as.Date("2013-04-05") 	#today() - days(1) 
date_vector <- seq(from=last_day, to=yesterday, by="1 day")

processLoomioData <- function() {
    events <- read.csv(events_path, stringsAsFactors=F, header=T, sep=",")
    users <- read.csv(users_path, stringsAsFactors=F, header=T, sep=",")
    groups <- read.csv(groups_path, stringsAsFactors=F, header=T, sep=",")

 	events$date <- as.Date(events$created_at)											#create a date column on the events table for easier manipulation.
 	users$inception <- as.Date(users$created_at)										
 	groups$inception <- as.Date(groups$created_at)

 	top_level_groups <- subset(groups, is.na(parent_id), select=c(id,inception))		#top level groups are those without a parent id in the parent id column
 	events2 <- events[events$user != helper_bot & events$kind %ni% na_actions,]			#filter out the helper bot and actions not relevant to engagement
 	events3 <- events2[events2$parent_group %ni% loomio_core,]							#filter  out Loomio, Enspiral, and 19Tory if needed
 	loomio_contrib <- unique(events$user[events$group == loomio_private_contrib_grp]) 	#find the user-id's of the users that have performed an action in the private loomio contributors group

 	metrics <- as.data.frame(date_vector)												#set a whole lot of date columns that will be used as parameters later on.
 	colnames(metrics) <- "date"
 	metrics$begin112 <- metrics$date -days(112)
 	metrics$begin84 <- metrics$date -days(84)
 	metrics$begin56 <- metrics$date - days(56)
 	metrics$begin28 <- metrics$date - days(28)
 	metrics$begin14 <- metrics$date -days(14)
 	metrics$begin7 <- metrics$date - days(7)

 	groups_df <- expand.grid(top_level_groups$id, date_vector) 						#make a dataframe for each top level group on each date in date_vector
 	colnames(groups_df) <- c("group", "date")

 	trimGroupsDF <- function (g, d) {												#trim down groups_df; get rid of dates when group did not exist	
 		if (top_level_groups$inception[top_level_groups$id == g] > d) {
 			return(TRUE)
 		} else {
 			return(FALSE)
 		}
 	}

 	groups_df$trim <- mapply(trimGroupsDF, groups_df$group, groups_df$date) 		#trim down groups_df; get rid of dates when group did not exist
 	groups_df <- groups_df[groups_df$trim == FALSE,]
 	groups_df$begin84 <- groups_df$date - days(84)
 	groups_df$begin28 <- groups_df$date - days(28)
 	groups_df$begin7 <- groups_df$date - days(7)

 	activeUsers <- function (s, e, threshold) {										#count number of users that performed at least [threshold] action between to dates ((s)tart and (e)nd)			
	    s <- subset(events3,  date > s & date <= e, select=user)
	    a <- as.data.frame(table(s$user))
	    result <- length(a$Freq[which(a$Freq >= threshold)])
	    return(result) 
	}

 	usersByGroup <- function (s, e, g)  {
 		users_vector <- events2$user[events2$date <= e & events2$date > s & events2$top_group == g] 
 		loomio_contrib_bool_vector <- unlist(sapply(users_vector, function (v) { if (v %in% loomio_contrib) { return(TRUE) } else { return(FALSE) } })) #tag actions by whether a loomio contributor performed them
	   	len <- length(users_vector)
	   	loomio_contrib_count <- sum(loomio_contrib_bool_vector)												#count number of loomio contributor actions
	   	loomio_contrib_ratio <- loomio_contrib_count / len 													#and divide by total number of actions
	   	active_count <- length(unique(users_vector))
	   	return(c(active_count,loomio_contrib_count))

	}

	accountConversion <- function (s, m, e, period) {
		accounts <- users$id[users$inception <= m & users$inception > s]									#extract user accounts created (inception) between (s)tart and (m)id date parameters
		activated <- users$id[users$inception <= m & users$inception > s & users$last_sign_in_at != ""]		#extract user accounts  that have been activated (users$last_sign_in_at != "") created (inception) between (s)tart and (m)id date parameters
		len <- length(activated)
	    monthCount <- 0
	    weekCount <- 0
	    limboCount <- 0
	    deadCount <- 0
	    monthThresh <- 1
	    weekThresh <- 4
	    if (period == 28) {																					#adjust action threshholds depending on the length of the period between (m)id and (e)nd
	    	monthThresh <- 3
	    	weekThresh <- 12 
	    }
	    for (i in 1:len) {																					#check if each ([i]) activated users perform equal or greater than [threshhold] actions between (m)id and (e)nd dates
	        v <- events2$user[events2$date > m & events2$date <= e & events2$user == activated[i]]
	        actions <- length(v)
	        if (actions >= monthThresh) {monthCount <- monthCount + 1}
	        if (actions >= weekThresh) {weekCount <- weekCount + 1}
	        if (actions > 0 & actions < monthThresh) {limboCount <- weekCount +1}
	        if (actions == 0) {deadCount <- deadCount + 1}
	    }		
		activatedRatio <- len / length(accounts)
		monthRatio <- monthCount / len
		weekRatio <- weekCount / len
		limboRatio <- limboCount / len
		deadRatio <- deadCount / len
		return(c(activatedRatio, monthRatio, weekRatio, limboRatio, deadRatio))
	}


	deadGroups <- function(s, m, e) {
	    gs <- top_level_groups$id[top_level_groups$inception <= m & top_level_groups$inception > s]
	    len <- length(gs)
	    deadCount <- 0
	    activeCount <- 0
	    for (i in 1:len) {
	        v <- events2$user[events2$date <= e & events2$date > m & events2$top_group == gs[i]] 	
	        a <- as.data.frame(table(v))
	        active <-  length(a$Freq[a$Freq >= 3])
	        if 	(length(v) == 0) {
	        	deadCount <- deadCount + 1
	        } else {
	        	if (active > 2) { 
	        		activeCount <- activeCount + 1 
				}
	       	}
		limboCount <- len - sum(deadCount, activeCount)
		}
		activeRatio <- activeCount / len
		deadRatio <- deadCount / len
		limboRatio <- limboCount / len

		return(c(activeCount, deadCount, limboCount, activeRatio, deadRatio, limboRatio))
	}

	monthly_by_group					<- mapply(usersByGroup, groups_df$begin28, groups_df$date, groups_df$group)
	weekly_by_group						<- mapply(usersByGroup, groups_df$begin7, groups_df$date, groups_df$group)	
	groups_df$monthly_active_users 		<- monthly_by_group[1,]
	groups_df$weekly_active_users 		<- weekly_by_group[1,]
	groups_df$loomio_contrib_monthly	<- monthly_by_group[2,]
	groups_df$weekly_active_users 		<- weekly_by_group[2,]	

	
	
	prob_user_cat <- mapply(probabilityUserCategory, metrics$begin112, metrics$begin84, metrics$date)
	deadLimbo <- mapply(deadGroups, metrics$begin112, metrics$begin84, metrics$date)
	userConversion1mth <- mapply(accountConversion, metrics$begin56, metrics$begin28, metrics$date, 28) #1 month lag
	userConversion3mth <- mapply(accountConversion, metrics$begin112, metrics$begin84, metrics$date, 84) #3 month lag

	metrics$total_user_accounts				<- sapply(metrics$date, function(d) { length(users$id[users$inception <= d]) })
	metrics$activated_accounts				<- sapply(metrics$date, function(d) { length(users$id[users$inception <= d & users$last_sign_in_at != ""]) } )
	metrics$monthly_active_users			<- mapply(activeUsers, metrics$begin28, metrics$date, 1)
	metrics$weekly_active_users				<- mapply(activeUsers, metrics$begin7, metrics$date, 1)
	metrics$monthly_engage					<- metrics$monthly_active_users / metrics$activated_accounts
	metrics$weekly_engage					<- metrics$weekly_active_users / metrics$activated_accounts
	metrics$accounts_2_activated_conv_1		<- userConversion1mth[1,]
	metrics$activated_2_mthlyActive_conv_1 	<- userConversion1mth[2,]
	metrics$activated_2_weeklyActive_conv_1	<- userConversion1mth[3,]
	metrics$activated_2_dead_1				<- userConversion1mth[5,]

	metrics$accounts_2_activated_conv_3		<- userConversion3mth[1,]
	metrics$activated_2_mthlyActive_conv_3 	<- userConversion3mth[2,]
	metrics$activated_2_weeklyActive_conv_3	<- userConversion3mth[3,]
	metrics$activated_2_limbo_conv_3		<- userConversion3mth[4,]
	metrics$activated_2_dead_3				<- userConversion3mth[5,]

	metrics$prob_become_W_active_user   	<- prob_user_cat[1,]	#three month lag
	metrics$prob_become_M_active_user		<- prob_user_cat[2,]
	metrics$prob_become_limbo_user			<- prob_user_cat[3,]	#three month lag
	metrics$prob_dead_user					<- prob_user_cat[4,]	#three month lag

	metrics$monthly_active_groups			<- sapply(metrics$date, function(d) { length(groups_df$group[groups_df$date == d & groups_df$monthly_active_users > 2]) })
	metrics$weekly_active_groups			<- sapply(metrics$date, function(d) { length(groups_df$group[groups_df$date == d & groups_df$weekly_active_users > 2]) })
	metrics$become_active_groups_flow 		<- deadLimbo[1,]
	metrics$become_dead_groups_flow			<- deadLimbo[2,]
	metrics$become_limbo_groups_flow		<- deadLimbo[3,]	
	metrics$prob_become_active_group		<- deadLimbo[4,]
	metrics$prob_become_limbo_group			<- deadLimbo[5,]
	metrics$prob_become_dead_group			<- deadLimbo[6,] 
	
	# generate images
	metrics_sub <- subset(metrics, select=c(date, total_user_accounts:prob_become_dead_group))
	metrics_melt <- melt(metrics_sub, id="date")
	
	raw_user_numbers <- subset(metrics_melt, variable %in% c("total_user_accounts", "activated_accounts", "monthly_active_users", "weekly_active_users"))
	
	image_monthly_active_by_group <- ggplot(groups_df, aes(x=date, y=monthly_active_users, color=group)) + geom_line() +theme(legend.position="none") + labs(x="", y="Monthly active users", title="Monthly active users by group")
	image_raw_user_numbers <-  ggplot(raw_user_numbers, aes(x=date, y=value, color=variable)) + geom_line() + theme(legend.text=element_text(size=15), legend.title=element_blank(), axis.text = element_text(size=15), axis.title=element_blank()) + scale_color_manual(values=c("blue", "purple","red",  "brown"), labels=c("Total accounts created", "Acivated accounts", "Monthly active users", "Weekly active users"))

	ggsave(image_monthly_active_by_group, path="~/Pictures/Loomio/script_outputs", filename=paste("month_act_by_group-", last_day, ".png", sep=""), width=20, height=12.5, units="cm")
	ggsave(image_raw_user_numbers, path="~/Pictures/Loomio/script_outputs", filename=paste("user_numbers-", last_day, ".png", sep=""), width=25, height=12.5, units="cm")

	#write files

	write.csv(groups_df, paste("~/R/Workspaces/group_df_", last_day, ".csv", sep=""))
	write.csv(metrics, paste("~/R/Workspaces/metrics_", last_day, ".csv", sep=""))
	return(metrics)

}

