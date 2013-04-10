This code processes the events, users, groups and group requests csv files from the Loomio app into metrics; 
writes these to and some images to file. You will need to change the filepaths to ones appropriate to your filesystem. 


The following metrics are currently supported at a daily resolution :

	total_user_accounts				
	activated_accounts				
	monthly_active_users			
	weekly_active_users				
	monthly_engage				
	weekly_engage								
	accounts_2_activated_conversion 
	activated_2_mthlyActive_conv_1 	
	activated_2_weeklyActive_conv_1	
	activated_2_dead_1				

	accounts_2_activated_conv_3		
	activated_2_mthlyActive_conv_3 	
	activated_2_weeklyActive_conv_3
	activated_2_limbo_conv_3		
	activated_2_dead_3				

	prob_become_W_active_user   	
	prob_become_M_active_user		
	prob_become_limbo_user			
	prob_dead_user					

	monthly_active_groups			
	weekly_active_groups			
	become_active_groups_flow 		
	become_dead_groups_flow			
	become_limbo_groups_flow			
	prob_become_active_group		
	prob_become_limbo_group			
	prob_become_dead_group			