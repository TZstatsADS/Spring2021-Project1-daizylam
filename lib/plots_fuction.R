#fuctions for graphing:

getfreq =function(df, col_name){
  df %>%
    group_by_(col_name) %>%
    summarise(X_counts = n(), 
              Y_counts_trump = sum(trump_voters),
              Y_counts_biden = sum (biden_voters)) %>%
    mutate(   trump_voter_by_X_pcnt = Y_counts_trump/X_counts,
              biden_voter_by_X_pcnt = Y_counts_biden/X_counts,
              X_pcnt = X_counts/sum(X_counts))
}

#y_count is the total trump/biden votes for each oberservation
#x_count is the total count of the same oberservation
#y/x - percentage of voters in the same oberservation
#x_pcnt - the distribution of x

graph=function(df,x_var){
  theme_set(theme_bw())    
  
  p1=ggplot(df, aes(x=get(x_var), y=biden_voter_by_X_pcnt,fill=get(x_var))) + 
    geom_bar(stat="identity", width=0.8) + 
    labs(title="Frequency barchart of Biden voters",subtitle=x_var,x =x_var,fill= x_var) + 
    coord_cartesian(ylim=c(0,1))+
    theme(axis.text.x = element_text(color = "blue", size = 8),
          legend.title = element_text(color = "blue", size = 8),
          legend.text = element_text(color = "blue", size = 6),
          legend.key.size = unit(0.7,"line"))
  
  p2=ggplot(df, aes(x=get(x_var), y=trump_voter_by_X_pcnt,fill=get(x_var))) + 
    geom_bar(stat="identity", width=0.8) + 
    labs(title="Frequency barchart of Trump voters",subtitle=x_var,x =x_var,fill= x_var) + 
    coord_cartesian(ylim=c(0,1))+
    theme(axis.text.x = element_text(color = "red", size = 8),
          legend.title = element_text(color = "red", size = 8),
          legend.text = element_text(color = "red", size = 6),
          legend.key.size = unit(0.7,"line"))
  
  p3=ggplot(df, aes(x=get(x_var), y=X_pcnt,fill=get(x_var))) + 
    geom_bar(stat="identity", width=0.8) + 
    labs(title="Frequency barchart of all obersevation",subtitle=x_var, x =x_var,fill= x_var) + 
    coord_cartesian(ylim=c(0,1))+
    theme( axis.text.x = element_text(color = "black", size = 8),
           legend.title = element_text(color = "black", size = 8),
           legend.text = element_text(color = "black", size = 6),
           legend.key.size = unit(0.7,"line"))
  
  require(gridExtra)
  grid.arrange(p1, p2, p3, nrow=2,heights=c(5,5))
}