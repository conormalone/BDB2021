---
  title: |
  ![](https://cowboyswire.usatoday.com/wp-content/uploads/sites/73/2017/07/usatsi_9756321.jpg){width=4in,height=2in}  
Identifying the Best Players Against the Pass: A Plus/Minus Metric for Defensive Back Effect on Quarterback Time to Throw
author: "Conor Malone"
date: "NFL Big Data Bowl 2021"
runtime: shiny
output:
  html_document:
  number_sections: true
fig_caption: true
toc: true
theme: readable
highlight: tango
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r echo = FALSE, message = FALSE, warnings = FALSE}

library(tidyverse)
####Mike Lopez's Code Below
#turning off warnings
options(warn=-1)

#setting plot width and height
options(repr.plot.width=15, repr.plot.height = 10) 

#includes schedule info for games
df_games <- read_csv("../input/nfl-big-data-bowl-2021/games.csv",
                     col_types = cols())

#includes play-by-play info on specific plays
df_plays <- read_csv("../input/nfl-big-data-bowl-2021/plays.csv",
                     col_types = cols())

#includes background info for players
df_players <- read_csv("../input/nfl-big-data-bowl-2021/players.csv",
                       col_types = cols())

##Reading tracking data (needs to be done iteratively)

#weeks of NFL season
weeks <- seq(1, 17)

#blank dataframe to store tracking data
df_tracking <- data.frame()

#iterating through all weeks
for(w in weeks){
  
  #temperory dataframe used for reading week for given iteration
  df_tracking_temp <- read_csv(paste0("../input/nfl-big-data-bowl-2021/week",w,".csv"),
                               col_types = cols())
  
  #storing temporary dataframe in full season dataframe
  df_tracking <- bind_rows(df_tracking_temp, df_tracking)                            
  
}
#merging plays and tracking data
df_merged <- inner_join(df_games,
                        df_plays,
                        by = c("gameId" = "gameId"))

#merging games data to previously merged frame
df_merged <- inner_join(df_merged,
                        df_tracking,
                        by = c("gameId" = "gameId",
                               "playId" = "playId"))

#######end of MLo's code
#delete unused dfs
rm(df_tracking_temp)
rm(df_tracking)
#get Player's team name as a variable
df_merged$Player_team <-ifelse(df_merged$team == "home", df_merged$homeTeamAbbr, df_merged$visitorTeamAbbr)
#variable describing whether a player's team is in possession at the start of the play, 1 for in possession, 0 otherwise
df_merged$InPoss <-ifelse(df_merged$possessionTeam == df_merged$Player_team, 1, 0)

#set event and position as factor
df_merged$event <- as.factor(df_merged$event)
df_merged$position <- as.factor(df_merged$position)

#sort events into snaps, forwards (passes) or sacks 
snaps <- c("ball_snap")
forwards <- c("lateral", "pass_forward", "pass_shovel")
#a relic from when this was a survival analysis, sacks were right-censoring
sacks <- c("qb_sack", "qb_strip_sack", "safety", "tackle", "out_of_bounds")

#filter df so it only has events contained in snap/forward passes/sacks
df_merged <- df_merged %>% mutate(play_type = case_when(
  event %in% snaps ~ "snap",
  event %in% forwards ~ "passforward",
  event %in% sacks ~ "sacked"))
df_merged <- df_merged %>% filter(!is.na(play_type))
df_smaller <-distinct(df_merged, gameId, playId, frameId, .keep_all = TRUE)%>% 
  dplyr::select(gameId, playId, frameId, play_type )

#widen to get snap and sack/pass as variables with playid gameid
df_smaller <- df_smaller %>% group_by(gameId, playId) %>%
  pivot_wider(id_cols = c(gameId, playId),names_from = play_type, values_from = frameId, values_fn = median)



#delete plays that don't have a snap and a pass forward or sack, eliminating incomplete plays
df_smaller <- df_smaller[rowSums(is.na(df_smaller))<2,]
#set survival_time variable = difference between snap time and pass/sack
df_smaller$survival_time <-ifelse(is.na(df_smaller$passforward), df_smaller$sacked- df_smaller$snap, df_smaller$passforward- df_smaller$snap)



#isolate defensive players by position
df_only_def_players <-df_merged %>% filter(position == "CB" | position == "FS" | position == "S" | position == "SS" |position == "DB")%>% 
  dplyr::select(gameId, playId, nflId) %>% 
  distinct(gameId, playId, nflId, .keep_all = TRUE)%>% 
  group_by(gameId, playId) %>% mutate(id = row_number())

#isolate offensive players based on who had a route on a play
df_only_off_players <-df_merged %>% filter(complete.cases(route))%>% 
  #the data contains some QBs given route descriptions who didn't have them, eliminate these
  filter(position != "QB")%>% 
  dplyr::select(gameId, playId, nflId) %>% 
  distinct(gameId, playId, nflId, .keep_all = TRUE)%>% 
  group_by(gameId, playId) %>% mutate(id = row_number())
#increase offensive ids by 10 so they don't clash with defensive player ids
df_only_off_players$id <- df_only_off_players$id+10

df_only_qbs <-df_merged %>% filter(position == "QB")%>% 
  dplyr::select(gameId, playId, nflId) %>% 
  distinct(gameId, playId, nflId, .keep_all = TRUE)%>% 
  group_by(gameId, playId) %>% mutate(id = row_number())
df_only_qbs$id <- df_only_qbs$id+20


#bind off and def players into df
df_onlyplayers <- rbind(df_only_def_players, df_only_off_players, df_only_qbs)

#isolate all player nflIds to use as colnames for df placing players in stints
thecolnames <- df_onlyplayers %>%
  dplyr::select(nflId) 

thecolnames <-as.list(unique(thecolnames$nflId))

#widen so a play is a row, with all players involved as cols
df_onlyplayers <- df_onlyplayers %>% 
  pivot_wider(names_from = id, values_from = nflId, names_prefix = 'player_')

wide_players <- df_onlyplayers %>% dplyr::select(player_1,player_2,player_3,player_4,player_5,player_6,player_7,player_8,player_11,player_12,player_13,player_14,player_15, player_21)
#set the variable "widey" as a string made up of all player nflids, this will be our stint identifier
wide_players$stint <- apply(wide_players[,3:16] , 1, function(x) toString(na.omit(x)))

dfsmaller <- inner_join(df_smaller, wide_players, by = c("gameId", "playId")) %>%
  dplyr::select(-c(player_1,player_2,player_3,player_4,player_5,player_6,player_7,player_8,player_11,player_12,player_13,player_14,player_15, player_21))
dfsmaller$stint  <-as.factor(dfsmaller$stint)

#get df with colnames of each player, rows for each stint, 0 if a column player is not present, or the number of plays in the stint otherwise 
df <- as.data.frame(matrix(nrow =length(levels(dfsmaller$stint)), ncol = length(thecolnames)),row.names = levels(dfsmaller$stint))
#set 1 column each for the nflids of players remaining in the data
names(df) <- thecolnames
off_ids <-df_only_def_players %>% 
  distinct(nflId) %>% 
  dplyr::select(nflId)
off_id_list <-off_ids$nflId

#get the sum of survival times for each stint and the number of plays
summed_times <-dfsmaller %>% dplyr::select(survival_time, stint) %>%  group_by(stint) %>% 
  summarise(time = sum(survival_time), count = n())
#loop through each player and stint to get players present
for(i in 1:ncol(df)){
  for(j in 1:nrow(df)){
    df[j,i] <- ifelse(length(grep(paste0("\\b",colnames(df[i]),"\\b"),row.names(df)[j], value=F)) == 0, 0,1 )*summed_times$count[j]
    
  }
}
#get the total duration of the stint as a variable in this new df
df$summed <- summed_times$time

#ridge regression
library(MASS) #contains ridge regression function
#perform ridge regression to get coefficient for each player.
#superior to linear regression in this case as reduces coefficients to reduce error.
fit <- lm.ridge(summed ~ ., data = df)
lmfit <- lm(summed ~ ., data = df)

#store coefficients as "results" dataframe
results <- as.data.frame(fit$coef)
col_sums <- as.data.frame(abs(colSums(df)))
col_sums <- rownames_to_column(col_sums, "NflId")
colnames(col_sums) <- c("NflId", "count")
results <- rownames_to_column(results, "NflId")
results$NflId <- gsub("`", '', results$NflId)
results$NflId <- as.numeric(results$NflId)
results <- merge(results, col_sums, by = "NflId" )
results <- merge(results, df_players, by.x = "NflId", by.y = "nflId" )


#####put results in printable format
#top DBs
topresulttable <- results[order(results$`fit$coef`, decreasing = T),]
topresulttable$`fit$coef` <-    round(topresulttable$`fit$coef`/10,2)                          
topresulttable <- topresulttable %>%  filter(position !="QB") %>%
  dplyr::select("fit$coef","displayName","position")
colnames(topresulttable) <-c("Seconds Added","Name", "Position" )

#top Receivers
botresulttable <- results[order(results$`fit$coef`, decreasing = F),]             
botresulttable$`fit$coef` <-    round(botresulttable$`fit$coef`/10,2)                          
botresulttable <- botresulttable %>% dplyr::select("fit$coef","displayName","position") %>% filter(position == "WR")
colnames(botresulttable) <-c("Seconds Added","Name", "Position" ) 

#top QBs                            
qbresulttable <- results[order(results$`fit$coef`, decreasing = T),]             
qbresulttable$`fit$coef` <-    round(qbresulttable$`fit$coef`/10,2)                          
qbresulttable <- qbresulttable %>% dplyr::select("fit$coef","displayName","position") %>% filter(position == "QB")
colnames(qbresulttable) <-c("Seconds Added","Name", "Position" ) 
####end of printable format

#merge the wide df from above with the df containing survival times
results_per_play <-merge(wide_players, dfsmaller, by = c("gameId", "playId"))
#lengthen this so each player gets a row  
results_per_play_long <- results_per_play %>% 
  pivot_longer(cols = starts_with("player_"), names_to = "player")%>% 
  filter(complete.cases(value))

df_merged <- df_merged %>% dplyr::select(gameId, playId, nflId, Player_team, InPoss )
#merge MLo's data with the regression results
df_merged <- merge(df_merged, results, by.x= "nflId", by.y = "NflId")
#merge the long df from above with the df containing play details and results
results_per_play_long <- merge(results_per_play_long, df_merged,by.x = c("gameId", "playId","value"),by.y = c("gameId", "playId","nflId")) %>% 
  distinct(gameId, playId, value, .keep_all = TRUE)
#add survival time
results_per_play_long <- merge(results_per_play_long, summed_times, by.x = "stint.x", by.y = "stint")
#aggregate the long data to get the sum of player coefficients for each play
per_play <- aggregate(results_per_play_long$`fit$coef`, by=list(survival_time = results_per_play_long$survival_time, gameId=results_per_play_long$gameId,playId=results_per_play_long$playId), FUN=sum)


#isolate results for DBs only
results_per_play_def <- results_per_play_long %>% filter(position == "CB" | position == "FS" | position == "S" | position == "SS" |position == "DB")
results_per_play_def$position <-as.factor(results_per_play_def$position)
results_per_play_def$adj_score <- results_per_play_def$`fit$coef`

#get results per team (defensive only)
per_team <- results_per_play_def %>%  group_by(Player_team) %>% 
  summarise(score = sum(adj_score))
per_team <-per_team[order(per_team$score, decreasing = T),]
per_team$Player_team <-factor(per_team$Player_team, levels = per_team$Player_team)

#get results per positions (defensive only)
per_pos <- results_per_play_def %>%  group_by(position) %>% 
  summarise(score = sum(adj_score))
per_pos$position <-factor(per_pos$position,levels =per_pos$position)


install.packages("nflfastR") #has team colors and logos
library(nflfastR)
#get team branding as variable
data(teams_colors_logos)
#get PFF rankings of the secondary of all teams after week 17 from 2018. details from:
#https://www.pff.com/news/pro-nfl-secondary-rankings-all-32-teams-after-17-weeks
PFF_rank <- c(17, 1, 22, 11, 16, 18, 28, 23, 12, 20, 7, 6, 10, 8, 2, 21, 3, 19, 4, 14, 5, 30, 25, 27, 29, 15, 26, 32, 24, 13, 31, 9)
per_team$PFF_rank <-PFF_rank
#add team branding to per_team data
per_team <- merge(per_team, teams_colors_logos, by.x = "Player_team", by.y = "team_abbr")

install.packages("ggimage") #better plotting of text as point (geom_text)
library(ggimage)
#####PLOT  
#barchart of team results with logo
team_bars <-ggplot(per_team, aes(Player_team,(score/mean(score)))) + geom_bar(fill = per_team$team_color, stat = "identity")+ 
  geom_image(aes(image=team_logo_espn), size=.04)+ geom_hline(yintercept=1, color = "red", size = 2)+      
  geom_text(aes(label=PFF_rank), show.legend = T, position=position_dodge(width=0.9), hjust=-1)+theme_classic()+
  xlab("Teams")+ylab("Combined Secondary Score Per Team as a Proportion of the Mean (Red Line)")+ 
  coord_flip()+scale_x_discrete(limits = rev(levels(per_team$Player_team)))

#isolate results by def players
per_player <- results_per_play_def %>%  
  dplyr::select(value, displayName, Player_team, adj_score) %>% 
  group_by(value) %>% 
  summarise(displayName = displayName, Player_team = Player_team, score = min(adj_score), count = n())

per_player_select <- merge(per_player,teams_colors_logos, by.x = "Player_team", by.y = "team_abbr" )

```
```{r echo = FALSE, message = FALSE, warnings = FALSE}
#interactive plot of defenders
int_graph_def <- results_per_play_long %>%   filter(position == "CB" | position == "FS" | position == "S" | position == "SS" |position == "DB")%>%
  dplyr::select(value, displayName, Player_team, `fit$coef`, position) %>% 
  group_by(value) %>% 
  summarise(displayName = displayName, Player_team = Player_team, position = position, score = min(`fit$coef`), count = n()) %>%
  distinct()
int_graph_def <- merge(int_graph_def,teams_colors_logos, by.x = "Player_team", by.y = "team_abbr" )
int_graph_def$displayName <-gsub("'", "", int_graph_def$displayName)
int_graph_def$doOnClick = sprintf("window.open(\"%s%s\")",
                                  "https://www.google.com/search?hl=en&q=",
                                  as.character(int_graph_def$displayName)," NFL")

int_graph_def$Player_team <- as.factor(int_graph_def$Player_team)
int_graph_def$position <- as.factor(int_graph_def$position)
```


```{r echo = FALSE, message = FALSE, warnings = FALSE}
#interactive plot of all teams
int_graph <- results_per_play_long %>%  
  dplyr::select(value, displayName, Player_team, `fit$coef`, position) %>% 
  group_by(value) %>% 
  summarise(displayName = displayName, Player_team = Player_team, position = position, score = min(`fit$coef`), count = n()) %>%
  distinct()
int_graph <- merge(int_graph,teams_colors_logos, by.x = "Player_team", by.y = "team_abbr" )
int_graph$displayName <-gsub("'", "", int_graph$displayName)
int_graph$doOnClick = sprintf("window.open(\"%s%s\")",
                              "https://www.google.com/search?hl=en&q=",
                              as.character(int_graph$displayName)," NFL")

int_graph$Player_team <- as.factor(int_graph$Player_team)
int_graph$position <- as.factor(int_graph$position)
```


# Abstract




To identify the best players against the pass I developed a metric to rate defensive backs based on their ability to prevent wide receivers from getting open to receive a pass; by measuring the change in the time the quarterback takes to throw a pass when players are on the field.  

The table below shows the top twenty players by this metric, __six of the twenty (highlighted in yellow) were selected for the 2018 Pro-Bowl __:
  ```{r echo = FALSE, message = FALSE, warnings = FALSE}
install.packages("kableExtra") #better table formatting options
library(kableExtra)               
```                            

```{r echo = FALSE, results = "asis"}
pro.bowl <- c(1,3,10,11,17, 19)                          

library(knitr)  #nice tables in Rmarkdown
thetop10 <- head(topresulttable,20)  
#thetop20 <- topresulttable[11:20,]                           


kable(thetop10, caption = "Top 20 DBs",
      booktabs = TRUE, valign = 't', row.names = F) %>% row_spec(pro.bowl, bold = T, background = "yellow")                           
```

My model accounts for 91% of the change in time to throw and rates 2018 Pro Bowlers Byron Jones and Jamal Adams and 3 time Pro Bowler Antoine Bethea as the three best DBs at increasing the time a QB held onto the ball in the 2018 season, with the top rated defender Jones adding seven tenths of a second per play.

The interactive plot below shows the coefficient (time plus/minus score) for all defensive backs in 2018, by how many snaps they played. _mouse over for name, click to Google them_.

```{r echo = FALSE, message = FALSE, warnings = FALSE}
library(ggiraph) #interactive plotting, superseded by girafe package
#####PLOT 
#ggiraph/girafe interactive plot   
plot1 <- int_graph_def %>% ggplot(.,aes(x=count, y =score/10))+ 
  geom_image(aes(image=team_logo_espn), size=.04)+
  xlab("Snaps Played")+
  ylab("Seconds added to Pass")+
  theme_minimal()+ggtitle("Time to Pass Plus/Minus per Snap for All DBs (mouse over for name)")

#*tooltip displays information when the mouse hovers over a point.
#*data_id highlights associated points when the mouse is hovering.
#*onclick manages the action taken when a point is clicked.
p1 = plot1 + geom_point_interactive(aes(tooltip = displayName, data_id = displayName, onclick = doOnClick, alpha=0.0001), size = 1.5)+ theme(legend.position="none")
ggiraph(code = print(p1))
```

# Introduction


Frequently the best DBs would not register traditional statistics as a result of their zone/man not being targeted on plays, for instance in his prime CB Richard Sherman at the Seahawks would go whole games without passes being thrown to his side of the field[[1]](https://bleacherreport.com/articles/2187908-richard-sherman-targeted-zero-times-during-win-over-packers) because of the threat he posed: while he was not accruing stats he was having a big effect on play.  

I wanted a metric to capture the influence of defensive backs outside of tackles or interceptions, 
A metric based on differences in the time between the snap and the quarterback releasing a pass would reflect a defender's ability to influence play on and off the ball: by preventing their man from getting open or, if the passer avoids throwing in their direction entirely, by reducing passing options and forcing a wait for a second or third read receiver to get open.  
As the boxplot below shows: as time to throw passes 3 seconds, median Expected Points Added (EPA, the expected points value of the outcome) stays below zero. *Each box represents 0.5 seconds, the line in each box represents the median EPA of a pass at that time stamp.*  
  
  ```{r echo = FALSE, message = FALSE, warnings = FALSE}
epa_df <- inner_join(per_play,df_plays,by = c("gameId" = "gameId","playId" = "playId")) %>% filter(survival_time >19)

ggplot(epa_df,aes(survival_time/10, epa)) + 
  geom_boxplot(aes(group = cut_width(survival_time,5)), outlier.alpha = 0.1)+theme_classic()+labs(title = "Boxplot of EPA by Time to Pass", x = "Seconds to Pass (mean is 2.8)", y = "Expected Points Added")+xlim(c(2,7))+ geom_hline(yintercept=0, color = "red", size = 2)
```      

By tracking time to pass across all combinations of players on passing plays in the 2018 NFL season my model can compare results by combinations to apportion value to individual players.

Scores for all secondaries grouped by team are summarised in the plot below, with the number beside the logo representing Pro Football Focus' rank of that secondary after completion of the regular season[[2]](https://www.pff.com/news/pro-nfl-secondary-rankings-all-32-teams-after-17-weeks) (the extent of this competition's dataset). While some results match up (the Bears are 1st with PFF, 2nd here, PFF's bottom two are bottom five for cumulative score) overall it couldn't be said that they agree: the RMSE of this metric against PFF grade is 11 places.
                                                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                                                           ```{r echo = FALSE, message = FALSE, warnings = FALSE}
                                                                                                                                                                                                                                                                                                                                                                           #team barchart with logos
                                                                                                                                                                                                                                                                                                                                                                           plot(team_bars)
                                                                                                                                                                                                                                                                                                                                                                           ```                             
                                                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                                                           # Methods
                                                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                                                           This model takes the 19,000 passing plays in the dataset and divides them into 12,000 "stints": combinations of DBs, route-runners (any player given an identified route in the dataset) and quarterbacks. For each stint the number of plays and the total time to pass (or event that replaces a pass, like a sack, fumble or QB run) is tracked for each player present, this is then solved to get a time "plus/minus" value for each player that quantifies the positive or negative effect they have on time to pass.
                                                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                                                           The plot below shows time added (subtracted) to (from) the pass across all defensive backs, quarterbacks, and receivers. QBs account for the majority of time to pass, as one would expect, the best QBs are credited for adding over 1.25 seconds to their plays where the average time to pass in the dataset is 2.8 seconds  
                                                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                                                           ## The Maths
                                                                                                                                                                                                                                                                                                                                                                           
                                                                                                                                                                                                                                                                                                                                                                           For every stint (combination of players) the model takes the formula
                                                                                                                                                                                                                                                                                                                                                                           $Y_p$ = $\beta_1X_p$ + $\beta_2X_p$.. + $\beta_nX_p$  
                                                                                                                                                                                                                                                                                                                                                                             
                                                                                                                                                                                                                                                                                                                                                                             Where $Y_p$ is predicted time to pass per play in stint $p$, $\beta_n$ is the coefficient or value of player $n$, for which we are solving and $X_p$ represents the number of snaps in stint $p$
                                                                                                                                                                                                                                                                                                                                                                             
                                                                                                                                                                                                                                                                                                                                                                             Ridge regression is used to solve for the coefficients (player scores). This is an adaption of linear regression that shrinks coefficients and reduces variance and multicollinearity at the cost of a slight increase in bias[[3]](https://www.jstor.org/stable/2285435?seq=1) [(See Section 4.1)](https://www.kaggle.com/connyfromtheblock/markdownworking#section-problems)        
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 the plot below shows predicted against actual time to throw for the dataset, with the regression line in blue: 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ```{r echo = FALSE, message = FALSE, warnings = FALSE}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 #####PLOT 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 #                          
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 df$new_y <- predict(lmfit, df[1:951])    
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 lmplot <- ggplot(df,aes(new_y/10, summed/10)) + 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   geom_point()+geom_smooth(method='lm')+theme_classic()+labs(title = "Predicted Vs Actual", x = "Predicted", y = "Actual")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 plot(lmplot)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ```   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ```{r echo = FALSE, message = FALSE, warnings = FALSE}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 paste0("The R^2 of this model is ", round(summary(lmfit)$r.squared,2))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ```
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 meaning the model can account for 91% of the change in time to pass. the high $R^2$ is somewhat influenced by outliers as the average stint duration is 4.4 seconds.  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 The tables below shows the best 10 quarterbacks, wide receivers and defensive backs by this metric, depending on their goals:  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   QBs and DBs are rated by most time added, WRs by least.   
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ```{r echo = FALSE, results = "asis"}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 thebottom10 <-  head(botresulttable,10)  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 QBtop10 <-  head(qbresulttable,10)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 DBtop10 <-  head(topresulttable,10)  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 #side by side tables
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 kable(list(QBtop10, thebottom10, DBtop10), caption = "Top QBs, WRs and DBs",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       booktabs = TRUE, valign = 't', row.names = F)%>% 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   kable_styling(font_size = 9)                          
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ```    
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ## Background
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 This is an adaption of a basketball statistic known as a Regularized Adjusted Plus-Minus, that attempts to account for a player's contribution independent of other players on the court at the same time. APM was popularised in NBA analytics circles around the turn of the century: current Pistons Director of Basketball Strategy, then blogger, Dan Rosenbaum started writing about this in 2005 [[4]](http://danrosenbaum.blogspot.com/2005/07/defense-for-big-men-what-do-adjusted.html) and ESPN has been calculating their own version since 2014[[5]](https://www.espn.com/nba/story/_/id/10740818/introducing-real-plus-minus).    
A traditional Plus-Minus statistic for a player is simply points scored while on court less points conceded while on court (adjusted for rate). This is the statistic that gained Houston Rocket Shane Battier renown as the "No Stat All Star" [[6]](https://www.nytimes.com/2009/02/15/magazine/15Battier-t.html) for his positive influence on play that was not captured by box-score statistics.
                                                                                                                                 
An Adjusted Plus-Minus augments this by solving each stint as a linear equation to apportion scores over all the players on Court.  
                                                                                                                                 
A Regularized Adjusted Plus-Minus improves accuracy further by regularising results to reduce error.
                
                            


# Problems                                                                                                                         
                                                                                                        

## Multicollinearity: The Sad Tale of Kevin Byard
                              
![Sad Kevin Byard](https://www.yardbarker.com/media/5/d/5d78390a8af2e2a84fff85808df7fdd6d9ea9bd3/thumb_16x9/11444325.jpg)    
                              
Kevin Byard (FS) and Kenny Vaccaro (SS) were the starting safeties for the Titans in 2018: 
Byard started the season as first team All-Pro, finished it as Pro Football Focus' 4th rated Safety [[7]](https://www.pff.com/news/pro-pff-ranks-the-top-10-safeties-ahead-of-the-2019-nfl-season) and in after the 2018 season Tennessee rewarded him with the largest contract ever for a safety.  
                                                                                                                                 
However in the eyes of the model Byard is rated as the worst defensive back in the NFL, as effective at speeding up the pass, and thus working against his team's interests, as Juju Smith-Schuster: the 7th best WR at this metric.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 This is due to a common problem with adjusted plus minus calculations: multicollinearity. Where 2 players mostly play at the same time the model can rely on a small number of separate plays to decide their value.  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 The plot below of all DB's has all Tennessee Titans players highlighted, at the top Titans strong safety Kenny Vaccaro and backup (to Byard) Kendrick Lewis add half and a quarter of a second to the time to throw, respectively. Meanwhile Kevin Byard occupies a space on his own in the bottom right, near other pro-bowl players suffering the same fate: Casey Hayward and Patrick Peterson. 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ```{r echo = FALSE, message = FALSE, warnings = FALSE}
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 highlights <- int_graph_def %>% filter(Player_team == "TEN")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 #####PLOT 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 #  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 plot4 <- int_graph_def %>% ggplot(.,aes(x=count, y =score/10))+geom_point()+
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   geom_point(data = highlights, aes(x=count, y =score/10), color = "red", size = 3)+
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   geom_text(data = highlights, aes(x=count, y =score/10,label = displayName), color = "red", show_guide=FALSE, nudge_y = 0.05)+
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   xlab("Snap Count")+
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ylab("Seconds added to Pass")+
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   theme_minimal()+ggtitle("Tennessee Titans DBs")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 plot(plot4)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ```
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ## Sample Size
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 As the previous plot shows, coefficients remain close to zero until players have played around 400 snaps, probably as playing less than this does not offer enough opportunities to differentiate one player from another.  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 A side-effect of this is that the players with the highest (and lowest) scores are also those who play the most.                                                                                                                         
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ## Lack of Context
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 This is a simple model: all players and plays are treated the same meaning some important context is not considered:  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   .	Different levels of play time or fatigue between players in a stint  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 .	Game Score  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 .	Season Standing (a week 17 game with both teams out of play-off contention is treated the same as any other)  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 .	Down/play situation  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 # Suggested Developments
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 The multicollinearity that incorrectly apportions the coefficient among players who play together frequently in the model could be reduced by adding more games to the dataset and adding to the numbers of stints. Additional games could come from the 2018 post-season or previous seasons, potentially weighted to reduce importance, would improve reliability of scores.  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Further insight could be provided by extending the dataset to include all defensive players: it would be interesting to quantify the effect the best pass rushing defensive ends (Aaron Donald) have on time to pass, however as detailed above this would necessitate adding even more stints to the data and since, as mentioned previously, the model has an $R^2$ of 0.91 there is at best 9% of the movement in the metric unaccounted for.                                                                                                                     
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 Another approach, used by PFF in their EPA based plus minuses [[8]](https://www.pff.com/news/nfl-a-new-valuation-metric-for-cornerbacks-pff-plus-minus)  is to use clustering of (statistically) similar players to extend the data, reasoning that if player X performs well against a combination and player Y is similar to player X, player Y would perform as well against the same players.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 # Conclusion
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 In NFL pass plays from the 2018 regular season a model based on the players on field can account for 91% of the movement in the quarterback's time to throw. An adjusted plus minus based on this shows promise in identifying the best defensive backs, by quantifying their ability to influence the game outside what is measured by traditional statistics.  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 While results for the most part seem an accurate evaluation of a player's ability some players considered among the best receive extremely low scores, this is due to difficulties in separating players who play together frequently: for the metric to provide reliable appraisals more than 17 weeks data are required.