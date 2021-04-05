library(tidyverse)
dir1<- dir(pattern ='week\\d+') %>% purrr::map_df(~read_csv(.))

players <- read.csv("players.csv")
games <- read.csv("games.csv")

defence <-c("CB", "DB", "S", "MLB", "ILB", "DE", "DT", "FS", "LB", "NT", "OLB", "SS")
offense <-c("QB", "RB", "WR", "TE", "FB", "HB")
special <-c("K", "LS", "P")


players <- players %>% mutate(
  category = case_when(
    position %in% defence ~ 'defence',
    position %in% offense ~ 'offense',
    position %in% special ~ 'special'
  )
)

allgames <- merge(dir1, games, by = "gameId")
allgames$PlayerTeam <- ifelse(allgames$team == "home", allgames$homeTeamAbbr, allgames$visitorTeamAbbr)



justATL<-allgames %>% filter(PlayerTeam == "ATL")
justATLDef <- justATL %>% mutate(
  category = case_when(
    position %in% defence ~ 'defence',
    position %in% offense ~ 'offense',
    position %in% special ~ 'special'
  )
)
justATLDef <- justATLDef %>% filter(category == "defence")
justATLDef$displayName <- as.factor(justATLDef$displayName)
levels(justATLDef$displayName)