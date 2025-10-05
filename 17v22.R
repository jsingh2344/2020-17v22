#To install necessary packages, remove the hashes:

# install.packages("tidyverse")
# install.packages("nflfastR")
# install.packages("ggimage")
# install.packages("gt")
# install.packages("nflreadr")
# install.packages("remotes")
# install.packages("RCurl")
# install.packages("readr")
# install.packages("ggthemes")
# remotes::install_github("jthomasmock/espnscrapeR")

# Load packages
{library(tidyverse)
library(reshape2)
library(ggthemes)
library(nflfastR)
library(ggimage)
library(gt)
library(tidyr)
library(nflreadr)
library(espnscrapeR)
library(readr)
library(jsonlite)
library(ggplot2)
library(furrr)
library(future)
library(ggrepel)
}
        
#NFLFastR, PFRStats 
{

{
#load 2019-2021 data (Tannehill's tenure in Tennessee)
{
rt_pbp <- load_pbp(2019:2021)

titans_pbp <- rt_pbp %>%
  filter(posteam == 'TEN') %>%
  filter(rush == 1 | pass == 1) %>%
  filter(!is.na(epa)) 
}

#First, summary of 17's vs 22's effectiveness per play
{

#WPA summary
{

#Down-based wpa for down < 3 and win probability is between 10 and 90
down_wpa_epa <- titans_pbp %>% 
  filter(passer == "R.Tannehill" | rusher == "D.Henry", down < 3) %>%
  group_by(down, name) %>%
  summarize(
    mean_epa = mean(epa), plays = n(), wpa_per30 = mean(wpa) * 30
  )
}
#Plot of down-based wpa
{
down_split_wpa_bar <- ggplot(down_wpa_epa, aes(fill=name, x=down, y=wpa_per30)) +
  geom_bar(position='dodge', stat='identity') +
  geom_text(aes(label = down), position = position_dodge(0.9))

down_split_wpa_bar <- down_split_wpa_bar + theme(
  axis.text.x = element_blank())
down_split_wpa_bar <- down_split_wpa_bar +
  labs(title = "Win % Added per 30 Plays: Ryan Tannehill and Derrick Henry",
       subtitle = "Win percentage between 10 and 90%, down < 3",
       caption = "Data from NFLFastR")
down_split_wpa_bar
}

#epa plot
{
down_split_epa_bar <- ggplot(down_wpa_epa, aes(fill=name, x=down, y=mean_epa)) +
  geom_bar(position='dodge', stat='identity') +
  geom_text(aes(label = down), position = position_dodge(0.9))

down_split_epa_bar <- down_split_epa_bar + theme(
  axis.text.x = element_blank()) +
  labs(title = "EPA Splits: Tannehill passes, Henry runs",
       subtitle = "down < 3, 10 < wpa < 90",
       caption = "NFLFastR")
down_split_epa_bar
}
  
#22 in the context of the league's running backs
  
#Loading RB data since 2018:
{
    rb <- load_pbp(2018:2021) %>%
      filter(play_type == 'run') %>%
      filter(!is.na(rusher_player_name)) %>%
      group_by(rusher_player_name) %>%
      summarize(
        epa = mean(epa), carries = n()
      ) %>%
      filter(carries > 100) %>%
      arrange(-carries)
}
  
  #Plot
  {
    rushing_effectiveness <- ggplot(rb, aes(x=carries, y=epa, fill = epa)) +
      geom_point(aes(color = epa)) +
      geom_text(aes(label=rusher_player_name),hjust=0, vjust=0)
    rushing_effectiveness
  }
  
  #Whereas Tannehill:
  {
    wp_tannehill <- load_pbp(2019:2021) %>%
      filter(play_type == 'pass') %>% 
      group_by(passer_player_name) %>%
      summarize(
        mean_wpa = mean(wpa) * 30, plays = n(), team=posteam, epa=epa) %>%
      arrange(-mean_wpa) %>%
      ungroup() %>%
      filter(plays > 450) 
    
    head(teams_colors_logos)
    
    wp_tannehill <- wp_tannehill %>%
      left_join(teams_colors_logos, by = c('team' = 'team_abbr'))
  }
  
  #Plot
  qb_wp <- wp_tannehill %>%
    ggplot(aes(x = mean_wpa, y = fct_reorder(passer_player_name, mean_wpa))) + 
    #geom_image(aes(image = team_logo_espn), size = wp$plays / 450, asp = 16 / 9)
    geom_point(color = wp_tannehill$team_color, size = wp_tannehill$plays / 450) + 
    labs(x = "WPA per 30 plays",
         y = "QBs",
         title = "Quarterback WPA, 2019-2021",
         caption = "Data: @nflfastR") +
    theme_bw()
  qb_wp

}

#Henry supports Tannehill's success?
{

#correlation between good running (efficient running) and passing?
{
  home_correlation <- titans_pbp %>%
    filter(passer == "R.Tannehill" | rusher == "D.Henry", down < 4) %>%
    filter(play_type == 'run' | play_type == 'pass', home_team == 'TEN') %>%
    group_by(game_id) %>%
    summarize(
      rush_epa = last(total_home_rush_epa),
      pass_epa = last(total_home_pass_epa)
    ) 


#when TEN is away
away_correlation <- titans_pbp %>%
  filter(passer == "R.Tannehill" | rusher == "D.Henry", down < 4) %>%
  filter(play_type == 'run' | play_type == 'pass', home_team != 'TEN') %>%
  group_by(game_id) %>%
  summarize(
    rush_epa = last(total_away_rush_epa),
    pass_epa = last(total_away_pass_epa)
  ) 



} 
#nope_home plot:
{
home_correlation_dotplot <- ggplot(home_correlation, aes(x=rush_epa, y=pass_epa))  +
  geom_point(aes(color='000000')) +
  geom_smooth(se = FALSE, color = "black", method = "lm") +
  labs(title = "Titans Home Offensive Passing EVA vs Rushing EPA",
       caption = "data from NFLFasR")
home_correlation_dotplot
}
#nope_away plot:
{
away_correlation_dotplot <- ggplot(away_correlation, aes(x=rush_epa, y=pass_epa))  +
  geom_point(aes(color='000000')) +
  geom_smooth(se = FALSE, color = "black", method = "lm") +
  labs(title = "Titans Away Offensive Passing EVA vs Rushing EPA",
        caption = "data from NFLFasR")
away_correlation_dotplot
}



#general correlation for all teams, with same result (plot and data summary)
{
correlation_allteams_rushing_passing <- load_pbp(2019:2021) %>%
  select(total_home_rush_epa, total_home_pass_epa)


general_correlation_dotplot <- correlation_allteams_rushing_passing %>%  
  ggplot(aes(x=total_home_rush_epa, y=total_home_pass_epa))  +
  geom_point(aes(color='000000')) +
  geom_smooth(se = FALSE, color = "black", method = "lm") +
  labs(title = "2019-2021: Offense Rushing EPA vs Passing EPA")

general_correlation_dotplot
}


#Do rushing and passing success correlate based on rush attempts?

#Data loading, summarizing:
{
attempt_correlation <- titans_pbp %>%
  filter(passer == "R.Tannehill" | rusher == "D.Henry", down < 4) %>%
  filter(play_type == 'run' | play_type == 'pass') %>%
  group_by(game_id) %>%
  summarize(
    rush_attempts = last(sum(rush_attempt)),
    pass_success_epa = last(sum(epa))
  ) 
}

#...and? Yes!.
attempt_correlation_dotplot <- attempt_correlation %>%
  ggplot(aes(x=rush_attempts, y=pass_success_epa)) +
  geom_point(aes(color='00ff00')) +
  geom_smooth(se = FALSE, color = "black", method = "lm") +
  labs(title = "Do rushing attempts correlate with passing success?",
       subtitle = "Titans Offense, 2019-2021",
       caption = "Data from NFLFastR")
attempt_correlation_dotplot



#But use only plays where before the Titans have a 2-score lead:

#Data loading + filtering
{
wpadjusted_pbp <- titans_pbp %>%
  filter(vegas_wp < 80) %>%
  filter(score_differential < 9)

attempt_correlation_adjusted <- wpadjusted_pbp %>%
  filter(passer == "R.Tannehill" | rusher == "D.Henry", down < 4) %>%
  filter(play_type == 'run' | play_type == 'pass') %>%
  group_by(game_id) %>%
  summarize(
    rush_attempts = last(sum(rush_attempt)),
    pass_success_epa = last(sum(epa)),
  ) 
}

#...and? Much less dramatic!

attempt_correlation_graph <-
  ggplot(attempt_correlation_adjusted, aes(x=rush_attempts, y=pass_success_epa)) +
  geom_point(aes(color='00ff00')) +
  geom_smooth(se = FALSE, color = "black", method = "lm") +
  labs(title ="Does passing success correlate with rushing attempts outside of garbage time?",
       subtitle = "Vegas WP under 80%",
       caption = "data from NFLFastR")
attempt_correlation_graph
}
} #Exclusively NFLFastR

#Visualisations involving PFR stats - providing pressure pct context
{
pfr <- load_pfr_advstats(2020)
pfr_stats <- load_player_stats(2020)

#Ryan Tannehill's Pressure Rate

#Data loading and molding: pressure vs EPA for QBs in 2020
{
names(pfr)[names(pfr) == 'team'] <- 'recent_team'

pressure_pfr <- pfr %>%
  group_by(pfr_player_name, season, recent_team) %>%
  summarize(
    presspct = mean(times_pressured_pct), pressures=sum(times_pressured)
  ) %>%
  filter(pressures > 30) 

qb_pfr_stats <- pfr_stats %>%
  group_by(player_name, recent_team) %>%
  summarize(
    epa = mean(passing_epa), plays = sum(attempts)
  ) %>%
  filter(plays > 150)

qb_pfr <- merge(qb_pfr_stats, pressure_pfr, by='recent_team') 

#color gradient function
rbPal <- colorRampPalette(c('red','blue'))

qb_pfr$Col <- rbPal(10)[as.numeric(cut(qb_pfr$epa*qb_pfr$presspct,breaks = 10))]
}

#Graph (with Gradient!)
{
qb_pressure_epa_graph <- qb_pfr %>%
  ggplot(aes(x=presspct, y=epa, label=player_name)) +
  geom_point() +
  geom_text(check_overlap = TRUE, color = qb_pfr$Col,hjust = 0, nudge_x = 0.008) +
  geom_smooth(se = FALSE, color = "black", method = "lm") +
  theme_bw() +
  labs(title = "Pressure vs EPA, QBs in 2020",
       caption = "Data from Pro Football Reference")
qb_pressure_epa_graph
}
}

#Visualisations involving PFR stats - the play-action vs rushing success relationship
{

#Seasonal Advanced PFR data
{
urlfile="https://raw.githubusercontent.com/nflverse/pfr_scrapR/master/data/pfr_advanced_passing.csv"
scrapR <- read_csv(url(urlfile))
}

#Explore correlation between play action and rushing success for the season of 2020
{
playaction <- scrapR %>%
  filter(season == 2020) %>%
  summarize (
    plays = pass_attempts, yards_per_pa = pa_pass_yards / pa_pass_att, player = player, team = team
  ) %>%
  filter(plays > 70)

runavg_2020 <- load_pbp(2020) %>%
  filter(play_type == 'run') %>%
  group_by(posteam) %>%
  summarize(
    avg = mean(yards_gained)
  )

names(runavg_2020)[names(runavg_2020) == 'posteam'] <- 'team'

PAvRun <- merge(runavg_2020, playaction, by='team')

PAvRun$Col <- rbPal(10)[as.numeric(cut(PAvRun$yards_per_pa, breaks = 10))]

PAvRun_dotplot <- PAvRun %>% 
  ggplot(aes(x=avg, y=yards_per_pa)) +
  geom_point(size = PAvRun$plays / 250, color = PAvRun$Col) +
  geom_smooth(se = FALSE, color = "black", method = "lm") +
  labs(title = "2020 PA versus Rushing Efficiency",
       caption = "Data from PFR and NFLFastR")
PAvRun_dotplot


#Fairly interesting, though the available data sets don't allow for further adjustment
#To weed out the noise from irregular game factors
}

#How about effectiveness plotted against pocket time? (2020)
#Team-based because of how nflfastR formats player name strings
{
simplepassing <- load_pbp(2020) %>%
  group_by(posteam) %>%
  summarise(
    epa = mean(epa), wpa30 = mean(wpa) * 30, plays = n()
  ) 

names(simplepassing)[names(simplepassing) == 'posteam'] <- 'team'

pocket_scrapR <- scrapR %>%
  filter(season == 2020) %>%
  group_by(team) %>%
  summarize (
    time = mean(pocket_time)
  )

TimevEpa <- merge(pocket_scrapR, simplepassing, by='team')
TimevEpa$Col <- rbPal(10)[as.numeric(cut(TimevEpa$epa, breaks = 10))]


TimevEPA_dotplot <- TimevEpa %>% 
  ggplot(aes(x=time, y=epa, label=team)) +
  geom_point(color = TimevEpa$Col, size = TimevEpa$plays / 450) +
  geom_text(check_overlap = TRUE, hjust = 0, nudge_x = 0.05) +
  theme_bw() +
  labs(title = "2020 Offensive Passing EPA versus Mean Passer Throw Time",
       subtitle = "Average is not weighted by attempts",
       caption = "Data from PFR and NFLFastR")
TimevEPA_dotplot
}
}


# Now for some interesting visualizations of Titans games: let's start with Week 17, 2020:

{
#loading in ungrouped pbp data
game_data20.17 <- titans_pbp %>%
  filter(week==17, season==2020, posteam == "TEN") %>%
  arrange(game_seconds_remaining)



#basic dotplot
wk17 <- ggplot(game_data20.17, aes(y=epa, x=game_seconds_remaining)) +
  geom_point() +
  geom_segment(aes(x=game_seconds_remaining, y=epa, xend=game_seconds_remaining, yend=0), 
               color= game_data20.17$pass + .2 + 2) +
  labs(title = "2020, Week 17, Titans vs Texans, Titans Offense",
       caption = "Data: @nflfastR", 
       subtitle = "Green is for passes, red for rushing attempts") +
  theme_bw()

wk17 <- wk17 + geom_point(color='#ce4822') 
wk17

#More interesting might be the Seattle comeback in week 2 of 2021

game_data21.2 <- titans_pbp %>%
  filter(week==2, season==2021, posteam == "TEN") %>%
  arrange(game_seconds_remaining)



wk2 <- ggplot(game_data21.2, aes(y=epa, x=game_seconds_remaining)) +
  geom_point(color="#226fce") +
  geom_segment(aes(x=game_seconds_remaining, y=epa, xend=game_seconds_remaining, yend=0), 
               color=game_data21.2$pass + .2 + 2) +
  labs(title = "2021, Week 2, Titans vs Seahawks, Titans Offense",
       caption = "Data: @nflfastR", 
       subtitle = "Green is for passes, red for rushing attempts") +
  theme_bw() +
  geom_point(color="#226fce")


  
wk2
}

}

#Sportradar
{

#Merging and cleaning data with FastR
{
  SR_2021_pbp <- readRDS("/Users/jsingh23/Desktop/R/Sportradar/Data/pbp_2020.rds")
  SR_2021_part <- readRDS("/Users/jsingh23/Desktop/R/Sportradar/Data/participation_2020.rds")
  fastr20 <- load_pbp(2020) 
  SR_2021_pbp$game_id <- str_replace_all(SR_2021_pbp$game_id, "JAC", "JAX")
  merged_SR <- merge(SR_2021_pbp, SR_2021_part, by="sr_id")
  merged_SR <- merged_SR %>%
    mutate(play_id = play_id.x)
  SR_fastr <- merge(merged_SR, 
              load_pbp(2020), 
              by=c("play_id", 
              "game_id"))
  SR_fastr %>% saveRDS("/Users/jsingh23/Desktop/R/SR_fastr")
  SR_fastr <- readRDS("/Users/jsingh23/Desktop/R/SR_fastr")
}

#There is an argument that Tannehill's effectiveness is based on the run threat, or play action:
{ 
  league_shotgun <- SR_fastr %>%
    filter(shotgun == 1) %>%
    filter(!is.na(passer_player_name)) %>%
    group_by(passer_player_name) %>%
    summarize(
     epa = mean(epa), plays = n(), box = mean(men_in_box)
    ) %>%
    filter(plays > 70) %>%
    arrange(-epa) 
  
  {
  
  shotgun_graph <- league_shotgun %>%
    filter(plays > 200) %>%
    arrange(-epa) %>%
    filter(!is.na(box)) %>%
    mutate(across(starts_with("box"), round, 1)) %>%
    ggplot(aes(x=reorder(passer_player_name, -epa), y=epa)) + 
    geom_point(size = 3, aes(color = factor(epa))) + 
    geom_segment(aes(x=passer_player_name, 
                     xend=passer_player_name, 
                     y=0, 
                     yend=epa)) +
    theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
    theme(legend.position = "none") +
    geom_text(aes(label = box), position = position_nudge(y = 0.03), alpha = 1, color = "skyblue") +
    labs(title = "2020 Selected Quarterbacks EPA/Play Out of Shotgun", subtitle = "Labels represent average number of men in box",
         y = "EPA/Play", x = "Passer", caption = "Data from Sportradar and NFLFastR")
    shotgun_graph

  } #Shotgun graphs
} #Shotgun data + viz
  
  
{
  {
 DH_On <- SR_fastr %>%
   filter(posteam == "TEN") %>%
   filter(grepl("Derrick Henry", home_names, fixed=TRUE) | 
            grepl("Derrick Henry", away_names, fixed=TRUE)) %>%
   mutate(DH = 1)
 DH_Off <- SR_fastr %>%
   filter(posteam == "TEN") %>%
   filter(play_type == "run" | play_type == "pass") %>%
   filter(!grepl("Derrick Henry", home_names, fixed=TRUE) & 
            !grepl("Derrick Henry", away_names, fixed=TRUE)) %>%
   mutate(DH = 0)
 participation_22 <- rbind(DH_Off, DH_On)
  } #Loading participation data
 
  #Density EPA graph
  { 
 DH_splits_density <- participation_22 %>% ggplot(aes(x=epa, group=DH, fill = DH)) +
          geom_density(adjust=1.5, alpha=.4) +
   theme_few() +
   theme(legend.position = "none") +
   labs(title = "The Titans 2020 Offense - Derrick Henry Splits",
        subtitle = "Plays with Henry on the field are represented by blue; without, by grey. 
Early downs, win percentage between 5 and 95 percent. Two minute drills are excluded",
        caption = "Data from Sportradar and NFLFastR") +
   theme(plot.subtitle=element_text(size=10))
    DH_splits_density
  } 
 
 
  #Getting summarized passing and rushing splits 
 {
   DH_passing <- participation_22 %>%
     filter(down == 1 | down == 2) %>%
     filter(play_type == "pass") %>%
     filter(half_seconds_remaining > 120) %>%
     filter(posteam == "TEN") %>%
     filter(wp > 5 | wp < 95) %>%
     group_by(DH) %>%
     summarize(
       passing_epa = mean(epa), passing_plays = n(), passing_success = mean(success)
     )
   DH_rushing <- participation_22 %>%
     filter(down == 1 | down == 2) %>%
     filter(play_type == "run") %>%
     filter(half_seconds_remaining > 120) %>%
     filter(posteam == "TEN") %>%
     filter(wp > 5 | wp < 95) %>%
     group_by(DH) %>%
     summarize(
       rushing_epa = mean(epa), rushing_plays = n(), rushing_success = mean(success)
     )
    
 DH_overall <- merge(DH_rushing, DH_passing, by="DH") 
 
 
 #Fit data for a bar graph:
 
 DH_overall <- as.data.frame(t(as.matrix(DH_overall)))
 DH_overall <- DH_overall %>%
   mutate(On = V2) %>%
   mutate(Off = V1)
 DH_overall$V1 <- NULL
 DH_overall$V2 <- NULL
 
 
 
 DH_overall <- rownames_to_column(DH_overall, var = "rowname")
 DH_barcomp <- pivot_longer(DH_overall, -rowname, names_to="variable", values_to="value")
 DH_barcomp <- DH_barcomp[-c(5, 6, 11, 12), ]
 DH_barcomp <- DH_barcomp[-c(1, 2), ]
   }#Getting summarized early down epa and success splits
 
 DH_splits_bar <- ggplot(DH_barcomp,aes(x = rowname,y = value)) + 
   geom_bar(aes(fill = variable),stat = "identity",position = "dodge") +
   labs(title = "2020 Titans Offense Derrick Henry On/Off Splits", subtitle = "Early downs, win percentage between 5 and 95 percent, two minute drills excluded",
        y = "Value", x = "Variable", caption = "Data from Sportradar and NFLFastR")

 DH_splits_bar
   
 
 
 
 
 
    
    
} #Derrick Henry Splits

}


  
  
  
  
  

  
  
    
  
 
  
  
  
  
  
  
  

  



  
  
    
 


  









  







  

  








  
  
  


  

  

  
  
