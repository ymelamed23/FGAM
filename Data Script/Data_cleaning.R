library(hoopR)
library(ggplot2)
library(tidyverse)
library(plotly)
library(geomtextpath)
library(ggimage)
setwd()
output = 
########################################### simple model##################################################################
options(bitmapType='cairo')
#load player shots
player = data.frame(nba_leaguedashplayershotlocations(league_id = '00', season = year_to_season(most_recent_nba_season() - 1)))
#make numeric
player= player %>% 
  mutate_at(7:29, as.numeric)
#calculate total averages
player_avgs = player %>% 
  summarise(midrange_avg = sum(ShotLocations.Mid_Range_FGM, na.rm = TRUE)/sum(ShotLocations.Mid_Range_FGA, na.rm = TRUE),
            midrange_shots = sum(ShotLocations.Mid_Range_FGA, na.rm = TRUE),
            midrange_makes = sum(ShotLocations.Mid_Range_FGM, na.rm = TRUE),
            restricted_avg = sum(ShotLocations.Restricted_Area_FGM, na.rm = TRUE)/sum(ShotLocations.Restricted_Area_FGA, na.rm = TRUE),
            restricted_shots = sum(ShotLocations.Restricted_Area_FGA, na.rm = TRUE),
            restricted_makes = sum(ShotLocations.Restricted_Area_FGM, na.rm = TRUE),
            in_paint_non_ra_avg = sum(ShotLocations.In_The_Paint_Non_RA_FGM, na.rm = TRUE)/sum(ShotLocations.In_The_Paint_Non_RA_FGA, na.rm = TRUE),
            in_paint_non_ra_shots = sum(ShotLocations.In_The_Paint_Non_RA_FGA, na.rm = TRUE),
            in_paint_non_ra_makes = sum(ShotLocations.In_The_Paint_Non_RA_FGM, na.rm = TRUE),
            above_break_3_avg = sum(ShotLocations.Above_the_Break_3_FGM, na.rm = TRUE)/sum(ShotLocations.Above_the_Break_3_FGA, na.rm = TRUE),
            above_break_3_shots = sum(ShotLocations.Above_the_Break_3_FGA, na.rm = TRUE),
            above_break_3_makes = sum(ShotLocations.Above_the_Break_3_FGM, na.rm = TRUE),
            general_corner_3_avg = sum(ShotLocations.Corner_3_FGM, na.rm = TRUE)/sum(ShotLocations.Corner_3_FGA, na.rm = TRUE),
            general_corner_3_shots = sum(ShotLocations.Corner_3_FGA, na.rm = TRUE),
            general_corner_3_makes = sum(ShotLocations.Corner_3_FGM, na.rm = TRUE))

#join with player data
player = player %>% 
  mutate(midrange_ex = ShotLocations.Mid_Range_FGA*(player_avgs$midrange_makes-ShotLocations.Mid_Range_FGM)/(player_avgs$midrange_shots-ShotLocations.Mid_Range_FGA),
         midrange_o_ex = ShotLocations.Mid_Range_FGM-midrange_ex, 
         restricted_ex = ShotLocations.Restricted_Area_FGA*(player_avgs$restricted_makes-ShotLocations.Restricted_Area_FGM)/(player_avgs$restricted_shots-ShotLocations.Restricted_Area_FGA),
         restricted_o_ex = ShotLocations.Restricted_Area_FGM-restricted_ex,
         in_paint_non_ra_ex = ShotLocations.In_The_Paint_Non_RA_FGA*(player_avgs$in_paint_non_ra_makes-ShotLocations.In_The_Paint_Non_RA_FGM)/(player_avgs$in_paint_non_ra_shots-ShotLocations.In_The_Paint_Non_RA_FGA),
         in_paint_non_ra_o_ex = ShotLocations.In_The_Paint_Non_RA_FGM-in_paint_non_ra_ex,
         #left_corner_3_ex = ShotLocations.Corner_3_FGA*player_avgs$left_corner_3_avg,
         #left_corner_3_o_ex = ShotLocations.Corner_3_FGM - left_corner_3_ex,
         #right_corner_3_ex = ShotLocations.Right_Corner_3_FGA*player_avgs$right_corner_3_avg,
         #right_corner_3_o_ex = ShotLocations.Right_Corner_3_FGM-right_corner_3_ex,
         above_break_3_ex = ShotLocations.Above_the_Break_3_FGA*(player_avgs$above_break_3_makes-ShotLocations.Above_the_Break_3_FGM)/(player_avgs$above_break_3_shots-ShotLocations.Above_the_Break_3_FGA),
         above_break_3_o_ex = ShotLocations.Above_the_Break_3_FGM-above_break_3_ex,
         corner_3_ex = ShotLocations.Corner_3_FGA*(player_avgs$general_corner_3_makes-ShotLocations.Corner_3_FGM)/(player_avgs$general_corner_3_shots-ShotLocations.Corner_3_FGA),
         corner_3_o_ex = ShotLocations.Corner_3_FGM-corner_3_ex,
         total_3_o_ex = above_break_3_o_ex+corner_3_o_ex,
         total_o_ex = midrange_o_ex + restricted_o_ex + in_paint_non_ra_o_ex + corner_3_o_ex +above_break_3_o_ex,
         total_o_ex_points = midrange_o_ex*2 + restricted_o_ex*2 + in_paint_non_ra_o_ex*2 + corner_3_o_ex*3 +above_break_3_o_ex*3)
######## get headshot
player = player %>% 
  mutate(head=nba_playerheadshot(player_id = ShotLocations.PLAYER_ID))


################################################ analysis ###############################################################
#check if means of xFG are = 0
summary(player)
#check for NAs
player %>% 
  filter(is.na(total_o_ex_points))
############################################## viz ####################################################################
#make long data
player_wide = player %>% 
  select(ShotLocations.PLAYER_NAME, ShotLocations.TEAM_ABBREVIATION, head, restricted_o_ex, in_paint_non_ra_o_ex, midrange_o_ex, corner_3_o_ex, above_break_3_o_ex, total_3_o_ex,
         total_o_ex) %>% 
  
  rename("Above Break 3"="above_break_3_o_ex", "Corner 3"="corner_3_o_ex", "Restricted"="restricted_o_ex", "Midrange"="midrange_o_ex", 
         "Paint Not-Restricted"="in_paint_non_ra_o_ex", "Total 3"="total_3_o_ex", "Total FGAM"="total_o_ex") %>% 
  pivot_longer(cols = c(4:10), names_to = "Shot_Type", values_to = "FGAM")

#write as csv
write.csv(player_wide, paste0(output,'nba_FGAM.csv'), row.names = FALSE)

