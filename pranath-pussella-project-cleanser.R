library(tibble)
library(dplyr)
library(caret)
library(cricketdata)

# Getting Player IDs - Don't re-run the following commented code unless you want to fetch data from ESPNCricinfo

# ipl_registry = read.csv("data/ipl_registry.csv")
# ipl_registry$batting_style = NA
# ipl_registry$bowling_style = NA
# head(ipl_registry)
# n_ipl_reg = dim(ipl_registry)[1]
# 
# for(i in 1:n_ipl_reg) {
#   row = ipl_registry[i, ]
#   player_meta = fetch_player_meta(row$key_cricinfo)
#   ipl_registry[i, c("batting_style")] = player_meta$batting_style
#   ipl_registry[i, c("bowling_style")] = player_meta$bowling_style
#   print(paste(i, "out of", n_ipl_reg))
#   print(paste(player_meta$full_name, "- Batting Style:", player_meta$batting_style))
#   print(paste(player_meta$full_name, "- Bowling Style:", player_meta$bowling_style))
#   Sys.sleep(2)
# }
# 
# write.csv(ipl_registry, file = "data/ipl_registry_v2.csv", row.names = FALSE, na = "")


## Adding identifiers, and batting/bowling styles to all_matches.csv and create a new version for that
all_matches = read.csv("data/all_matches.csv")
all_matches = add_column(all_matches, striker_id = NA, .after = "bowling_team")
all_matches = add_column(all_matches, striker_style = NA, .after = "striker")
all_matches = add_column(all_matches, non_striker_id = NA, .before = "non_striker")
all_matches = add_column(all_matches, non_striker_style = NA, .after = "non_striker")
all_matches = add_column(all_matches, bowler_id = NA, .before = "bowler")
all_matches = add_column(all_matches, bowler_style = NA, .after = "bowler")
head(all_matches)

n_all_matches = dim(all_matches)[1]
ipl_registry = read.csv("data/ipl_registry_v2.csv")

for(i in 1:n_all_matches) {
  row = all_matches[i, ]
  all_matches[i, "striker_id"] = ipl_registry[ipl_registry$name_x == row$striker, ]$identifier
  all_matches[i, "striker_style"] = ipl_registry[ipl_registry$name_x == row$striker, ]$batting_style
  all_matches[i, "non_striker_id"] = ipl_registry[ipl_registry$name_x == row$non_striker, ]$identifier
  all_matches[i, "non_striker_style"] = ipl_registry[ipl_registry$name_x == row$non_striker, ]$batting_style
  all_matches[i, "bowler_id"] = ipl_registry[ipl_registry$name_x == row$bowler, ]$identifier
  all_matches[i, "bowler_style"] = ipl_registry[ipl_registry$name_x == row$bowler, ]$bowling_style
  print(i)
}

head(all_matches)

# Checking whether all required fields have been populated as expected
dim(all_matches[is.na(all_matches$striker_id) | all_matches$striker_id == "", ])
dim(all_matches[is.na(all_matches$striker_style) | all_matches$striker_style == "", ])
dim(all_matches[is.na(all_matches$non_striker_id) | all_matches$non_striker_id == "", ])
dim(all_matches[is.na(all_matches$non_striker_style) | all_matches$non_striker_style == "", ])
dim(all_matches[is.na(all_matches$bowler_id) | all_matches$bowler_id == "", ])
dim(all_matches[is.na(all_matches$bowler_style) | all_matches$bowler_style == "", ])

# There are  7 rows with missing bowler_style
all_matches[is.na(all_matches$bowler_style) | all_matches$bowler_style == "", ]

# Bowling style of "BJ Rohrer" is missing from cricinfo. This has to be populated by looking at the specific commentary record for the match
# https://www.espncricinfo.com/series/indian-premier-league-2013-586733/rajasthan-royals-vs-delhi-daredevils-52nd-match-598049/ball-by-ball-commentary
# Unfortunately, the commentary record does not have much useful information about the bowling style of BJ Rohrer
# But it looks like it's Right Arm Medium, by considering the 11.3 record, and this can be further confirmed by looking at game highlights
# https://www.crichighlightsvidz.com/2020/12/rr-vs-dd-52nd-match-ipl-2013-highlights.html
# Therefore, BJ Rohrer's bowling style will be considered as "Right arm Medium"

all_matches[all_matches$bowler_id == "27af6414", "bowler_style"] = "Right arm Medium"
all_matches[all_matches$bowler_style == "Right arm Medium", ]

# Saving it as v1.1 - If not, just go ahead without saving
# write.csv(all_matches, file = "data/all_matches_v1.1.csv", row.names = FALSE, na = "")

## Setting the primary bowler action for some bowlers (some bowlers have multiple bowling styles)

unique(all_matches$striker_style)
sort(unique(all_matches$bowler_style))

unique(all_matches[all_matches$bowler_style == "Legbreak Googly", "bowler"])

# "Legbreak" and "Legbreak Googly" mean the same thing - Let's call all of them "Legbreak"
all_matches[all_matches$bowler_style == "Legbreak Googly", "bowler_style"] = "Legbreak"

unique(all_matches[all_matches$bowler_style == "Right arm Medium, Right arm Offbreak", "bowler"])
# "A Symonds"      "DJ Thornely"    "SA Yadav"       "N Rana"         "Shashank Singh"
# "Right arm Medium, Right arm Offbreak" - This is a case where a bowler can have multiple styles.
# It can be safely assumed that they use their primary style (first style) when bowling
# "Right arm Medium, Right arm Offbreak" = "Right arm Medium"
all_matches[all_matches$bowler_style == "Right arm Medium, Right arm Offbreak", "bowler_style"] = "Right arm Medium"
unique(all_matches[all_matches$bowler_style == "Right arm Medium, Right arm Offbreak", "bowler"])

sort(unique(all_matches$bowler_style))

unique(all_matches[all_matches$bowler_style == "Right arm Offbreak, Legbreak", "bowler"])
# LS Livingstone and A Dananjaya usually bowl "Right arm Offbreak". 
all_matches[all_matches$bowler_style == "Right arm Offbreak, Legbreak", "bowler_style"] = "Right arm Offbreak"
unique(all_matches[all_matches$bowler_style == "Right arm Offbreak, Legbreak", "bowler"])


sort(unique(all_matches$bowler_style))

unique(all_matches[all_matches$bowler_style == "Right arm Offbreak, Legbreak Googly", "bowler"])
# "SR Tendulkar"   "Jalaj S Saxena" "SS Iyer" usually bowl "Right arm Offbreak".
# SR Tendulkar usually bowls Legbreak
# Jalaj S Saxena bowls offbreak primarily
# SS Iyer has bowled only one over in the history of IPL. It can be assumed that he bowls offbreak primarily

all_matches[all_matches$bowler == "SR Tendulkar", "bowler_style"]  = "Legbreak"
all_matches[all_matches$bowler == "Jalaj S Saxena", "bowler_style"] = "Right arm Offbreak"
all_matches[all_matches$bowler == "SS Iyer", "bowler_style"]  = "Right arm Offbreak"

sort(unique(all_matches$bowler_style))

unique(all_matches[all_matches$bowler_style == "Slow Left arm Orthodox, Left arm Wrist spin", "bowler"])
# K Kartikeya primarily bowls "Slow Left arm Orthodox"
all_matches[all_matches$bowler == "K Kartikeya", "bowler_style"]  = "Slow Left arm Orthodox"

sort(unique(all_matches$bowler_style))

table(all_matches$bowler_style)

# Saving the result as version 1.2 (all_matches_v1.2.csv) - If not, go ahead
write.csv(all_matches, file = "data/all_matches_v1.2.csv", row.names = FALSE, na = "")

# Between "data/all_matches_v1.2.csv" and "data/all_matches_v1.3.csv", I had to use Python 
# for manipulating JSON-based data since it's easier for me to work with JSON-data in Python
# I have added the Python code below as comments, for documenting purposes
# This will fetch the target from the JSON-based datafile for each match, and write the correct target for chasing team

# ======= Start of Python Code ======= #

# import os
# import pandas as pd
# import numpy as np
# import matplotlib.pyplot as plt
# import json
# import glob
# from matplotlib.ticker import StrMethodFormatter
# from pathlib import Path
# 
# parent_dir = os.path.dirname(os.getcwd())
# raw_data_dir = os.path.join(parent_dir, "ipl_male_csv2")
# json_dir = os.path.join(parent_dir, "ipl_json")
# all_matches_path = os.path.join(parent_dir, "data/all_matches_v1.2.csv")
# player_registry = None
# files = glob.glob(os.path.join(json_dir, "*.json"))
# 
# files = glob.glob(os.path.join(json_dir, "*.json"))
# 
# all_matches = pd.read_csv(all_matches_path, low_memory=False)
# all_matches.insert(4, 'match_overs', 20)
# all_matches.insert(5, 'match_result_method', np.NaN)
# all_matches.insert(6, 'target_runs', 0)
# all_matches.insert(6, 'target_overs', 20)
# all_matches.insert(7, 'target_balls', 120)
# 
# for file in files:
#   with open(file) as json_file:
#   match_info = json.load(json_file)
#   match_overs = match_info['info']['overs']
#   
#   innings_list = match_info['innings']
#   second_innings = innings_list[1] if 1 < len(innings_list) else None 
#   
#   target_info = second_innings.get('target') if second_innings != None else None
#   
#   target_overs = target_info.get('overs') if target_info != None else None
#   split_array = None if target_overs == None else str(target_overs).split('.')
#   ovs = None if split_array == None else split_array[0]
#   dels = None if split_array == None else split_array[1] if 1 < len(split_array) else 0
#   target_balls = None if split_array == None else int(ovs)*6 + int(dels)
#   
#   target_runs = target_info.get('runs') if target_info != None else None
#   method = match_info['info']['outcome'].get("method")
#   match_id = int(os.path.basename(os.path.splitext(file)[0]))
#   
#   all_matches.loc[(all_matches['innings'] == 2) & (all_matches['match_id'] == match_id), "target_runs"] = target_runs
#   all_matches.loc[(all_matches['innings'] == 2) & (all_matches['match_id'] == match_id), "target_overs"] = target_overs
#   all_matches.loc[(all_matches['innings'] == 2) & (all_matches['match_id'] == match_id), "target_balls"] = target_balls
#   all_matches.loc[all_matches['match_id'] == match_id, "match_result_method"] = method
#   all_matches.loc[all_matches['match_id'] == match_id, "match_overs"] = match_overs
#   print(file)
# 
# all_matches.to_csv("data/all_matches_v1.3.csv", index = False)

# ======= End of Python Code ======= #



###############################################
## Feature Creation using the available data ##
###############################################

df = read.csv("data/all_matches_v1.3.csv")

# Remove results related to super overs
df = subset(df, !(innings == 3 | innings == 4 | innings == 5 | innings == 6))

df = add_column(df, innings_id = paste0(df$match_id, "-", df$innings), .after = "innings")
df = add_column(df, innings_score = ave(x = (df$runs_off_bat + df$extras), df$innings_id, FUN = cumsum), .after = "bowler_style")
df = add_column(df, is_wicket = ifelse(df$wicket_type == "" | df$wicket_type == "retired hurt", 0, 1), .after = "wicket_type")
df = add_column(df, innings_wickets = ave(x = df$is_wicket, df$innings_id, FUN = cumsum), .after = "innings_score")
df = add_column(df, is_legal_ball = ifelse( !is.na(df$wides) | !is.na(df$noballs), 0, 1), .after = "innings_wickets")
df = add_column(df, innings_legal_balls = ave(x = df$is_legal_ball, df$innings_id, FUN = cumsum), .after = "is_legal_ball")
df = add_column(df, innings_run_rate = (df$innings_score / df$innings_legal_balls) * 6, .after = "innings_legal_balls")

# Grouping and Getting the "PRED" values which can be used for model training
# PREDS for Innings Score / Innings Wickets / Innings Run Rate
df = data.frame(df %>% group_by(innings_id) %>% mutate(innings_score_pred = lag(innings_score)))
df = data.frame(df %>% group_by(innings_id) %>% mutate(innings_wickets_pred = lag(innings_wickets)))
df = data.frame(df %>% group_by(innings_id) %>% mutate(innings_run_rate_pred = lag(innings_run_rate)))
df = data.frame(df %>% group_by(innings_id) %>% mutate(innings_legal_balls_pred = lag(innings_legal_balls)))
# Making NA values ZERO
df$innings_score_pred[is.na(df$innings_score_pred)] = 0
df$innings_wickets_pred[is.na(df$innings_wickets_pred)] = 0
df$innings_run_rate_pred[is.na(df$innings_run_rate_pred)] = 0
df$innings_run_rate_pred[is.infinite(df$innings_run_rate_pred)] = max(df$innings_run_rate_pred[is.finite(df$innings_run_rate_pred)])
df$innings_legal_balls_pred[is.na(df$innings_legal_balls_pred)] = 0

## Handling Striker's Stats against specific bowler styles
# Cumulative Score
df = data.frame(df %>% group_by(striker_id, bowler_style) %>% arrange(start_date) %>% mutate(striker_cum_score_bowler_style = cumsum(runs_off_bat)))
df = data.frame(df %>% group_by(striker_id, bowler_style) %>% arrange(start_date) %>% mutate(striker_cum_score_bowler_style_pred = lag(striker_cum_score_bowler_style)))
df$striker_cum_score_bowler_style_pred[is.na(df$striker_cum_score_bowler_style_pred)] = 0

# Cumulative Balls Faced
df = data.frame(df %>% group_by(striker_id, bowler_style) %>% arrange(start_date) %>% mutate(striker_cum_balls_bowler_style = cumsum(ifelse(is.na(wides), 1, 0)) ))
df = data.frame(df %>% group_by(striker_id, bowler_style) %>% arrange(start_date) %>% mutate(striker_cum_balls_bowler_style_pred = lag(striker_cum_balls_bowler_style)))
df$striker_cum_balls_bowler_style_pred[is.na(df$striker_cum_balls_bowler_style_pred)] = 0

# Cumulative Dismissals
df = data.frame(df %>% group_by(striker_id, bowler_style) %>% arrange(start_date) %>% mutate(striker_cum_wickets_bowler_style = cumsum( ifelse((striker == player_dismissed & wicket_type != "retired hurt"), 1, 0) ) ))
df = data.frame(df %>% group_by(striker_id, bowler_style) %>% arrange(start_date) %>% mutate(striker_cum_wickets_bowler_style_pred = lag(striker_cum_wickets_bowler_style)))
df$striker_cum_wickets_bowler_style_pred[is.na(df$striker_cum_wickets_bowler_style_pred)] = 0

# AVG/SR Calculation (Getting PRED directly)
df = add_column(df, striker_avg_bowler_style_pred = (df$striker_cum_score_bowler_style_pred / df$striker_cum_wickets_bowler_style_pred), .after = "striker_cum_wickets_bowler_style_pred")
df = add_column(df, striker_sr_bowler_style_pred = (df$striker_cum_score_bowler_style_pred / df$striker_cum_balls_bowler_style_pred) * 100, .after = "striker_avg_bowler_style_pred")
df$striker_avg_bowler_style_pred[is.na(df$striker_avg_bowler_style_pred) | is.infinite(df$striker_avg_bowler_style_pred)] = max(df$striker_avg_bowler_style_pred[is.finite(df$striker_avg_bowler_style_pred)])
df$striker_sr_bowler_style_pred[is.na(df$striker_sr_bowler_style_pred) | is.infinite(df$striker_sr_bowler_style_pred)] = 0

## Handling CAREER Cumulative Scores/Cumulative Balls and Cumulative AVG/SR

# Cumulative Score / Cumulative Balls
df = add_column(df, striker_cum_score = ave(x = df$runs_off_bat, df$striker_id, FUN = cumsum), .after = "innings_run_rate")
df = add_column(df, striker_cum_balls = ave(x = ifelse(is.na(df$wides), 1, 0), df$striker_id, FUN = cumsum), .after = "striker_cum_score")

# Cumulative Dismissals
df = add_column(df, striker_cum_dismissals = ave(x = ifelse((df$striker == df$player_dismissed & df$wicket_type != "retired hurt"), 1, 0), df$striker_id, FUN = cumsum), .after = "striker_cum_balls")

# Fixing Special Runout Cases (Non-Striker Getting Runout)
special_runouts = df[df$non_striker == df$player_dismissed, ]
n_special_runouts = dim(special_runouts)[1]
n_special_runouts
n_df = dim(df)[1]

for(i in 1:n_special_runouts) {
  df[((df$start_date) > (special_runouts[i, ]$start_date)) & df$striker == special_runouts[i, ]$player_dismissed,  "striker_cum_dismissals"] = df[((df$start_date) > (special_runouts[i, ]$start_date)) & df$striker == special_runouts[i, ]$player_dismissed,  "striker_cum_dismissals"] + 1
  
  print(paste(i,"out of", n_special_runouts))
}

df = add_column(df, striker_cum_avg = (df$striker_cum_score / df$striker_cum_dismissals), .after = "striker_cum_dismissals")
df = add_column(df, striker_cum_sr = (df$striker_cum_score / df$striker_cum_balls) * 100, .after = "striker_cum_avg")

df = data.frame(df %>% group_by(striker_id) %>% arrange(start_date) %>% mutate(striker_cum_avg_pred = lag(striker_cum_avg)))
df = data.frame(df %>% relocate(striker_cum_avg_pred, .after = "striker_cum_avg"))

# striker_cum_avg contains Inf, NA, NaN values. In cricket, this occurrs when a batsman has not been dismissed yet
# Technically, the average isn't either undefined or the maximum possible value
# Therefore, let's impute them using the maximum available value from that column
df$striker_cum_avg_pred[is.na(df$striker_cum_avg_pred) | is.infinite(df$striker_cum_avg_pred)] = max(df$striker_cum_avg_pred[is.finite(df$striker_cum_avg_pred)])

df = data.frame(df %>% group_by(striker_id) %>% arrange(start_date) %>% mutate(striker_cum_sr_pred = lag(striker_cum_sr)))
df = data.frame(df %>% relocate(striker_cum_sr_pred, .after = "striker_cum_sr"))
df$striker_cum_sr_pred[is.na(df$striker_cum_sr_pred)] = 0

## Striker's FOUR/SIX Rate
# Fours
df = add_column(df, striker_cum_fours = ave(x = ifelse(df$runs_off_bat == 4, 1, 0), df$striker_id, FUN = cumsum))
df = data.frame(df %>% group_by(striker_id) %>% arrange(start_date) %>% mutate(striker_cum_fours_pred = lag(striker_cum_fours)))
df = add_column(df, striker_cum_four_rate_pred = (df$striker_cum_fours_pred / df$striker_cum_balls), .after = "striker_cum_fours_pred")
df$striker_cum_four_rate_pred[is.na(df$striker_cum_four_rate_pred)] = 0

# Sixes
df = add_column(df, striker_cum_sixes = ave(x = ifelse(df$runs_off_bat == 6, 1, 0), df$striker_id, FUN = cumsum))
df = data.frame(df %>% group_by(striker_id) %>% arrange(start_date) %>% mutate(striker_cum_sixes_pred = lag(striker_cum_sixes)))
df = add_column(df, striker_cum_sixes_rate_pred = (df$striker_cum_sixes_pred / df$striker_cum_balls), .after = "striker_cum_sixes_pred")
df$striker_cum_sixes_rate_pred[is.na(df$striker_cum_sixes_rate_pred)] = 0

## Last Ball Score
df = data.frame(df %>% group_by(innings_id) %>% mutate(last_ball_score = lag(runs_off_bat + extras)))
df$last_ball_score[is.na(df$last_ball_score)] = 0

## Batter's Current Score / SR
df = data.frame(df %>% group_by(innings_id, striker_id) %>% arrange(start_date) %>% mutate(striker_current_score = cumsum(runs_off_bat)))
df = data.frame(df %>% group_by(innings_id, striker_id) %>% arrange(start_date) %>% mutate(striker_current_score_pred = lag(striker_current_score)))
df$striker_current_score_pred[is.na(df$striker_current_score_pred)] = 0

df = data.frame(df %>% group_by(innings_id, striker_id) %>% arrange(start_date) %>% mutate(striker_current_balls = cumsum(ifelse(is.na(wides), 1, 0)) ))
df = data.frame(df %>% group_by(innings_id, striker_id) %>% arrange(start_date) %>% mutate(striker_current_balls_pred = lag(striker_current_balls)))

df = add_column(df, striker_current_sr_pred = (df$striker_current_score_pred / df$striker_current_balls_pred) * 100, .after = "striker_current_balls_pred")
df$striker_current_sr_pred[is.na(df$striker_current_sr_pred)] = 0

## Balls since last boundary (4 or 6)
df$is_not_boundary = ifelse(df$runs_off_bat == 4 | df$runs_off_bat == 6, 0, 1)
df = data.frame(df %>% group_by(innings_id, cumsum( is_not_boundary == 0 )) %>% arrange(start_date) %>% mutate(balls_without_boundary = cumsum(is_not_boundary) ))
df = data.frame(df %>% group_by(innings_id) %>% mutate(balls_without_boundary_pred = lag(balls_without_boundary)))
df$balls_without_boundary_pred[is.na(df$balls_without_boundary_pred)] = 0


# Computing the Required Run Rate (RRR)
df = df %>% mutate(rrr_pred = if_else(innings == 1, 0, ( (target_runs - innings_score_pred) * 6 / (target_balls - innings_legal_balls_pred) ) ))


## Handling Bowler's Stats against specific Batsman styles
# Cumulative Score Conceded
df = data.frame(df %>% group_by(bowler_id, striker_style) %>% arrange(start_date) %>% mutate(bowler_cum_score_striker_style = cumsum(runs_off_bat + ifelse(is.na(wides), 0, wides) + ifelse(is.na(noballs), 0, noballs) ) ))

df = data.frame(df %>% group_by(bowler_id, striker_style) %>% arrange(start_date) %>% mutate(bowler_cum_score_striker_style_pred = lag(bowler_cum_score_striker_style)))
df$bowler_cum_score_striker_style_pred[is.na(df$bowler_cum_score_striker_style_pred)] = 0

# Cumulative Balls Bowled
df = data.frame(df %>% group_by(bowler_id, striker_style) %>% arrange(start_date) %>% mutate(bowler_cum_balls_striker_style = cumsum(ifelse(is.na(wides) & is.na(noballs), 1, 0)) ))
df = data.frame(df %>% group_by(bowler_id, striker_style) %>% arrange(start_date) %>% mutate(bowler_cum_balls_striker_style_pred = lag(bowler_cum_balls_striker_style)))
df$bowler_cum_balls_striker_style_pred[is.na(df$bowler_cum_balls_striker_style_pred)] = 0

# Cumulative Dismissals
df = data.frame(df %>% group_by(bowler_id, striker_style) %>% arrange(start_date) %>% mutate(bowler_cum_wickets_striker_style = cumsum( ifelse((striker == player_dismissed & wicket_type != "retired hurt"), 1, 0) ) ))
df = data.frame(df %>% group_by(bowler_id, striker_style) %>% arrange(start_date) %>% mutate(bowler_cum_wickets_striker_style_pred = lag(bowler_cum_wickets_striker_style)))
df$bowler_cum_wickets_striker_style_pred[is.na(df$bowler_cum_wickets_striker_style_pred)] = 0

# AVG/SR Calculation (Getting PRED directly)
df = add_column(df, bowler_avg_striker_style_pred = (df$bowler_cum_score_striker_style_pred / df$bowler_cum_wickets_striker_style_pred), .after = "bowler_cum_wickets_striker_style_pred")
df = add_column(df, bowler_sr_striker_style_pred = (df$bowler_cum_balls_striker_style_pred / df$bowler_cum_wickets_striker_style_pred), .after = "bowler_avg_striker_style_pred")
df$bowler_avg_striker_style_pred[is.na(df$bowler_avg_striker_style_pred) | is.infinite(df$bowler_avg_striker_style_pred)] = max(df$bowler_avg_striker_style_pred[is.finite(df$bowler_avg_striker_style_pred)])

df$bowler_sr_striker_style_pred[is.na(df$bowler_sr_striker_style_pred) | is.infinite(df$bowler_sr_striker_style_pred)] = 0



## Handling CAREER Cumulative Scores Conceded / Cumulative Balls Bowled and Cumulative AVG/SR

# Cumulative Runs Conceded / Cumulative Balls Bowled
df = data.frame(df %>% group_by(bowler_id) %>% arrange(start_date) %>% mutate(bowler_cum_score = cumsum(runs_off_bat + ifelse(is.na(wides), 0, wides) + ifelse(is.na(noballs), 0, noballs) ) ))
df = data.frame(df %>% group_by(bowler_id) %>% arrange(start_date) %>% mutate(bowler_cum_balls = cumsum(ifelse(is.na(wides) & is.na(noballs), 1, 0)) ))


# Cumulative Dismissals
df = add_column(df, bowler_cum_dismissals = ave(x = ifelse((df$striker == df$player_dismissed & df$wicket_type != "retired hurt"), 1, 0), df$bowler_id, FUN = cumsum))

df = add_column(df, bowler_cum_avg = (df$bowler_cum_score / df$bowler_cum_dismissals))
df = add_column(df, bowler_cum_sr = (df$bowler_cum_balls / df$bowler_cum_dismissals), .after = "striker_cum_avg")

df = data.frame(df %>% group_by(striker_id) %>% arrange(start_date) %>% mutate(bowler_cum_avg_pred = lag(bowler_cum_avg)))

# bowler_cum_avg_pred contains Inf, NA, NaN values. In cricket, this occurs when a batsman has not been dismissed yet
# Technically, the average isn't either undefined or the maximum possible value
# Therefore, let's impute them using the maximum available value from that column
df$bowler_cum_avg_pred[is.na(df$bowler_cum_avg_pred) | is.infinite(df$bowler_cum_avg_pred)] = max(df$bowler_cum_avg_pred[is.finite(df$bowler_cum_avg_pred)]) 

df = data.frame(df %>% group_by(bowler_id) %>% arrange(start_date) %>% mutate(bowler_cum_sr_pred = lag(bowler_cum_sr)))
df$bowler_cum_sr_pred[is.na(df$bowler_cum_sr_pred)] = 0
df$bowler_cum_sr_pred[is.infinite(df$bowler_cum_sr_pred)] = max(df$bowler_cum_sr_pred[is.finite(df$bowler_cum_sr_pred)]) 

# Defining Outcome Column
get_outcome_column = function(row) {
  runs_off_bat = row["runs_off_bat"]
  extras = row["extras"]
  total_runs = as.numeric(runs_off_bat) + as.numeric(extras)
  wicket_type = row["wicket_type"]
  
  if (is.na(wicket_type) | wicket_type == "") {
    return(toString(runs_off_bat))
  }
  
  if (wicket_type == "retired hurt") {
    return(toString(runs_off_bat))
  } 
  
  if (!is.na(wicket_type) | wicket_type != "") {
    return("Wicket")
  }
  
  print(row["start_date"])
  return(NA)
}

df$outcome = apply(df, 1, FUN = get_outcome_column)

colnames(df)

# [1] "match_id"                              "season"                                "start_date"                            "venue"                                
# [5] "innings"                               "innings_id"                            "ball"                                  "batting_team"                         
# [9] "bowling_team"                          "striker_id"                            "striker"                               "striker_style"                        
# [13] "non_striker_id"                        "non_striker"                           "non_striker_style"                     "bowler_id"                            
# [17] "bowler"                                "bowler_style"                          "innings_score"                         "innings_wickets"                      
# [21] "is_legal_ball"                         "innings_legal_balls"                   "innings_run_rate"                      "striker_cum_score"                    
# [25] "striker_cum_balls"                     "striker_cum_dismissals"                "striker_cum_avg"                       "bowler_cum_sr"                        
# [29] "striker_cum_avg_pred"                  "striker_cum_sr"                        "striker_cum_sr_pred"                   "runs_off_bat"                         
# [33] "extras"                                "wides"                                 "noballs"                               "byes"                                 
# [37] "legbyes"                               "penalty"                               "wicket_type"                           "is_wicket"                            
# [41] "player_dismissed"                      "other_wicket_type"                     "other_player_dismissed"                "outcome"                              
# [45] "innings_score_pred"                    "innings_wickets_pred"                  "innings_run_rate_pred"                 "innings_legal_balls_pred"             
# [49] "striker_cum_score_bowler_style"        "striker_cum_score_bowler_style_pred"   "striker_cum_balls_bowler_style"        "striker_cum_balls_bowler_style_pred"  
# [53] "striker_cum_wickets_bowler_style"      "striker_cum_wickets_bowler_style_pred" "striker_avg_bowler_style_pred"         "striker_sr_bowler_style_pred"         
# [57] "striker_cum_fours"                     "striker_cum_fours_pred"                "striker_cum_four_rate_pred"            "striker_cum_sixes"                    
# [61] "striker_cum_sixes_pred"                "striker_cum_sixes_rate_pred"           "last_ball_score"                       "striker_current_score"                
# [65] "striker_current_score_pred"            "striker_current_balls"                 "striker_current_balls_pred"            "striker_current_sr_pred"              
# [69] "is_not_boundary"                       "cumsum.is_not_boundary....0."          "balls_without_boundary"                "balls_without_boundary_pred"          
# [73] "first_innings_score"                   "rrr_pred"                              "bowler_cum_score_striker_style"        "bowler_cum_score_striker_style_pred"  
# [77] "bowler_cum_balls_striker_style"        "bowler_cum_balls_striker_style_pred"   "bowler_cum_wickets_striker_style"      "bowler_cum_wickets_striker_style_pred"
# [81] "bowler_avg_striker_style_pred"         "bowler_sr_striker_style_pred"          "bowler_cum_score"                      "bowler_cum_balls"                     
# [85] "bowler_cum_dismissals"                 "bowler_cum_avg"                        "bowler_cum_avg_pred"                   "bowler_cum_sr_pred" 


# Columns of Interest
interested_columns = c("match_id", "season", "start_date", "match_result_method", "innings", "innings_id", "ball", "striker_id", "striker", "striker_style",
                       "non_striker_id", "non_striker", "non_striker_style", "bowler_id", "bowler", "bowler_style",
                       "runs_off_bat", "extras", "wicket_type", "player_dismissed", "innings_score", "innings_wickets", 
                       "innings_score_pred", "innings_wickets_pred", "innings_legal_balls_pred", "innings_run_rate_pred", "striker_cum_avg_pred",
                       "striker_cum_sr_pred", "striker_avg_bowler_style_pred", "striker_sr_bowler_style_pred", "striker_cum_four_rate_pred",
                       "striker_cum_sixes_rate_pred", "last_ball_score", "striker_current_score_pred", "striker_current_sr_pred", 
                       "balls_without_boundary_pred", "target_runs", "target_balls", "rrr_pred", "bowler_avg_striker_style_pred", "bowler_sr_striker_style_pred", 
                       "bowler_cum_avg_pred", "bowler_cum_sr_pred", 
                       "outcome")

# Filtering only the interested columns
df = df[, interested_columns]

# 3 and 5 are RARE cases. Therefore, we will get rid of them.
df = df[!(df$outcome == 5 | df$outcome == 3), ]

# Viewing the Current Distribution (as a percentage)
round(table(df$outcome) / nrow(df), 4)*100

# Finding the number of available observations: #225024
nrow(df)


## Performing Stratified Sampling to Split the dataset (using createDataPartition function of caret package)
## Train (80%) : Validation (10%) : Test (10%)

# First, the dataset will be split into Train-Validation (90%) : Test (10%)
set.seed(1500)
testing_index = createDataPartition(df$outcome, p = 0.1, list = FALSE)
testing_df = df[testing_index, ]
train_val_df = df[-testing_index, ]

# Number of observations in Testing dataset: 22506
dim(testing_df)

# Viewing the distribution of Testing Dataset (as a percentage)
# This is almost equal to the distribution of the original dataset
round(table(testing_df$outcome) / nrow(testing_df), 4)*100


# We should try to get approximately 22506 observations for the validation dataset as well
train_val_split_percentage = nrow(testing_df) / nrow(train_val_df)

# Splitting the Train-Validation dataset into Train : Validation
validation_index = createDataPartition(train_val_df$outcome, p = train_val_split_percentage, list = FALSE)
training_df = train_val_df[-validation_index, ]
validation_df = train_val_df[validation_index, ]  

dim(training_df)
round(table(training_df$outcome) / nrow(training_df), 4)*100

dim(validation_df)
round(table(validation_df$outcome) / nrow(validation_df), 4)*100


# Saving the datasets
write.csv(training_df, "data/cricket_training.csv", row.names = FALSE, na = "")
write.csv(validation_df, "data/cricket_validation.csv", row.names = FALSE, na = "")
write.csv(testing_df, "data/cricket_testing.csv", row.names = FALSE, na = "")