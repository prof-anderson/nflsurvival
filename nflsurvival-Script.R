# Install packages if not already installed
# install.packages(c("nflseedr", "nflreadr", "dplyr", "tidyr"))

library(nflseedr)
library(nflreadr)
library(dplyr)
library(tidyr)

# ==============================================================================
# 1. SETUP & DATA LOADING
# ==============================================================================

#' Load Season Data
#' Fetches historical schedule and results for a specific season.
get_season_data <- function(target_season) {
  # nflreadr::load_schedules() fetches historical games with scores and betting lines
  nflreadr::load_schedules(seasons = target_season) %>%
    filter(game_type == "REG") %>% # Regular season only
    select(game_id, week, season, home_team, away_team, 
           home_score, away_score, result, spread_line)
}

# ==============================================================================
# 2. HEURISTIC (STRATEGY)
# ==============================================================================

#' Default Heuristic: "Safe Betting"
#' Picks the biggest favorite (based on spread) available that hasn't been used.
#' 
#' @param week_games Dataframe of games for the current week.
#' @param used_teams Vector of team abbreviations already picked.
#' @return A list containing: list(team = "TEAM_ABBR", reason = "Explanation")
heuristic_biggest_favorite <- function(week_games, used_teams) {
  
  # 1. Identify all potential picks (Home and Away)
  options <- bind_rows(
    week_games %>% 
      transmute(team = home_team, 
                opponent = away_team,
                # In nflreadr, positive spread_line usually means Home is favored 
                # (checks required depending on specific data version, but often spread_line is Home Adv)
                # Actually standard: spread_line is points Home is favored by. 
                # e.g., if spread_line is 7, Home is favored by 7.
                favorability = spread_line), 
    week_games %>% 
      transmute(team = away_team, 
                opponent = home_team,
                favorability = -spread_line)
  )
  
  # 2. Filter out teams already used
  available_options <- options %>%
    filter(!team %in% used_teams)
  
  # 3. Apply Logic: Pick the max favorability (biggest favorite)
  if(nrow(available_options) == 0) {
    return(NULL) # No valid moves left
  }
  
  pick <- available_options %>%
    arrange(desc(favorability)) %>%
    slice(1)
  
  return(list(team = pick$team, reason = paste("Favored by", round(pick$favorability, 1))))
}

# ==============================================================================
# 3. SIMULATION LOGIC
# ==============================================================================

#' Simulate Survivor Season
#' 
#' @param season_year Integer, the year to simulate (e.g., 2023).
#' @param heuristic_fn Function to select a team. Arguments: (week_games, used_teams).
simulate_survivor_season <- function(season_year, heuristic_fn) {
  
  # Get data
  message(paste("Loading data for season:", season_year))
  schedule <- get_season_data(season_year)
  
  # Initialize state
  used_teams <- c()
  results_log <- data.frame()
  alive <- TRUE
  current_week <- 1
  max_weeks <- max(schedule$week)
  
  while(alive && current_week <= max_weeks) {
    
    # Get games for this week
    week_games <- schedule %>% filter(week == current_week)
    
    # -------------------------------------------------------
    # CALL HEURISTIC
    # -------------------------------------------------------
    selection <- heuristic_fn(week_games, used_teams)
    
    if (is.null(selection)) {
      message(paste("Week", current_week, ": No available teams left to pick! Eliminated."))
      alive <- FALSE
      break
    }
    
    picked_team <- selection$team
    reason <- selection$reason
    
    # -------------------------------------------------------
    # DETERMINE RESULT
    # -------------------------------------------------------
    # Find the game the picked team played
    game <- week_games %>% 
      filter(home_team == picked_team | away_team == picked_team)
    
    # Check if they won
    # result = home_score - away_score
    # If pick is Home, need result > 0. If pick is Away, need result < 0.
    # Note: Ties (result == 0) are usually losses in Survivor.
    
    outcome <- "LOSS"
    score_summary <- paste0(game$home_team, " ", game$home_score, " - ", game$away_team, " ", game$away_score)
    
    if (picked_team == game$home_team && game$result > 0) {
      outcome <- "WIN"
    } else if (picked_team == game$away_team && game$result < 0) {
      outcome <- "WIN"
    }
    
    # Log it
    results_log <- bind_rows(results_log, data.frame(
      Week = current_week,
      Pick = picked_team,
      Outcome = outcome,
      Score = score_summary,
      Reason = reason
    ))
    
    # Update State
    if (outcome == "WIN") {
      used_teams <- c(used_teams, picked_team)
      current_week <- current_week + 1
    } else {
      alive <- FALSE
      message(paste("Week", current_week, ": Picked", picked_team, "and LOST."))
    }
  }
  
  # Final Output
  return(list(
    survived_weeks = sum(results_log$Outcome == "WIN"),
    log = results_log,
    status = if(alive) "Perfect Season!" else "Eliminated"
  ))
}

# ==============================================================================
# 4. EXECUTION
# ==============================================================================

# Example Usage: Simulate the 2023 Season
# You can easily change the year here
simulation_year <- 2023

# Run the simulation
sim_result <- simulate_survivor_season(simulation_year, heuristic_biggest_favorite)

# Print Results
print(paste("Result for", simulation_year, ":", sim_result$status))
print(sim_result$log)

# ------------------------------------------------------------------------------
# HOW TO REPLACE THE HEURISTIC
# ------------------------------------------------------------------------------
# Define a new function, e.g., "Pick Random Available Team"
heuristic_random <- function(week_games, used_teams) {
  # Get all teams playing this week
  all_teams <- c(week_games$home_team, week_games$away_team)
  available <- setdiff(all_teams, used_teams)
  
  if(length(available) == 0) return(NULL)
  
  pick <- sample(available, 1)
  return(list(team = pick, reason = "Random Choice"))
}

# Run with new heuristic
# sim_result_random <- simulate_survivor_season(simulation_year, heuristic_random)