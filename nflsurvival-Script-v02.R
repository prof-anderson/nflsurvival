# ==============================================================================
# NFL Survivor Simulation (Corrected)
# Requires: nflseedr, nflreadr, dplyr, tidyr
# Problem:  Uses actual season results instead of simulated results
#     Could be useful though later.
# ==============================================================================

# Ensure libraries are loaded
library(nflseedR)
library(nflreadr)
library(dplyr)
library(tidyr)

# ==============================================================================
# 1. SETUP & DATA LOADING
# ==============================================================================

#' Load Season Data
#' Fetches historical schedule and results for a specific season.
get_season_data <- function(target_season) {
  # We use dplyr::select and dplyr::filter to avoid conflicts with other packages
  nflreadr::load_schedules(seasons = target_season) %>%
    dplyr::filter(game_type == "REG") %>% # Regular season only
    dplyr::select(game_id, week, season, home_team, away_team, 
                  home_score, away_score, result, spread_line)
}

# ==============================================================================
# 2. HEURISTIC (STRATEGY)
# ==============================================================================

#' Default Heuristic: "Safe Betting"
#' Picks the biggest favorite available.
#' CORRECTION: In betting, a negative spread (e.g., -10) means the team is favored.
#' We want the team with the LOWEST spread value.
#' 
#' @param week_games Dataframe of games for the current week.
#' @param used_teams Vector of team abbreviations already picked.
#' @return A list containing: list(team = "TEAM_ABBR", reason = "Explanation")
heuristic_biggest_favorite <- function(week_games, used_teams) {
  
  # 1. Identify all potential picks (Home and Away)
  # home_spread > 0 means Home is Underdog.
  # home_spread < 0 means Home is Favorite.
  
  options <- dplyr::bind_rows(
    week_games %>% 
      dplyr::transmute(team = home_team, 
                       opponent = away_team,
                       # Lower spread = Better favorite (e.g. -14 is better than -3)
                       spread = spread_line), 
    week_games %>% 
      dplyr::transmute(team = away_team, 
                       opponent = home_team,
                       # If Home is -7, Away is +7. 
                       spread = -spread_line)
  )
  
  # 2. Filter out teams already used and NA spreads
  available_options <- options %>%
    dplyr::filter(!team %in% used_teams) %>%
    dplyr::filter(!is.na(spread)) # Safety check for missing data
  
  # 3. Apply Logic: Pick the LOWEST spread (biggest favorite)
  if(nrow(available_options) == 0) {
    return(NULL) # No valid moves left
  }
  
  pick <- available_options %>%
    dplyr::arrange(spread) %>% # Ascending sort (Most negative first)
    dplyr::slice(1)
  
  return(list(team = pick$team, reason = paste("Spread:", pick$spread)))
}

# ==============================================================================
# 3. SIMULATION LOGIC
# ==============================================================================

#' Simulate Survivor Season
simulate_survivor_season <- function(season_year, heuristic_fn) {
  
  message(paste("Loading data for season:", season_year))
  schedule <- get_season_data(season_year)
  
  if(nrow(schedule) == 0) {
    stop("No data found for this year. Please check the season year.")
  }
  
  # Initialize state
  used_teams <- c()
  results_log <- data.frame()
  alive <- TRUE
  current_week <- 1
  max_weeks <- max(schedule$week, na.rm = TRUE)
  
  while(alive && current_week <= max_weeks) {
    
    # Get games for this week
    week_games <- schedule %>% dplyr::filter(week == current_week)
    
    # Skip empty weeks (e.g., if data is missing for a week)
    if(nrow(week_games) == 0) {
      current_week <- current_week + 1
      next
    }
    
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
      dplyr::filter(home_team == picked_team | away_team == picked_team)
    
    # Error Handling: Ensure game exists and result is not NA
    if (nrow(game) != 1) {
      message("Error: Could not locate unique game for pick.")
      alive <- FALSE
      break
    }
    
    if (is.na(game$result)) {
      message(paste("Week", current_week, ": Result is missing (NA). Assuming game not played/Loss."))
      alive <- FALSE
      break
    }
    
    # Logic: result = home_score - away_score
    outcome <- "LOSS"
    score_summary <- paste0(game$home_team, " ", game$home_score, " - ", game$away_team, " ", game$away_score)
    
    if (picked_team == game$home_team && game$result > 0) {
      outcome <- "WIN"
    } else if (picked_team == game$away_team && game$result < 0) {
      outcome <- "WIN"
    }
    
    # Log it
    # Use rbind to avoid dplyr bind_rows type issues in loops if init is empty
    results_log <- rbind(results_log, data.frame(
      Week = current_week,
      Pick = picked_team,
      Outcome = outcome,
      Score = score_summary,
      Reason = reason,
      stringsAsFactors = FALSE
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
    status = if(alive) "Perfect Season (or Data Ended)" else "Eliminated"
  ))
}

# ==============================================================================
# 4. EXECUTION
# ==============================================================================

# Run the simulation for the 2023 season
# Try changing this to 2015, 2020, etc.
sim_result <- simulate_survivor_season(2023, heuristic_biggest_favorite)

# Print clean results
print(paste("Final Status:", sim_result$status))
print(sim_result$log)