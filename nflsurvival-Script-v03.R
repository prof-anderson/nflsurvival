# ==============================================================================
# NFL Survivor Simulation (Fixed & Tested)
# ==============================================================================

library(nflseedR)
library(nflreadr)
library(dplyr)
library(tidyr)

# ==============================================================================
# 1. SETUP & SIMULATION
# ==============================================================================

#' Generate a Simulated Season
#' Uses nflseedR to simulate one full season.
#' 
#' @param season_year Integer, the year to simulate.
#' @return A dataframe containing the simulated schedule with outcomes.
generate_simulated_reality <- function(season_year) {
  
  # 1. Load actual schedule
  # We keep ALL columns. nflseedR needs 'season', 'location', etc. to run.
  real_schedule <- nflreadr::load_schedules(seasons = season_year) %>%
    dplyr::filter(game_type == "REG")
  
  if(nrow(real_schedule) == 0) stop("No data found for this season.")
  
  # 2. Prepare data for nflseedR
  # We set 'result' to NA so the function knows to simulate these games.
  # We do NOT use select() here, to preserve all necessary columns for the engine.
  sim_input <- real_schedule %>%
    dplyr::mutate(
      result = NA_real_ # Clear actual results to force simulation
    )
  
  # 3. Run the Simulation
  message("Running nflseedR simulation... (Computing Elo and Outcomes)")
  
  # We capture the output to avoid printing the long list
  sim_output <- nflseedR::nfl_simulations(
    games = sim_input,
    simulations = 1,
    fresh_season = TRUE, # Simulate fresh without carrying over previous state history
    chunks = 1,
    exec = "sequential"
  )
  
  # 4. Extract Results
  # The output $games dataframe contains the filled-in 'result' column.
  # We select the columns we need to merge back.
  simulated_games <- sim_output$games %>%
    dplyr::select(week, away_team, home_team, sim_result = result)
  
  # 5. Merge Simulated Results back with original Betting Data
  # We join on week/teams to match the simulated score with the original spread lines
  final_data <- real_schedule %>%
    dplyr::left_join(simulated_games, by = c("week", "away_team", "home_team")) %>%
    dplyr::mutate(result = sim_result) # Overwrite the NA result with the simulated one
  
  return(final_data)
}

# ==============================================================================
# 2. HEURISTIC (STRATEGY)
# ==============================================================================

#' Default Heuristic: "Safe Betting"
#' Picks the biggest favorite available based on HISTORICAL spreads.
#' 
#' @param week_games Dataframe of games for the current week.
#' @param used_teams Vector of team abbreviations already picked.
heuristic_biggest_favorite <- function(week_games, used_teams) {
  
  # Identify potential picks
  options <- dplyr::bind_rows(
    week_games %>% 
      dplyr::transmute(team = home_team, 
                       opponent = away_team,
                       # In betting data: Negative spread = Favorite.
                       # We want the MOST negative number (e.g. -14 is better than -3).
                       spread = spread_line), 
    week_games %>% 
      dplyr::transmute(team = away_team, 
                       opponent = home_team,
                       # If Home is -7, Away is +7 (Underdog).
                       spread = -spread_line)
  )
  
  # Filter available teams and valid data
  available_options <- options %>%
    dplyr::filter(!team %in% used_teams) %>%
    dplyr::filter(!is.na(spread))
  
  if(nrow(available_options) == 0) return(NULL)
  
  # Pick the team with the lowest (most negative) spread
  pick <- available_options %>%
    dplyr::arrange(spread) %>% 
    dplyr::slice(1)
  
  return(list(team = pick$team, reason = paste("Spread:", pick$spread)))
}

# ==============================================================================
# 3. SURVIVOR LOOP
# ==============================================================================

run_survivor_game <- function(season_year, heuristic_fn) {
  
  # --- Step 1: Create the Simulated Reality ---
  schedule <- generate_simulated_reality(season_year)
  
  # --- Step 2: Play the Game ---
  used_teams <- c()
  results_log <- data.frame()
  alive <- TRUE
  current_week <- 1
  max_weeks <- max(schedule$week, na.rm = TRUE)
  
  while(alive && current_week <= max_weeks) {
    
    # Filter for current week
    week_games <- schedule %>% dplyr::filter(week == current_week)
    
    # Skip weeks with no games (e.g., if data gap)
    if(nrow(week_games) == 0) {
      current_week <- current_week + 1
      next
    }
    
    # Make Pick
    selection <- heuristic_fn(week_games, used_teams)
    
    if (is.null(selection)) {
      message(paste("Week", current_week, ": No teams left to pick! Eliminated."))
      alive <- FALSE
      break
    }
    
    picked_team <- selection$team
    
    # Determine Outcome based on SIMULATED result
    # We find the specific game record
    game <- week_games %>% 
      dplyr::filter(home_team == picked_team | away_team == picked_team)
    
    # Logic: result = home_score - away_score
    # result > 0 means Home Wins. result < 0 means Away Wins.
    outcome <- "LOSS"
    
    if (picked_team == game$home_team && game$result > 0) {
      outcome <- "WIN"
    } else if (picked_team == game$away_team && game$result < 0) {
      outcome <- "WIN"
    }
    
    # Log Result
    results_log <- rbind(results_log, data.frame(
      Week = current_week,
      Pick = picked_team,
      Outcome = outcome,
      Simulated_Margin = round(game$result, 1),
      Reason = selection$reason,
      stringsAsFactors = FALSE
    ))
    
    if (outcome == "WIN") {
      used_teams <- c(used_teams, picked_team)
      current_week <- current_week + 1
    } else {
      alive <- FALSE
      message(paste("Week", current_week, ": Picked", picked_team, "and LOST. (Simulated Result:", round(game$result, 1), ")"))
    }
  }
  
  return(list(
    status = if(alive) "Perfect Season!" else "Eliminated",
    weeks_survived = sum(results_log$Outcome == "WIN"),
    log = results_log
  ))
}

# ==============================================================================
# 4. EXECUTION
# ==============================================================================

# Simulation settings
target_year <- 2023

# Run
# Note: Because nfl_simulations is stochastic, results will vary every run!
final_result <- run_survivor_game(target_year, heuristic_biggest_favorite)

# Output
print(paste("Final Result:", final_result$status))
print(final_result$log)

final_result <- run_survivor_game(target_year, heuristic_biggest_favorite)

# Output
print(paste("Final Result:", final_result$status))
print(final_result$log)

final_result <- run_survivor_game(target_year, heuristic_biggest_favorite)

# Output
print(paste("Final Result:", final_result$status))
print(final_result$log)
