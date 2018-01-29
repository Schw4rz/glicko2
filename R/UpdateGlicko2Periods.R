#' Update ratings for a given number of periods and players.
#'
#' @param strengths [\code{data.table}]\cr
#'   A \code{data.table} object containing players strengths with columns
#'   \code{player, period, r, RD, sigma}.
#' @param matches [\code{data.table}]\cr
#'   A \code{data.table} object containing matches with columns
#'   \code{player, period, opponent, is_winner}.
#' @param max_period [\code{numierc(1)}]\cr Maximum period up to which
#'   and including rating updates are calculated.
#' @param min_period [\code{numierc(1)}]\cr Minimum period from which
#'   and including rating updates are calculated.
#' @param tau [\code{numeric(1)}]\cr
#'   Parameter \code{tau} for the Glicko-2 algorithm, reasonable choices are in \code{[0.3, 1.2]}.
#' @param epsilon [\code{numeric(1)}]\cr
#'   Parameter \code{epsilon} for the Glicko-2 algorithm.
#'
#' @return A [\code{data.table}] object containing updates on
#'  strength with columns \code{player, period, r, RD, sigma} for all
#'  players and the given (\code{strengths}) and selected range of periods
#'  (\code{min_period}, \code{max_period}).
#' @import data.table
#' @export
#' @examples
#' data <- InitializeData(player = c("A", "B", "C"),
#'                        opponent = c("D", "D", "D"),
#'                        is_winner = c(1, 0, 0),
#'                        period = c(1, 1, 2))
#'
#' UpdateGlicko2Periods(data$strengths, data$matches)
UpdateGlicko2Periods <- function(strengths, matches,
                                      max_period = max(matches$period),
                                      min_period = max(strengths$period) + 1,
                                      tau = 0.5, epsilon = 1e-6) {

  # get list of all players
  unique_players <- strengths[period == 0, ]$player

  for (selected_period in min_period:max_period) {
    # select single period
    period_matches <- matches[period == selected_period, ]
    period_strengths <- strengths[period == (selected_period - 1), ]

    # update all players srengths within this period
    updated_strengths <-
      lapply(unique_players,
             UpdateGlicko2,
             selected_period = selected_period,
             period_strengths = period_strengths,
             period_matches = period_matches,
             tau = tau,
             epsilon = epsilon)

    # bind to data.table
    updated_strengths <- data.table::rbindlist(updated_strengths)

    # append to existing data.table
    strengths <- data.table::rbindlist(list(strengths, updated_strengths))
  }

  return(strengths)
}
