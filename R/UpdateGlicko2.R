#' Update a single player's rating for a given period.
#'
#' @param selected_player [\code{chracter(1)}]\cr
#'   Name of the player for whom updates should be calculated.
#' @param selected_period [\code{numeric(1)}]\cr
#'   Period for which updates should be calculated.
#' @param period_strengths [\code{data.table}]\cr
#'   A \code{data.table} object containing players strengths with columns
#'   \code{player, period, r, RD, sigma} for selected player, opponents
#'   and selected period (\code{selected_period}).
#' @param period_matches [\code{data.table}]\cr
#'   A \code{data.table} object containing matches with columns
#'   \code{player, period, opponent, is_winner}.
#' @param tau [\code{numeric(1)}]\cr
#'   Parameter \code{tau} for the Glicko-2 algorithm, reasonable choices are
#'   in the interval \code{[0.3, 1.2]}.
#' @param epsilon [\code{numeric(1)}]\cr
#'   Parameter \code{epsilon} for the Glicko-2 algorithm.
#'
#'
#' @return A [\code{data.table}] object containing updates for selected players
#'   \code{selected_player} strength with columns
#'   \code{player, period, r, RD, sigma}.
#' @import data.table
#' @export
UpdateGlicko2 <- function(selected_player, selected_period,
                               period_strengths, period_matches,
                               tau = 0.5, epsilon = 1e-6) {
  # get matches and strength values for selected player
  player_matches <- period_matches[player == selected_player, ]
  player_strength <- period_strengths[player == selected_player, ]

  # add opponents strength values
  data.table::setkey(player_matches, opponent)
  data.table::setkey(period_strengths, player)
  player_matches <- period_strengths[player_matches]

  # calculate the updated parameters for a single player
  glicko_update <- GetGlicko2Rating(player_strength$r,
                                    player_strength$RD,
                                    player_strength$sigma,
                                    player_matches$r,
                                    player_matches$RD,
                                    player_matches$is_winner,
                                    tau = tau,
                                    epsilon = epsilon)

  # format result in a table
  player_strength_updated <-
    data.table::data.table(player = selected_player,
                           period = selected_period,
                           r = glicko_update$r.new,
                           RD = glicko_update$RD.new,
                           sigma = glicko_update$sigma.new)
  return(player_strength_updated)
}
