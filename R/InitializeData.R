#' Initialize data (matches and strengths) for Glicko-2
#'
#' @param player [\code{charcater}] or [\code{integer}]\cr
#'   Players, either by character or by index.
#' @param opponent [\code{charcater}] or [\code{integer}]\cr
#'   Opponents of players, either by character or by index.
#' @param is_winner [\code{numeric}] or [\code{logical}]\cr
#'   Indicator wheter \code{player} won against \code{opponent}.
#'   If \code{numeric} must only containt ones and zeros.
#' @param period [\code{integer}]\cr
#'   Period the match of \code{player} vs. \code{opponent}.
#' @param is_symmetric [\code{logical(1)}]\cr
#'   Whether data is already provided symmetrically. E.g. if one match was
#'   played and \code{is_symmetric=TRUE} there must exist two entries in
#'   all supplied vectors, one for the first player as player and the second
#'   player as opponent and vice versa (see examples below).
#' @param initial_r [\code{numeric(0)}]\cr
#'   Initial rating for Glicko-2.
#' @param initial_RD [\code{numeric(0)}]\cr
#'   Initial rating deviance for Glicko-2.
#' @param initial_sigma [\code{numeric(0)}]\cr
#'   Initial sigma for Glicko-2.
#'
#' @return A named [\code{list(2)}] with a [\code{data.table}] object
#'   containing players strengths (\code{strengths}) with columns
#'   \code{player, period, r, RD, sigma} and a [\code{data.table}] object
#'   containing a symmetric set of matches with columns (\code{mactches})
#'   \code{player, opponent, is_winner, period}
#' @export
#'
#' @examples
#' InitializeData(player = c("A", "B", "C"),
#'                opponent = c("D", "D", "D"),
#'                is_winner = c(1, 0, 0),
#'                period = c(1, 1, 2))
InitializeData <- function(player, opponent, is_winner, period,
                           is_symmetric = FALSE, initial_r = 1500,
                           initial_RD = 350, initial_sigma = 0.06) {

  matches <- InitializeMatches(player, opponent, is_winner, period,
                               is_symmetric)
  strengths <- InitializeStrengths(unique(c(player, opponent)))

  return(list(strengths = strengths, matches = matches))
}

#' Initialize  matches for Glicko-2
#'
#' @param player [\code{charcater}] or [\code{integer}]\cr
#'   Players, either by character or by index.
#' @param opponent [\code{charcater}] or [\code{integer}]\cr
#'   Opponents of players, either by character or by index.
#' @param is_winner [\code{numeric}] or [\code{logical}]\cr
#'   Indicator wheter \code{player} won against \code{opponent}.
#'   If \code{numeric} must only containt ones and zeros.
#' @param period [\code{integer}]\cr
#'   Period the match of \code{player} vs. \code{opponent}.
#' @param is_symmetric [\code{logical(1)}]\cr
#'   Whether data is already provided symmetrically. E.g. if one match was
#'   played and \code{is_symmetric=TRUE} there must exist two entries in
#'   all supplied vectors, one for the first player as player and the second
#'   player as opponent and vice versa (see examples below).
#'
#' @return A [\code{data.table}] object containing a symmetric
#' set of matches with columns \code{player, opponent, is_winner, period}.
#' @export
#'
#' @examples
#' InitializeMatches(player = c("A", "B", "C"),
#'                   opponent = c("D", "D", "D"),
#'                   is_winner = c(1, 0, 0),
#'                   period = c(1, 1, 2))
InitializeMatches <- function(player, opponent, is_winner, period,
                              is_symmetric = FALSE) {

  if (is_symmetric) {
    # get data in data.table of matches by period
    matches <- data.table::data.table(player = player,
                                      opponent = opponent,
                                      is_winner = is_winner,
                                      period = period)
  } else {
    # get symmetric data.table of matches by period
    matches <- data.table::data.table(player = c(player, opponent),
                                      opponent = c(opponent, player),
                                      is_winner = c(is_winner, 1 - is_winner),
                                      period = c(period, period))
  }

  return(matches)
}

#' Initialize  strengths for Glicko-2
#'
#' @param player [\code{charcater}] or [\code{integer}]\cr
#'    of players, either by character or by index. Must be of
#'    \code{length(period)} is \code{period} is given.
#' @param period [\code{integer}]\cr
#'   period indices.
#' @param r [\code{numeric}]\cr
#'   ratings from Glicko-2.
#' @param RD \code{numeric}]\cr
#'  rating deviations RD from Glicko-2.
#' @param sigma [\code{numeric}]\cr
#'   sigma from Glicko-2.
#' @param initial_r [\code{numeric(0)}]\cr
#'   Initial rating for Glicko-2.
#' @param initial_RD [\code{numeric(0)}]\cr
#'   Initial rating deviance for Glicko-2.
#' @param initial_sigma [\code{numeric(0)}]\cr
#'   Initial sigma for Glicko-2.
#'
#' @return A [\code{data.table}] object containing players
#'  strengths with columns \code{player, period, r, RD, sigma}.
#' @export
#'
#' @examples
#' InitializeStrengths(player = c("Foo", "Bar", "Baz"))
InitializeStrengths <- function(player, period = NULL, r = NULL, RD = NULL,
                                sigma = NULL, initial_r = 1500,
                                initial_RD = 350, initial_sigma = 0.06) {


  if (!is.null(period) & !is.null(r) & !is.null(RD) & !is.null(sigma)) {

    # get table with predefined strengths
    strengths <- data.table::data.table(player = player,
                                        period = period,
                                        r = r,
                                        RD = RD,
                                        sigma = sigma)

  } else {
    # no strengths known -> initialize only
    strengths <- data.table::data.table(player = player,
                                        period = 0,
                                        r = initial_r,
                                        RD = initial_RD,
                                        sigma = initial_sigma)
  }
  return(strengths)
}
