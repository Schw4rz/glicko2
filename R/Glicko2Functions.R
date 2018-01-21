## step 1
# defaults for player/team variables (r, RD, sigma)
kr <- 1500  # default player rating
kRD <- 350  # default rating deviation
ksigma <- 0.06  # default volatility
ktau <- 0.5  # system constant for volatility
kepsilon <- 1e-6  # only for optimization

## step 2
#' Convert rating r to mu on Glicko-2 scale.
#'
#' @param r [\code{numeric}]\cr
#'   A vector of ratings.
#' @return [\code{numeric}] of ratings on the Glicko-2 scale.
GetMu <- function(r) {
  (r - 1500) / 173.7178
}

#' Convert rating deviation RD to phi on Glicko-2 scale.
#'
#' @param RD [\code{numeric}]\cr
#'   A vector of rating deviations (RD).
#' @return [\code{numeric}] the numeric vector of rating deviations on
#'   the Glicko-2 scale.
GetPhi <- function(RD) {
  RD / 173.7178
}

## step 3
#' Get g(phi) for computing nu.
#'
#' @param phi [\code{numeric}]\cr
#' A vector of rating deviations on the Glicko-2 scale.
#' @return [\code{numeric}] the vector of g(phi).
GetG <- function(phi) {
  1 / sqrt(1 + (3 * phi^2) / pi^2)
}

#' Get Expected value E(mu, mu.op, phi.op) for computing nu.
#'
#' @param mu [\code{numeric(1)}]\cr
#'   A value of a players rating on the Glicko-2 scale.
#' @param mu.op [\code{numeric}]\cr
#'   A vector of opponents ratings on the Glicko-2 scale.
#' @param phi.op [\code{numeric}]\cr
#'   A vector of opponents rating deviations on the Glicko-2 scale.
#' @return [\code{numeric}] the vector of E(mu, mu_j, phi_j).
GetE = function(mu, mu.op, phi.op) {
  1 / (1 + exp(-GetG(phi.op) * (mu - mu.op)))
}

#' Get quantity nu.
#'
#' @param E.op [\code{numeric}]\cr
#'   A vetor of expected values E(mu, mu.op, phi.op) on the Glicko-2 scale.
#' @param g.op [\code{numeric}]\cr
#'   A vector of g(phi.op) on the Glicko-2 scale.
#' @return [\code{numeric(1)}] the numeric value of the quantity nu.
GetNu <- function(E.op, g.op) {
  1 / sum(g.op^2 * E.op * (1 - E.op))
}

## step 4
#' Get quantity Delta.
#'
#' @param nu [\code{numeric}]\cr
#'   A vector of the quantity nu.
#' @param E.op [\code{numeric}]\cr
#'   A vector of expected values E(mu, mu.op, phi.op) on the Glicko-2 scale.
#' @param g.op [\code{numeric}]\cr
#'   A vector of g(phi.op) on the Glicko-2 scale.
#' @param s.op [\code{numeric}]\cr
#'   A vector of results \code{s.op} coded as (1 = win, 0 = loss, 0.5 = draw).
#' @return [\code{numeric}] the numeric value of the quantity Delta.
GetDelta <- function(nu, E.op, g.op, s.op) {
  nu * sum(g.op * (s.op - E.op))
}

## step 5
#' Get value of helper function f(x).
#'
#' @param x [\code{numeric}]\cr
#'   A vector of the quantity x.
#' @param sigma [\code{numeric(1)}]\cr
#'   A numeric value of the rating volatility for a given player on the
#'   Glicko-2 scale.
#' @param phi [\code{numeric}]\cr
#'   A numeric value of the rating deviation on the Glicko-2 scale.
#' @param Delta [\code{numeric}]\cr
#'   A numeric value of quantity Delta.
#' @param nu [\code{numeric}]\cr
#'   A numeric value of the quantity nu.
#' @param tau [\code{numeric(1)}]\cr
#'   A numeric value of the system constant tau.
#' @return [\code{numeric}] the numeric value of the helper function.
GetFxHelper <- function(x, sigma, phi, Delta, nu, tau) {
  f <- (exp(x) * (Delta^2 - phi^2 - nu - exp(x)) /
          (2 * (phi^2 + nu + exp(x))^2)) - ((x - log(sigma^2)) / tau^2)
  return(f)
}

#' Get the updated value of sigma: sigma.new.
#'
#' @param sigma [\code{numeric(1)}]\cr
#'   A numeric value of the rating volatility for a given player on the
#'   Glicko-2 scale.
#' @param phi [\code{numeric(1)}]\cr
#'   A numeric value of the rating deviation on the Glicko-2 scale.
#' @param Delta [\code{numeric(1)}]\cr
#'   A numeric value of quantity Delta.
#' @param nu [\code{numeric(1)}]\cr
#'   A numeric value of the quantity nu.
#' @param tau [\code{numeric(1)}]\cr
#'   A numeric value of the system constant tau.
#' @return [\code{numeric(1)}] the updated value of \code{sigma}.
GetSigmaNew <- function(sigma, phi, Delta, nu, tau) {
  # 2
  A <- a <- log(sigma^2)
  if (Delta^2 > (phi^2 + nu)) {
    B <- log(Delta^2 - phi^2 - nu)
  } else {
    k <- 1
    while (GetFxHelper((a - k * tau), sigma, phi, Delta, nu, tau) < 0) {
      k <- k + 1
      # print(k)
    }
    B <- a - k * tau
  }
  # 3
  fA <- GetFxHelper(A, sigma, phi, Delta, nu, tau)
  fB <- GetFxHelper(B, sigma, phi, Delta, nu, tau)
  # 4
  while (abs(B - A) > kepsilon) {
    # print(abs(B - A))
    # a
    C <- A + (A - B) * fA / (fB - fA)
    fC <- GetFxHelper(C, sigma, phi, Delta, nu, tau)
    # b
    if ((fC * fB) < 0) {
      A <- B
      fA <- fB
    } else {
      fA <- fA / 2
    }
    # c
    B <- C
    fB <- fC
  }
  sigma.new <- exp(A/2)
  return(sigma.new)
}

# step 6
#' Get the updated value of phi to new pre-rating period: phi.pre.
#'
#' @param phi [\code{numeric}]\cr
#'   A vector of the rating deviations on the Glicko-2 scale.
#' @param sigma.new [\code{numeric}]\cr
#'   A vector of the updated rating volatility for a given player on the
#'   Glicko-2 scale.
#' @return [\code{numeric}] a vector of the updated value of phi to
#'   new pre-rating period.
GetPhiPre <- function(phi, sigma.new) {
  sqrt(phi^2 + sigma.new^2)
}

# step 7
#' Get the updated value of phi: phi.new.
#'
#' @param phi.pre [\code{numeric}]\cr
#'   A vector of pre-rating deviations on the Glicko-2 scale.
#' @param sigma.new [\code{numeric}]\cr
#'   A vector of the updated rating volatility on the Glicko-2 scale.
#' @param nu [\code{numeric(1)}]\cr
#'   A numeric value of the quantity nu.
#' @return [\code{numeric(1)}] a vector of the updated values of phi: phi.new.
GetPhiNew <- function(phi.pre, sigma.new, nu) {
  1 / sqrt(1 / phi.pre^2 + 1 / nu)
}

#' Get the updated value of mu: mu.new.
#'
#' @param mu [\code{numeric}]\cr
#'   A vector of ratings on the Glicko-2 scale.
#' @param phi.new [\code{numeric}]\cr
#'   A vector of updated rating volatilities on the Glicko-2 scale.
#' @param nu [\code{numeric}]\cr
#'   A vector of the quantity nu.
#' @param Delta [\code{numeric}]\cr
#'   A vector of the quantity Delta.
#' @return [\code{numeric}] A vector of the updated values of mu: mu.new.
GetMuNew <- function(mu, phi.new, nu, Delta) {
  mu + phi.new^2 * (Delta / nu)  # part in the brackets is the sum part
}

# step 8
#' Get the updated value of r: r.new.
#'
#' @param mu.new [\code{numeric}]\cr
#'   A vector of updated ratings on the Glicko-2 scale.
#' @return [\code{numeric}] A vector of the updated values of the rating r.
GetRNew <- function(mu.new) {
  173.7178 * mu.new + 1500
}

#' Get the updated value of RD: RD.new.
#'
#' @param phi.new [\code{numeric}]\cr
#'   A vector of the rating volatility on the Glicko-2 scale.
#' @return [\code{numeric}] a vector of the updated value of the rating RD.
GetRDNew <- function(phi.new) {
  173.7178 * phi.new
}

#' Get a Glicko-2 rating update for a player.
#'
#' @param r [\code{numeric(1)}]\cr
#'   A players current rating r.
#' @param RD [\code{numeric(1)}]\cr
#'   A players current rating deviation (RD).
#' @param sigma [\code{numeric(1)}]\cr
#'   A players rating volatility sigma.
#' @param r.op [\code{numeric}]\cr
#'   A vector of opponents current ratings r.
#' @param RD.op [\code{numeric}]\cr
#'   A vector of opponents current rating deviations RD.
#' @param s.op [\code{numeric}]\cr
#'   A vector of results s.op coded as (1 = win, 0 = loss, 0.5 = draw)
#'   from players perspective.
#' @param tau [\code{numeric(1)}]\cr
#'   A numeric value of the system constant tau.
#' @param epsilon [\code{numeric(1)}]\cr
#'   A parameter used to measure convergance of the algorithm.
#' @return A named [\code{list(3)}] with updated parameters of player
#'   strength, \code{r.new}, \code{RD.new}, \code{sigma.new}.
#' @export
#' @examples
#' # initial rating
#' e.r <- 1500
#' # initial rating deviation
#' e.RD <- 200
#' # initial volatility
#' e.sigma <- 0.06
#'
#' # opponents ratings
#' e.r.op <- c(1400, 1550, 1700)
#' # opponents rating deviations
#' e.RD.op <- c(30, 100, 300)
#' # results
#' e.s.op <- c(1, 0, 0)  # win, loss, loss
#' # system constant
#' e.tau <- 0.5
#'
#' GetGlicko2Rating(e.r, e.RD, e.sigma, e.r.op, e.RD.op, e.s.op, 0.5, 1e-6)
GetGlicko2Rating <- function(r = 1500, RD = 350, sigma = 0.06,
                             r.op, RD.op, s.op, tau = 0.5, epsilon = 1e-6) {
  # player
  mu <- GetMu(r)
  phi <- GetPhi(RD)

  if (length(s.op) > 0) {
    # opponents
    mu.op <- GetMu(r.op)
    phi.op <- GetPhi(RD.op)
    g.op <- GetG(phi.op)
    E.op <- GetE(mu, mu.op, phi.op)

    # calculate quantities for computation
    nu <- GetNu(E.op, g.op)
    Delta <- GetDelta(nu, E.op, g.op, s.op)

    # updates
    sigma.new <- GetSigmaNew(sigma, phi, Delta, nu, tau)
    phi.pre <- GetPhiPre(phi, sigma.new)
    phi.new <- GetPhiNew(phi.pre, sigma.new, nu)
    mu.new <- GetMuNew(mu, phi.new, nu, Delta)

    # convert
    r.new <- GetRNew(mu.new)
    RD.new <- GetRDNew(phi.new)

    # result
    result <- list(r = r, RD = RD, sigma = sigma,
                   r.new = r.new, RD.new = RD.new, sigma.new = sigma.new)

  } else {
    phi.new <- sqrt(phi^2 + sigma^2)
    RD.new <- GetRDNew(phi.new)
    # result
    result <- list(r.new = r, RD.new = RD.new, sigma.new = sigma)
  }
  return(result)

}
