context("Glicko-2 Update Periods")

# generate data
matches <- InitializeMatches(player = c("A", "A", "A"),
                             opponent = c("B", "C", "D"),
                             is_winner = c(1, 0, 0),
                             period = 1)


strengths <- InitializeStrengths(player = c("A", "B", "C", "D"),
                                 period = 0,
                                 r = c(1500, 1400, 1550, 1700),
                                 RD = c(200, 30, 100, 300),
                                 sigma = 0.06)


test_that("Glicko-2 update periods is correct", {
  expect_equal(UpdateGlicko2Periods(strengths, matches),
               data.table::data.table(player = rep(LETTERS[1:4], 2),
                                      period = c(rep(0, 4), rep(1, 4)),
                                      r = c(1500,
                                            1400,
                                            1550,
                                            1700,
                                            1464.050671,
                                            1398.143558,
                                            1570.394740,
                                            1784.421790),
                                      RD = c(200,
                                             30,
                                             100,
                                             300,
                                             151.51652412,
                                             31.67021528,
                                             97.70916852,
                                             251.56556453),
                                      sigma = c(rep(0.06, 4),
                                                0.05999598429,
                                                0.05999912373,
                                                0.05999941947,
                                                0.05999901176))
  )
})
