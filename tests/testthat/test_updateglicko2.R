context("Glicko-2 Update")

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


test_that("Glicko-2 update is correct", {
  expect_equal(UpdateGlicko2(selected_player = "A",
                             selected_period = 1,
                             period_strengths = strengths,
                             period_matches = matches),
               data.table::data.table(player = "A",
                                      period = 1,
                                      r = 1464.050671,
                                      RD = 151.5165241,
                                      sigma = 0.05999598429))
})
