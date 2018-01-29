context("Glicko-2 Functions")

test_that("Glicko-2 function is correct", {
  expect_equal(GetGlicko2Rating(1500, 200, 0.06,
                                c(1400, 1550, 1700),
                                c(30, 100, 300),
                                c(1, 0, 0), 0.5, 1e-6),
               list(r.new = 1464.050671,
                    RD.new = 151.5165241,
                    sigma.new = 0.05999598429))
})
