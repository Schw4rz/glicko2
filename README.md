# glicko2
[![Travis build status](https://travis-ci.org/Schw4rz/glicko2.svg?branch=master)](https://travis-ci.org/Schw4rz/glicko2)
[![Coverage status](https://codecov.io/gh/Schw4rz/glicko2/branch/master/graph/badge.svg)](https://codecov.io/github/Schw4rz/glicko2?branch=master) 

An Implementation of [Mark Glickman's Glicko-2 Algorithm](http://www.glicko.net/glicko.html) for R. It closely follows Mark Glickman's suggested implementation which can be found [here](http://www.glicko.net/glicko/glicko2.pdf).

## Installation
```R
devtools::install_github("schw4rz/glicko2")
```
## Example

Update ratings for all players with `strengths` and `matches` stored in `data.table` objects created using the `InitializeMatches` and `InitializeStrengths` functions (example from Mark Glickman's suggested implementation [paper](http://www.glicko.net/glicko/glicko2.pdf)).

```R
library(glicko2)

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
                                 
# calculate updates for all players
UpdateGlicko2Periods(strengths, matches)
```
Result is a `data.table` object with the history of Glicko-2 ratings for each player and period:
```
   player period           r           RD         sigma
1:      A      0 1500.000000 200.00000000 0.06000000000
2:      B      0 1400.000000  30.00000000 0.06000000000
3:      C      0 1550.000000 100.00000000 0.06000000000
4:      D      0 1700.000000 300.00000000 0.06000000000
5:      A      1 1464.050671 151.51652412 0.05999598429
6:      B      1 1398.143558  31.67021528 0.05999912373
7:      C      1 1570.394740  97.70916852 0.05999941947
8:      D      1 1784.421790 251.56556453 0.05999901176
```

*work in progress*
