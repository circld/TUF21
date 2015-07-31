#### Metric 1: Frequency of both teams still live by the end of the second-to-last episode
live.by.end <- function(scores) {
  score.to.win <- sum(scores) / 2
  blackzilians <- (runif(11) > .5) * 1  # logical vector
  ATT <- (blackzilians == 0) * 1
  
  # neither team has won by final match
  if (sum(ATT * scores[1:11]) <= score.to.win && sum(blackzilians * scores[1:11]) <= score.to.win) {
    return(1)
  }
  return(0)
}

n <- 10000
increasing.scores <- c(25, 25, 25, 25, 50, 50, 50, 50, 100, 100, 100, 100)
level.scores <- rep(1, 12)

live.inc <- replicate(n, live.by.end(increasing.scores))
live.lvl <- replicate(n, live.by.end(level.scores))

cat("\nWith incremental scoring, the show is still live", 
    sum(live.inc) / n * 100, "percent of the time by the last episode.\n")
cat("\nWith even scoring, the show is still live", 
    sum(live.lvl) / n * 100, "percent of the time by the last episode.\n")


#### Metric 2: Average episode by which one team has won
episode.win <- function(scores) {
  score.to.win <- sum(scores) / 2
  blackzilians <- (runif(11) > .5) * 1  # logical vector
  ATT <- (blackzilians == 0) * 1
  
  # earliest episode won
  ATT.win <- which(cumsum(ATT * scores[1:11]) > score.to.win)
  blackzilians.win <- which(cumsum(blackzilians * scores[1:11]) > score.to.win)
  
  result <- suppressWarnings(min(ATT.win[1], blackzilians.win[1], na.rm = TRUE))
  if (is.infinite(result)) {
    return(NA)
  }
  return(result)
}

ep.inc <- replicate(n, episode.win(increasing.scores))
ep.lvl <- replicate(n, episode.win(level.scores))

cat("\nEarliest episode a team wins, on average, with incremental scoring: ",
    sum(ep.inc, na.rm = TRUE) / length(na.exclude(ep.inc)), "\n")
cat("\nEarliest episode a team wins, on average, with even scoring: ",
    sum(ep.lvl, na.rm = TRUE) / length(na.exclude(ep.lvl)), "\n")
