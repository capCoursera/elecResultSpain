library("dplyr", lib.loc="~/R/x86_64-redhat-linux-gnu-library/3.2")


dHondt <- function (parties, votes, seats,threshold=0)
{ if (threshold != 0)
  {
    sumVotes <- sum(votes)
    votes[votes < (sumVotes*threshold)] <- 0
  }
  .temp <- data.frame(parties = rep(parties, each = seats),
                      scores = as.vector(sapply(votes, function(x) x/1:seats)))
  out <- with(.temp, (parties[order(-scores)][1:seats]))
  out <- data.frame(SciencesPo::freq(out)[, 1:3])
  out <- out %>% dplyr::arrange(dplyr::desc(freq))
  names(out) <- c("Parties", "d'Hondt", "Perc")
  return(out)
}
