# White elephant gift exchange
# Blog post at http://blog.analyzestuff.com/2013/12/how-fair-is-white-elephant.html
#
# Rules:
# 1) N players each bring one gift
# 2) Each round, a player
# 3) Gifts can not be stolen more than three times each
# 4) Gifts cannot be stolen more than once per round
#
# Assumptions:
# 1) Each player has a
# 2) Players steal with probability p = (number of gifts taken) / N
# 3) Players steal the gift of maximal utility to them

library(data.table)
library(plyr)

# Maximum number of steals per gift
kMaxSteals <- 3

ChooseGift <- function(current.player, utility.matrix, gifts) {
  # Function for a single player to choose a White Elephant gift (one round)
  #
  # Logic for a single turn is as follows:
  #   1) Player will choose an unopened gift with probability
  #      p = (number of gifts taken) / N
  #   2) If player chooses an unopened gift, they will choose one at random
  #   3) If player chooses to steal, they will steal their favorite gift
  #      of those that have not been stolen this round
  #
  # Args:
  #   current.player: Person # taking the turn
  #   utility.matrix: utility matrix with persons in rows and gifts as columns
  #   gifts: data.table for each gift's state. Includes columns for
  #          1) gift
  #          2) is.visible
  #          3) steals (number of steals)
  #          4) stolen.this.round
  #          5) is.choosable
  #          6) player (owner)
  #          7) is.stealable
  #
  # Return:
  #   List with two elements:
  #   1) gifts, an updated data.table of all gifts
  #   2) stealee, the ID of the player from whom the gift was stolen.
  #      NA if player didn't steal.

  # Calculate stealing probability
  prob.steal <- gifts[, sum(is.stealable) / sum(is.choosable)]
  # Generate a random U(0, 1) number to determine whether the player will steal
  will.steal <- (runif(1) < prob.steal)

  if(will.steal) {
    # If they steal, choose their favorite
    # Vector of gift utilities for current player
    player.gift.utilities <- utility.matrix[current.player, ]
    # Maximum utility for player
    max.stealable.utility <-
      max(player.gift.utilities[gifts[is.stealable == T]$gift])
    # Corresponding gift
    chosen.gift <- which(player.gift.utilities == max.stealable.utility)
    # Find stealee
    stealee <- gifts[chosen.gift]$player
    # Alter gift record
    gifts[chosen.gift, stolen.this.round := T]
    gifts[chosen.gift, steals := steals + 1]
  } else {
    # Otherwise choose a random wrapped gift
    unopened.gifts <- gifts[is.na(player)]$gift
    # Check whether the length is 1
    # This is necessary because sample(3, 1) is equivalent to sample(1:3, 1)
    if(length(unopened.gifts) == 1) {
      chosen.gift <- unopened.gifts
    } else {
      chosen.gift <- sample(unopened.gifts, 1)
    }
    stealee <- NA
  }

  gifts[chosen.gift, player := current.player]

  return(list(gifts=gifts, stealee=stealee))
}

IsChoosable <- function(steals, stolen.this.round)
  return(steals < kMaxSteals & !stolen.this.round)

IsStealable <- function(is.choosable, player)
  return(is.choosable & !is.na(player))

# Function to generate mapping including ranks from utility matrix
RankMap <- function(utility.matrix) {
  require(reshape2)
  require(data.table)
  rank.map <- data.table(melt(apply(utility.matrix, 1, function(x) rank(-x))))
  utility.map <- data.table(melt(utility.matrix))
  result <- cbind(utility.map, rank.map$value)
  setnames(result, c("player", "gift", "utility", "rank"))
  return(result)
}

Play <- function(utility.matrix) {
  # Play full game of White Elephant
  #
  # Turns are taken until the gift unwrapped last is chosen
  #
  # Args:
  #   utility.matrix: utility matrix with persons in rows and gifts as columns
  #
  # Return:
  #   data.table representing the N gifts and their owners

  N <- nrow(utility.matrix)

  # Initialize gifts data.table
  gifts <- data.table(gift=1:N,
                      steals=0,
                      stolen.this.round=F,
                      player=as.integer(NA)) # Gift owner
  gifts[, is.choosable := IsChoosable(steals, stolen.this.round)]
  gifts[, is.stealable := IsStealable(is.choosable, player)]
  # Initialize stealee
  stealee <- NA
  # Initialize number of unopened gifts
  n.unopened.gifts <- N

  # Continue while at least one gift remains unopened
  while(n.unopened.gifts > 0) {
    # Assign current.player and reset stolen.this.round for new rounds
    is.first.round <- (n.unopened.gifts == N)
    if(is.first.round) {
      # Player 1 plays first round
      current.player <- as.integer(1)
    } else if(is.na(stealee)) {
      # If not a steal, move to next player in queue
      gift.owners <- gifts[!is.na(player)]$player
      current.player <- max(gift.owners) + as.integer(1)
      # Also reset all stolen.this.round field for all gifts, as this
      # constitutes a new round
      gifts[, stolen.this.round := F]
    } else {
      # If gift was stolen, assign current.player to stealee
      current.player <- stealee
    }

    # Take a turn
    turn.outcome <- ChooseGift(current.player, utility.matrix, gifts)
    gifts <- turn.outcome$gift
    stealee <- turn.outcome$stealee

    # Update gift flags and unopened count
    gifts[, is.choosable := IsChoosable(steals, stolen.this.round)]
    gifts[, is.stealable := IsStealable(is.choosable, player)]
    n.unopened.gifts <- sum(is.na(gifts$player))
  }

  # Merge on players' preference ranks, drop unnecessary variables and output
  rank.matrix <- RankMap(utility.matrix)
  gifts.merged <- merge(gifts, rank.matrix, by=c("player", "gift"), all.x=T)
  gifts.merged[, stolen.this.round := NULL]
  gifts.merged[, is.choosable := NULL]
  gifts.merged[, is.stealable := NULL]
  return(gifts.merged)
}

GenerateUtilityMatrix <- function(n.players, gift.utility.weight) {
  # Generate a player-gift utility matrix
  #
  # Args:
  #   n.players: Number of players (and therefore gifts)
  #   gift.utility.weight: Weight given to each gift's utility relative to
  #                        person/gift level noise. For example, set to 1
  #                        values in each column would be the same
  #
  # Returns:
  #    Square matrix with players as rows and gifts as columns, utility in each
  #    cell.

  # Generate gift-level utility
  gift.utility <- runif(n.players)

  return(matrix((gift.utility * gift.utility.weight) +
                (runif(n.players ^ 2) * (1 - gift.utility.weight)),
                n.players, n.players, byrow=T))
}

mcadply <- function(x, fun) {
  require(data.table)
  require(plyr)
  require(parallel)
  return(data.table(rbind.fill(mclapply(x, fun))))
}

Simulate <- function(n.players, n.runs) {
  result <- mcadply(seq(n.runs),
                    function(x) Play(GenerateUtilityMatrix(n.players, 0.5)))
  # Add run number and return
  result[, run := sort(rep(seq(n.players), n.runs))]
  return(result)
}

# Set of results
SimulateMultiplePlayers <- function(player.seq, n.runs) {
  result <- data.table(adply(player.seq, 1, function(x) Simulate(x, n.runs)))
  setnames(result, "X1", "n.players")
  return(result)
}

result <- SimulateMultiplePlayers(seq(20), 10000)
save.image()

class(result$n.players) <- "integer"

# Analysis by number of players
# Average steals per gift, average utility per person, and Gini coefficient
library(ineq)

n.player.summary <-
  result[, list(mean.steals=mean(steals),
                mean.utility=mean(utility),
                utility.per.turn=mean(utility / (steals + 1)),
                gini=ineq(utility, type="Gini"),
                mean.utility.first.player=
                mean(ifelse(player == 1, utility, NA), na.rm=T),
                mean.utility.final.player=
                mean(ifelse(player == n.players, utility, NA), na.rm=T)),
         n.players]

write.csv(n.player.summary, "n_player_summary.csv", row.names=F)

# Linear model of utility
summary(lm(utility ~ n.players + player, data=result))

# Analysis
AnalyzeSimulations <- function(result) {
  # Produces charts and models to analyze White Elephant simulation results
  #
  # Args:
  #   result: data.table holding simulation results as produced by Simulate
  #
  # Returns:
  #   Nothing; charts are printed

  # Determine number of players
  n.players <- max(result$player)


  # Averages by player
  result[, list(mean.utility=mean(utility),
                mean.rank=mean(rank),
                pct.top.choice=sum(rank == 1) / .N,
                pct.bottom.choice=sum(rank == n.players) / .N),
         player]

  # Likelihood for top choice
  summary(glm((rank == 1) ~ factor(player), data=result, family="binomial"))
  summary(glm((rank == 1) ~ player, data=result, family="binomial"))

  # Likelihood for bottom choice
  summary(glm((rank == n.players) ~ factor(player), data=result,
              family="binomial"))
  summary(glm((rank == n.players) ~ player, data=result, family="binomial"))

  # Linear model on utility
  summary(lm(utility ~ player, data=result))
  summary(lm(utility ~ factor(player), data=result))

  # Linear model on rank
  summary(lm(rank ~ player, data=result))
  summary(lm(rank ~ factor(player), data=result))
}

save.image()
