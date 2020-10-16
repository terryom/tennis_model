## APPARENTLY THIS SHOULD ALSO HAPPEN IN GIT BRANCHES! 
library(data.table)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
inv_logit <- function(u) 1 / (1 + exp(-u))
expectedBankRoll <- function(x, p, b){
  if(sum(x) > 1 | any(x<0)){return(-99999)}
  expBankRoll = p*log(1+(b-1)*x) + (1-p)*log(1-x)
  return(sum(expBankRoll))
}
## let's merge more data 
dat <- fread("./data/atp_matches_2020.csv")
dat2 <- fread("./data/atp_matches_2019.csv")

all_dat <- rbind(dat, dat2)
all_players <- unique(c(all_dat$winner_id, all_dat$loser_id))
K <- length(all_players)

#playas <- data.table(player = all_players)

matches <- all_dat[, .(y0 = winner_id, y1= loser_id, y = 0, y0age = winner_age, y1age = loser_age )] # player 0 wins here automatically 

## I wonder for the Stan model if there needs to be variation in y 
## hmmm. just randomise the pairing so 
matches[, paste0("player", 0:1) := {
  pos <- sample(1:2,1) 
  sd <- data.frame(copy(.SD)) ## DT characteristics can be annoying here 
  p1 <- sd[1,pos, drop=T] # drop gets ride of data.frame characteristics 
  p2 <- sd[1,(1:2)[pos != 1:2], drop=T]
  list(p1, p2)
}, by = 1:nrow(matches)]


matches[, paste0("player", 0:1, "_age") := {
  age1 <- fifelse(y0 == player0, y0age, y1age)
  age2 <- fifelse(y0 == player1, y1age, y0age)
}]

matches[, win := fifelse(y0 == player0, 0, 1)]
matches <- matches[, .(player0, player1, y = win, player0_age, player1_age)]
matches


mod_dat <- list(
  K = length(all_players),
  N = nrow(matches),
  ## need to index players, rather than use these ID code
  player0 = match(matches$player0, all_players),
  player1 = match(matches$player1, all_players),
  player0_age = mathes$player0_age, 
  player1_age = mathes$player1_age, 
  y = matches$y
)




## let's go again with a prior on the distribution of alphas
## Why macros do Bayesian after all 
#
stan_prog3 <- "
  data {
    int<lower = 0> K;                     // players
    int<lower = 0> N;                     // matches
    int<lower=1, upper = K> player1[N];   // player 1 for game n
    int<lower=1, upper = K> player0[N];   // player 0 for game n
    int<lower = 0, upper = 1> y[N];       // winner for match n
    real player0_age[N];                   // 
    real player1_age[N];                   // 
  }
parameters {
  vector[K] alpha;                      // ability for player n
  real<lower=0, upper = 10> sigma  ;    // hyper prior on the random effect
  real beta;                            // coefficient on age  
  }

model {
  alpha ~ normal(0, sigma);                   // normal prior on alpha
  beta ~ normal(0, 50);                   // normal prior on alpha
  y ~ bernoulli_logit((alpha[player1] + beta * player0_age) - (alpha[player0]+ beta * player1_age));
}
"
## Now, I wonder how we could get the odds data ... 
bayes_model_gq <-
  stan(model_code =  stan_prog3, 
       data = mod_dat)

bayes_model_gq

rank_dat <- data.table(
  all_players)

## now get player names 
## winners 
players_data <- unique(rbind(
  all_dat[, .(id = winner_id, names = winner_name)],
  all_dat[, .(id = loser_id, names = loser_name)])
)

rank_dat[, name := players_data$names[match(all_players, players_data$id)]]

## Amazingly, have just replicated the ATP database .. great 

## Would be cool to predict the result of the French open using these rankings 
## or actually, the alphas!
### Let's go one more time using the entire vector of alpha per player
n_sims <- length(extract(bayes_model_gq)$alpha[ , 1])
out <- matrix(NA, ncol = n_sims, nrow = K)
for (k in 1:length(all_players)){
  out[k,] <- extract(bayes_model_gq)$alpha[ , k]
}

options(scipen=10)
test_kelly <- function(p1, p2, odds){
  
  pla1 <- rank_dat[ ,which(name %like% p1)] 
  pla2 <- rank_dat[ ,which(name %like% p2)] 
  
  if(any(length(pla1) > 1, length(pla2) >1)) stop("players not unique")
  
  ## probabilities 
  p <- inv_logit(out[pla1,]  -  out[pla2,])
  
  kelly_mean <- 
    optimize(function(x) (-1)*expectedBankRoll(x, 
                                               p = mean(p),
                                               b = odds), c(-0.5,1))$minimum
  list("Kelly of the mean" = kelly_mean, 
       "p" = mean(p), 
       "b" = 1/odds)
  
}

## Market has underpriced Zverev so
test_kelly("Alexander Zverev", "Cecc", 1.4)
test_kelly("Schwartz", "Gomb", 1.07)
test_kelly("Gomb", "Schwartz", 9)
test_kelly("Gomb", "Schwartz", 2)

33.75/3.75 
8.09/3.75 
7.06/3.75
6.33/3.75
4.5

1 / (33.75/3.75)
1 / (6.75/3.75)

1/3.75


test_kelly("Gomb", "Schwartz", 33.75/3.75 )
test_kelly("Gomb", "Schwartz", 10/3.75 )
test_kelly("Gomb", "Schwartz", 10/3.75 )

test_kelly("Gomb", "Schwartz", 2.157333)


test_kelly("Gomb", "Schwartz", 9)
test_kelly("Wawrinka", "Gaston", 1.13)
test_kelly("Gaston", "Wawrinka", 6)
test_kelly("Anderson", "Rublev", 4.5)
test_kelly("Bautista", "Carreno", 1.73)
test_kelly("Aljaz Bedene", "Stefanos Tsitsipas", 7)


test_kelly("Aljaz Bedene", "Stefanos Tsitsipas", 7)


test_kelly("Fucsovics", "Rublev",4.7)
test_kelly("Carreno", "Altmai",1.25)
test_kelly("Dimitrov", "Stefanos Tsitsipas",3.65)
test_kelly("Djok", "Khachanov",1.05)


expectedBankRoll2 <- function(x, p, b){
  #if(sum(x) > 1 | any(x<0)){return(-99999)}
  expBankRoll = p*log(1+(b-1)*x) + (1-p)*log(1-x)
  return(sum(expBankRoll))
}

test_kelly2 <- function(p1, p2, odds){
  
  pla1 <- rank_dat[ ,which(name %like% p1)] 
  pla2 <- rank_dat[ ,which(name %like% p2)] 
  
  if(any(length(pla1) > 1, length(pla2) >1)) stop("players not unique")
  
  ## probabilities 
  p <- inv_logit(out[pla1,]  -  out[pla2,])
  
  hist(p)
  
  kelly_mean <- 
    optimize(function(x) (-1)*expectedBankRoll2(x, 
                                                p = mean(p),
                                                b = odds), c(-0.5,1))$minimum
  list("Kelly of the mean" = kelly_mean, 
       "p" = mean(p), 
       "b" = 1/odds)
  
}

test_kelly2("Djok", "Khachanov",1.06)
## djoko is way overpriced so ... not sure I want to put 50% of my bank roll on it though!!!

## Honestly .. I think the 5-set nature needs to be accounted for here. Djoko is a monster in 5 sets. Not betting on him at all 
test_kelly2("Carreno", "Altmai",1.25)

hist(out[rank_dat[ ,which(name %like% "Altmai")] ,])
hist(out[rank_dat[ ,which(name %like% "Carreno")] ,])
all_dat[winner_name %like% "Altmai"]
all_dat[loser_name %like% "Altmai"]
all_dat[winner_name %like% "Carreno"]

## wait a second! We have one observation for this guy
## So his alpha is just 0! 
## Surely you need to account for probability of observing this guy in the data .. 
## Or at least use the known things about him 
## he did beat berretini though


test_kelly2("Fucsovics", "Rublev",4.7)

test_kelly2("Dimitrov", "Stefanos Tsitsipas",3.65)
## more comfortable with this tbh 
20*test_kelly2("Dimitrov", "Stefanos Tsitsipas",3.6)$`Kelly of the mean`
# paddy power odds much worser 
test_kelly2("Dimitrov", "Stefanos Tsitsipas",3.2)
test_kelly2("Stefanos Tsitsipas","Dimitrov",1.3)
test_kelly2("Stefanos Tsitsipas","Dimitrov",1.3)


test_kelly2("Khachanov","Djok", 12)
test_kelly("Khachanov","Djok", 12)
0.05612385*28

