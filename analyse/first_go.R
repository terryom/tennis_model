## I am leaving this here from my chromebook downstairs.

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

all_dat

all_players <- unique(c(all_dat$winner_id, all_dat$loser_id))
K <- length(all_players)

matches <- all_dat[, .(y0 = winner_id, y1= loser_id, y = 0)] # player 0 wins here automatically 

## I wonder for the Stan model if there needs to be variation in y 
## hmmm. just randomise the pairing so 
matches[, paste0("player", 0:1) := {
  pos <- sample(1:2,1) 
  sd <- data.frame(copy(.SD)) ## DT characteristics can be annoying here 
  p1 <- sd[1,pos, drop=T] # drop gets ride of data.frame characteristics 
  p2 <- sd[1,(1:2)[pos != 1:2], drop=T]
  list(p1, p2)
}, by = 1:nrow(matches)]

matches[, win := fifelse(y0 == player0, 0, 1)]
matches <- matches[, .(player0, player1, y = win)]
matches


mod_dat <- list(
  K = length(all_players),
  N = nrow(matches),
  ## need to index players, rather than use these ID code
  player0 = match(matches$player0, all_players),
  player1 = match(matches$player1, all_players),
  y = matches$y
)


stan_prog <- "
  data {
    int<lower = 0> K;                     // players
    int<lower = 0> N;                     // matches
    int<lower=1, upper = K> player1[N];   // player 1 for game n
    int<lower=1, upper = K> player0[N];   // player 0 for game n
    int<lower = 0, upper = 1> y[N];       // winner for match n
  }
parameters {
  vector[K] alpha;                      // ability for player n
}
model {
  y ~ bernoulli_logit(alpha[player1] - alpha[player0]);
}
"



## chains not mixing with just 2020, meaning something gone wrong. 
# players (alpha params) > matches, so not identified
mle_model <-
  stan(model_code =  stan_prog, 
       data = mod_dat)



## let's go again with a prior on the distribution of alphas
## Why macros do Bayesian after all 

stan_prog2 <- "
  data {
    int<lower = 0> K;                     // players
    int<lower = 0> N;                     // matches
    int<lower=1, upper = K> player1[N];   // player 1 for game n
    int<lower=1, upper = K> player0[N];   // player 0 for game n
    int<lower = 0, upper = 1> y[N];       // winner for match n
  }
parameters {
  vector[K] alpha;                      // ability for player n
}
model {
  alpha ~ normal(0, 1);                   // normal prior on alpha
  y ~ bernoulli_logit(alpha[player1] - alpha[player0]);
}
"

## OPEN QUESTION
## does a normally distributed prior make sense?
## would something with a long-tail be better? E.g. I don't there's a
##  a player as bad, as Roger Federer is good: they would not make 
## it on the tour!!

## d
bayes_model <-
  stan(model_code =  stan_prog2, 
       data = mod_dat)

bayes_model
## few priors did the trick ... v nice


print(bayes_model, "alpha", probs=c(0.05, 0.5, 0.95))


## Rank the players
## stan programs have the ability to generate quantities (and intervals

# generated quantities {
#   int<lower=1, upper=K> ranking[K];       // rank of player ability
#   {
#     int ranked_index[K] = sort_indices_desc(alpha);
#     for (k in 1:K)
#       ranking[ranked_index[k]] = k;
#   }
# }

stan_prog3 <- "
  data {
    int<lower = 0> K;                     // players
    int<lower = 0> N;                     // matches
    int<lower=1, upper = K> player1[N];   // player 1 for game n
    int<lower=1, upper = K> player0[N];   // player 0 for game n
    int<lower = 0, upper = 1> y[N];       // winner for match n
  }
parameters {
  vector[K] alpha;                      // ability for player n
}
model {
  alpha ~ normal(0, 1);                   // normal prior on alpha
  y ~ bernoulli_logit(alpha[player1] - alpha[player0]);
}

generated quantities {
int<lower=1, upper=K> ranking[K];       // rank of player ability
   {
   int ranked_index[K] = sort_indices_desc(alpha);
     for (k in 1:K)
       ranking[ranked_index[k]] = k;
   }
}
"
## Now, I wonder how we could get the odds data ... 
bayes_model_gq <-
  stan(model_code =  stan_prog3, 
       data = mod_dat)

rank_hat <- rep(NA, length(all_players))
for (k in 1:length(all_players))
  rank_hat[k] <- mean(extract(bayes_model_gq)$ranking[ , k])

length(all_players)

rank_dat <- data.table(
  all_players,
  rank_hat)

rank_dat <- rank_dat[order(rank_hat)]

## now get player names 
## winners 
players_data <- unique(rbind(
  all_dat[, .(id = winner_id, names = winner_name)],
  all_dat[, .(id = loser_id, names = loser_name)])
)

rank_dat[, name := players_data$names[match(all_players, players_data$id)]]

## Amazingly, have just replicated the ATP database .. great 


rank_dat[1:10]


## Would be cool to predict the result of the French open using these rankings 
## or actually, the alphas!

alpha_low <- rep(NA,length(all_players)) 
alpha_high <- rep(NA,length(all_players))
alpha_mean<- rep(NA,length(all_players))
for (k in 1:length(all_players)){
  out <- extract(bayes_model_gq)$alpha[ , k]
  alpha_mean[k] <- mean(out)
  alpha_high[k] <- quantile(out, 0.025)
  alpha_low[k] <- quantile(out, 1-0.025)
}

rank_dat$alpha_low <- alpha_low
rank_dat$alpha_high <- alpha_high
rank_dat$alpha_mean <- alpha_mean

## Writing this while the 2020 French OPen is on 

# So what's the probability that 
# Jack Sock beats Dominic Theim
# wow, super low
inv_logit(
  rank_dat[name %like% "Sock", alpha_mean] - 
    rank_dat[name %like% "Dominic Thiem", alpha_mean]
)

# Mackenzie McDonald beats Rafa (not taking into account clay here!)
rank_dat[name %like% "Nadal", alpha_mean]

inv_logit(
  rank_dat[name %like% "Mack", alpha_mean] - 
    rank_dat[name %like% "Nadal", alpha_mean]
)

# well, not that low!

# these matches are already a set in ... so let's look at a match tonight
# Zverev ... Herbert 
# and indeed .. they went to 5 sets 
## should really learn regex for this 
inv_logit(
  rank_dat[name %like% "Alexander Zverev", alpha_mean] - 
    rank_dat[name %like% "Herbert", alpha_mean]
)


inv_logit(
  rank_dat[name %like% "Alexander Zverev", alpha_mean] - 
    rank_dat[name %like% "Herbert", alpha_mean]
)

inv_logit(
  rank_dat[name %like% "Shapo", alpha_mean] - 
    rank_dat[name %like% "Baena", alpha_mean]
)

inv_logit(
  rank_dat[name %like% "Thiem$", alpha_mean] - 
    rank_dat[name %like% "Ruud", alpha_mean]
)
1/1.16


inv_logit(
  rank_dat[name %like% "Sonego$", alpha_mean] - 
    rank_dat[name %like% "Fritz", alpha_mean]
)
1/2.06

# interesting ... 
#I actually make Sonego the better clay-courter on 18-month clay data, and my model made him a slight favourite to progress to Sunday's fourth round.


inv_logit(
  rank_dat[name %like% "Alexander Zverev", alpha_mean] - 
    rank_dat[name %like% "Cecc", alpha_mean]
)
1/1.4


inv_logit(
  rank_dat[name %like% "Alexander Zverev", alpha_mean] - 
    rank_dat[name %like% "Cecc", alpha_mean]
)

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
test_kelly("Wawrinka", "Gaston", 1.13)
test_kelly("Gaston", "Wawrinka", 6)
test_kelly("Anderson", "Rublev", 4.5)
test_kelly("Bautista", "Carreno", 1.73)
test_kelly("Aljaz Bedene", "Stefanos Tsitsipas", 7)


26*0.31

fuc <- rank_dat[, which(name %like% "Fucsovics")] 
mon <- rank_dat[, which(name %like% "Monteiro")] 

hist(
  out[fuc,] 
)

hist(
  out[mon,]
)

hist(
  inv_logit(out[fuc,]  -  out[mon,])
)

# this is beter
mean(inv_logit(out[fuc,]  -  out[mon,]))

optimize(function(x) (-1)*expectedBankRoll(x, 
                                           p = , b), c(-0.5,1))




# 70% probability 
0.7 / (1-0.7) #  
# 1/x = 2.3
# 1 = 2.3*x
# x = 1/2.3

# 0.43  to 1  oods 
## approx 4 to 9

## Zverev's odds are much higher in PP of course
## I presume their model is quite alright 
## unless you believe this model I built in 10 mins is superior, in which case you should 
## bet on the other guy ... 
# what are the odds on PP right now?


inv_logit(
  rank_dat[name %like% "Fucsovics", alpha_mean] - 
    rank_dat[name %like% "Ramos", alpha_mean]
)
# this match wasn't even close .. Fucsovics is playing a lot better than his ranking suggests 

inv_logit(
  rank_dat[name %like% "Fucsovics", alpha_mean] - 
    rank_dat[name %like% "Medvedev", alpha_mean]
)
# very low chance here but he won


inv_logit(
  rank_dat[name %like% "Fucsovics", alpha_mean] - 
    rank_dat[name %like% "Monteiro", alpha_mean]
)

inv_logit(
  rank_dat[name %like% "Gaston", alpha_mean] - 
    rank_dat[name %like% "Wawr", alpha_mean]
)
1/6

#Odds on PP are 1/2 
# So get 1 back for every 2 bet 
dec <- 1/2
# implied probability

1/ (1.5)
## So I now have an edge I think! (did not adjust for clay but sure look)




# expectedBankRoll <- function(x, p, b){
#   expBankRoll = p*log(1+(b-1)*x) + (1-p)*log(1-x)
#   return(sum(expBankRoll))
# }

p = c(inv_logit(
  rank_dat[name %like% "Fucsovics", alpha_mean] - 
    rank_dat[name %like% "Monteiro", alpha_mean]
))

b = c(1.5) ## PP odds right now 

optim(c(0.5), function(x) (-1)*expectedBankRoll(x, p, b))

optimize(function(x) (-1)*expectedBankRoll(x, p, b), c(-0.5,1))

# I should bet 23% of my bankroll on this bet so!!!

# I have 26.25
B <- 26.25*0.23
B

# I think there's a fairly large issue here which is that my probability has
# a distribution. 
# So not sure if the Kelly bet follows! 
# In the original Ed Thorpe application, he can calculate the probability. 
# Here I just estimated it 
# surely that is an issue 



out[1:10,1:10]

fuc <- rank_dat[, which(name %like% "Fucsovics")] 
mon <- rank_dat[, which(name %like% "Monteiro")] 

hist(
out[fuc,] 
)

hist(
out[mon,]
)

hist(
  inv_logit(out[fuc,]  -  out[mon,])
)

# this is beter
mean(inv_logit(out[fuc,]  -  out[mon,]))

# you can see there is a loooong left tail, is that good??

# how much of the distribution is below the betting oddS?

mean(
  inv_logit(out[fuc,]  -  out[mon,]) < (1/1.5)
)

# quite a lot!

## anyway, back to the Kelly bet 
## should we do this for other bets I wonder??

optimize(function(x) (-1)*expectedBankRoll(x, 
                                           p = mean(inv_logit(out[fuc,]  -  out[mon,])),
                                           b = 1.5), c(-0.5,1))

0.1364608*26.25

rank_dat[name %like% "Cecc"] 
rank_dat[name %like% "Alexander Z"] 

cecc <- rank_dat[, which(name %like% "Cecc")] 
zver <- rank_dat[, which(name %like% "Alexander Z")] 


optimize(function(x) (-1)*expectedBankRoll(x, 
                                           p = mean(inv_logit(out[zver,]  -  out[cecc,])),
                                           b = 1.44), c(-0.5,1))

optimize(function(x) (-1)*expectedBankRoll(x, 
                                           p = mean(inv_logit(out[cecc,]  -  out[zver,])),
                                           b = 2.75), c(-0.5,1))

gomb <- rank_dat[ ,which(name %like% "Gomb")] 
dieg <- rank_dat[, which(name %like% "Diego")] 



hist(inv_logit(out[gomb,]  -  out[dieg,]))
hist(inv_logit(out[fuc,]  -  out[mon,]))

# I think it's fair to say that for the first distribution, you do not want to bet as much
# as for the second!! 
bet <- 
sapply(1:4000, function(i){
  optimize(function(x) (-1)*expectedBankRoll(x, 
                                             p = inv_logit(out[gomb,i]  -  out[dieg,i]),
                                             b = 10), c(-0.5,1))$minimum
  })

mean(bet)
quantile(bet, 0.025)
quantile(bet, 1-0.025)

bet2 <- 
  sapply(1:4000, function(i){
    optimize(function(x) (-1)*expectedBankRoll(x, 
                                               p = inv_logit(out[fuc,i]  -  out[mon,i]),
                                               b = 1.5), c(-0.5,1))$minimum
  })

mean(bet2)
quantile(bet2, 0.025)
quantile(bet2, 1-0.025)

sd(bet)
sd(bet2)

mean(bet)
mean(bet2)


hist(bet2)

optimize(function(x) (-1)*expectedBankRoll(x, 
                                           p = mean(inv_logit(out[fuc,]  -  out[mon,])),
                                           b = 1.5), c(-0.5,1))$minimum

26.25*0.1364608


baut_ag <- rank_dat[, which(name %like% "Bautista")] 
balaz <-  rank_dat[, which(name %like% "Balaz")] 

optimize(function(x) (-1)*expectedBankRoll(x, 
                                           p = mean(inv_logit(out[baut_ag,]  -  out[balaz,])),
                                           b = 1.05), c(-0.5,1))$minimum

optimize(function(x) (-1)*expectedBankRoll(x, 
                                           p = mean(inv_logit(out[balaz,]  -  out[baut_ag,])),
                                           b = 8.5), c(-0.5,1))$minimum

optim(c(0.5), function(x) (-1)*expectedBankRoll(x, p, b))

all_dat[loser_name %like% "Bala"]
all_dat[winner_name %like% "Bala"]


## There is just not enough data on this guy is there.
## I think your model actually sucks doesn't it ... 

all_dat[loser_name %like% "Bauti"]
all_dat[winner_name %like% "Bauti"]


## thinking about Kelly betting now

## previously the functions was 
expectedBankRoll <- function(x, p, b){
  if(sum(x) > 1 | any(x<0)){return(-99999)}
  expBankRoll = p*log(1+(b-1)*x) + (1-p)*log(1-x)
  return(sum(expBankRoll))
}

## But now ... we want to integrate over the probability uncertainty when doing this! 

## What is the area under the curve defined by outcome*posterior probability 

## here is the posterior probability 
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
  
  mean_kelly <- 
    mean(sapply(1:50, function(i){
  optimize(function(x) (-1)*expectedBankRoll(x, 
                                             p = p[i],
                                             b = odds), c(-0.5,1))$minimum
  }))
  
  list("Kelly of the mean" = kelly_mean, 
       "Mean of the Kelly" = mean_kelly)
}

test_kelly("Fucsovics", "Monteiro", 1.5)
test_kelly("Bautist", "Balaz", 1.05)
test_kelly("Alexander Zv", "Cecc", 1.44)
test_kelly("Gomb", "Diego S", 1.44)



p_test_1 <- inv_logit(rnorm(100, 0,1))

optimize(function(x) (-1)*expectedBankRoll(x, 
                                             p = mean(p_test_1),
                                             b = 2), c(-0.5,1))$minimum

mean(sapply(1:100, function(i){
    optimize(function(x) (-1)*expectedBankRoll(x, 
                                               p = p_test_1[i],
                                               b = 2), c(-0.5,1))$minimum
  }))


p_test_2 <- inv_logit(rnorm(100, 0,5))

optimize(function(x) (-1)*expectedBankRoll(x, 
                                           p = mean(p_test_2),
                                           b = 2), c(-0.5,1))$minimum
mean(
sapply(1:100, function(i){
  optimize(function(x) (-1)*expectedBankRoll(x, 
                                             p = p_test_2[i],
                                             b = 2), c(-0.5,1))$minimum
}))

## Look maybe this is right: the kelly strategy over bets?



# So our Kelly betting has incresed with the standard deviation. Obviously wrong! 

## obviously not thinking straight!
test_kelly <- function(p1, p2, odds){
  pla1 <- rank_dat[ ,which(name %like% p1)] 
  pla2 <- rank_dat[ ,which(name %like% p2)] 
  
  if(any(length(pla1) > 1, length(pla2) >1)) stop("players not unique")
  
  ## probabilities 
  p <- inv_logit(out[pla1,]  -  out[pla2,])
  
  plot.new()
  par(mfrow=c(3,1))
  
  ## posterior denstiy
  plot(density(p))
  
  ## loss function
  ## is the optimized bank roll I think
  ## Nope!!! loss function is just the expected bank roll bit! 
  browser()
  xint <- seq(0,1,0.01)
  plot(xint, 
  
  sapply(xint, function(i){
    i*log(1+(b-1)*x) + (1-i)*log(1-x)
  }), type="l")
  
  ## posterior * loss function 
  p_dis <- prop.table(table(cut(p, length(xint))))
  
  
  loss_d <- sapply(xint, function(i){
    optimize(function(x) (-1)*expectedBankRoll(x, 
                                               p = i,
                                               b = odds), c(0,1))$minimum})
  
  plot(xint, p_dis*loss_d, type="l")
  
  kelly_mean <- 
    optimize(function(x) (-1)*expectedBankRoll(x, 
                                               p = mean(p),
                                               b = odds), c(-0.5,1))$minimum
  
  mean_kelly <- 
    mean(sapply(1:50, function(i){
      optimize(function(x) (-1)*expectedBankRoll(x, 
                                                 p = p[i],
                                                 b = odds), c(-0.5,1))$minimum
    }))
  
  list("Kelly of the mean" = kelly_mean, 
       "Mean of the Kelly" = mean_kelly)
}

test_kelly("Fucsovics", "Monteiro", 1.5)
