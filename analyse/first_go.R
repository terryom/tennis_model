library(data.table)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

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

alpha_hat <- rep(NA, length(all_players))
for (k in 1:K)
  alpha_hat[k] <- mean(extract(individual_posterior)$alpha[ , k])

ggplot(data.frame(alpha_hat = alpha_hat),
       aes(x =  alpha_hat)) +
  geom_density() + 
  ggtheme_tufte()

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

## should really learn regex for this 
inv_logit(
  rank_dat[name %like% "Alexander Zverev", alpha_mean] - 
    rank_dat[name %like% "Herbert", alpha_mean]
)

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







