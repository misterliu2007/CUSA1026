# MLB team 1920 stats
#https://www.baseball-reference.com/leagues/majors/1920.shtml

# Babe Ruth stats
# https://www.baseball-reference.com/players/gl.fcgi?id=ruthba01&t=b&year=1920
mlb_1920 = read.csv("home_run_1920.csv")
head(mlb_1920)

mlb_1920_avg = mean(mlb_1920$HR)
print(mlb_1920_avg)

ruth = 54
print(ruth/mlb_1920_avg)

# MLB team 2022 stats
# https://www.baseball-reference.com/leagues/majors/2022.shtml
mlb_2022 = read.csv("home_run_2022.csv")
head(mlb_2022)

mlb_2022_avg = mean(mlb_2022$HR)
print(mlb_2022_avg)

print(mlb_2022_avg * ruth/mlb_1920_avg)

# Hall of Fame Receiver
# https://www.statmuse.com/nfl/ask/who-are-all-of-the-pro-football-hall-of-fame-wr-with-the-most-receiving-yards-in-their-careers
hall <- read.csv("hall_of_fame_rec.csv")
hall = hall[order(hall$REC),]
barplot(hall$REC)
barplot(hall$REC, names.arg=hall$NAME, 
        main="NFL Hall of Fame Wide Receivers",
        xlab="Number of Receptions",
        las=1, cex.names = .5, horiz=TRUE)

# Hat Trick
# https://en.wikipedia.org/wiki/List_of_FIFA_Women%27s_World_Cup_hat-tricks#:~:text=The%20first%20hat%2Dtrick%20was,2023%20FIFA%20Women's%20World%20Cup.
hat <- read.csv("hat_trick.csv")
hat = hat[order(hat$Time.of.Hat.Trick),]
hat = head(hat,10)
barplot(hat$Time.of.Hat.Trick)
barplot(hat$Time.of.Hat.Trick, names.arg=hat$Player, 
        xlab="Time to Hat Trick",
        las=1, cex.names = .45, horiz=TRUE)


# swimming record
# https://en.wikipedia.org/wiki/World_record_progression_1500_metres_freestyle
swim <- read.csv("swim_record.csv")
swim$Date <- as.Date(swim$Date, format="%B %d, %Y")
swim$duration <- diff(c(swim$Date,today()))
swim = swim[order(swim$duration, decreasing=TRUE),]
swim = head(swim,10)
barplot(as.numeric(swim$duration), names.arg=swim$Name, 
        xlab="The number of days remained unbeaten ",
        las=1, cex.names = .45, horiz=TRUE)

# one-on-one with steph curry
score = 0
{
  for(i in 1:13){
    shot = rbinom(1,1,0.5)
    score = score + shot
    cat(shot," ")
    Sys.sleep(1)
  }
  
  if(score > 11){
    cat("-> WON!\n")
  }else{
    cat("-> LOST!\n")
  }
}

# one-on-one with curry 1000 times!
won = 0
total_game = 1000

for(i in 1:total_game){
  
  result = rbinom(13,1,0.5)

  if(sum(result) > 11){
    cat(i," ", result, "-> WON!\n")
    won = won + 1
  }else{
    cat(i," ", result, "-> LOST!\n")
  }
  Sys.sleep(0.1)
}

print(paste0("You won ", won/total_game*100, "% of time."))

# Agassi vs Sampras

# Sampras stats:
# https://www.ultimatetennisstatistics.com/playerProfile?playerId=1948&tab=statistics

# Simulate Sampras
sampras_won = 0
agassi_won = 0

sim_itr = 100000

for(i in 1:sim_itr){
  
  sampras_pt = 0
  agassi_pt = 0
  
  while(TRUE){
    if(runif(1) < 0.5947){ # first serve in
      if(runif(1) < 0.8092){ # first serve won
        sampras_pt = sampras_pt + 1
      }else{
        agassi_pt = agassi_pt + 1
      }
    }else{ # first serve out
      if(runif(1) < 0.5261){ # second serve in and won
        sampras_pt = sampras_pt + 1
      }else{
        agassi_pt = agassi_pt + 1
      }
    }
    if(sampras_pt >= 3 & agassi_pt >=3){ # deuce
      if(sampras_pt - agassi_pt >= 2){
        sampras_won = sampras_won + 1
        break
      }else if(agassi_pt - sampras_pt >= 2){
        agassi_won = agassi_won + 1
        break
      }
    }else if(agassi_pt == 4){
      agassi_won = agassi_won + 1
      break
    }else if(sampras_pt == 4){
      sampras_won = sampras_won + 1
      break
    }
  }
}
sampras_percent = sampras_won/sim_itr*100
print(paste0("Sampras wins with probability ", sampras_percent, "%"))
print(paste0("Sampras wins six straight service games with probability ", (sampras_percent/100)^6*100, "%"))

# Agassi stats:
# https://www.ultimatetennisstatistics.com/playerProfile?playerId=1736&tab=statistics
sampras_won = 0
agassi_won = 0

sim_itr = 100000

for(i in 1:sim_itr){
  
  sampras_pt = 0
  agassi_pt = 0
  
  while(TRUE){
    if(runif(1) < 0.6262){ # first serve in
      if(runif(1) < 0.7293){ # first serve won
        agassi_pt = agassi_pt + 1
      }else{
        sampras_pt = sampras_pt + 1
      }
    }else{ # first serve out
      if(runif(1) < 0.5398){ # second serve in and won
        agassi_pt = agassi_pt + 1
      }else{
        sampras_pt = sampras_pt + 1
      }
    }
    
    if(sampras_pt >= 3 & agassi_pt >=3){ # deuce
      if(sampras_pt - agassi_pt >= 2){
        sampras_won = sampras_won + 1
        break
      }else if(agassi_pt - sampras_pt >= 2){
        agassi_won = agassi_won + 1
        break
      }
    }else if(agassi_pt == 4){
      agassi_won = agassi_won + 1
      break
    }else if(sampras_pt == 4){
      sampras_won = sampras_won + 1
      break
    }
  }
}

agassi_percent = agassi_won/sim_itr*100
print(paste0("Agassi wins with probability ", agassi_percent, "%"))
print(paste0("Agassi wins six straight service games with probability ", (agassi_percent/100)^6*100, "%"))

print(paste0("The probability of reaching 6-6 is ", (agassi_percent/100)^6 * (sampras_percent/100)^6 *100, "%"))

print(paste0("The probability of reaching 6-6 in 4 set is ", ((agassi_percent/100)^6 * (sampras_percent/100)^6)^4 *100, "%"))


# hot-hand phenomenon
observe = c(0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1)
n = length(observe)

measure = 0
total = 0
for(i in 4:n){
  if(sum(observe[(i-3):(i-1)])==3){
    total = total + 1
    if(observe[i]==1){
      measure = measure + 1
    }  
  }
}
measure = measure/total
print(measure)

# how often do we observe Curry's performance or even better?
sim_itr = 100000
result = rep(0,sim_itr)

for(j in 1:sim_itr){
  observe0 = sample(observe,n)
  measure0 = 0
  total0 = 0
  
  for(i in 4:n){
    if(sum(observe0[(i-3):(i-1)])==3){
      total0 = total0 + 1
      if(observe0[i]==1){
        measure0 = measure0 + 1
      }  
    }
  }

  if(total0>0)
    result[j] = measure0/total0
  print(result[j])
}
hist(result)
mean(result > measure)

# Poisson distribution
n = 10000
par(mfrow=c(2,3))

lambda = 0.2
barplot(table(rpois(n, lambda ))/n, main=paste("lambda=",lambda),ylim=c(0,1),xlim=c(0,10))
lambda = 0.4
barplot(table(rpois(n, lambda ))/n, main=paste("lambda=",lambda),ylim=c(0,1),xlim=c(0,10))
lambda = 0.6
barplot(table(rpois(n, lambda ))/n, main=paste("lambda=",lambda),ylim=c(0,1),xlim=c(0,10))
lambda = 0.8
barplot(table(rpois(n, lambda ))/n, main=paste("lambda=",lambda),ylim=c(0,1),xlim=c(0,10))
lambda = 1.0
barplot(table(rpois(n, lambda ))/n, main=paste("lambda=",lambda),ylim=c(0,1),xlim=c(0,10))
lambda = 1.2
barplot(table(rpois(n, lambda ))/n, main=paste("lambda=",lambda),ylim=c(0,1),xlim=c(0,10))

par(mfrow=c(1,1))

# Manchester performance
lambda = 1.75
manchaster = dpois(0:5, lambda)
        
# Arsenal performance
lambda = 1.77
arsenal = dpois(0:5, lambda)

match_prob = arsenal %*% t(manchaster)
print(match_prob)

# Arsenal winning probability
sum(match_prob[lower.tri(match_prob)])

# Manchester winning probability
sum(match_prob[upper.tri(match_prob)])

# Arsenal vs Manchaster tie probability
sum(diag(match_prob))
