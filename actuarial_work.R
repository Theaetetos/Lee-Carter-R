# script for actuarial calculations involved in interpreting
# Lee-Carter model results

# import prepped lc dataset
# TODO - move to separate module
lc <- read.delim('~/Coding/r/lee_carter/lc_prepped.txt')

q.const <- function(m.x.t){
  #' takes as input force of mortality for age x at time t and
  #' returns q.x,t, i.e. the probability of dying at age x in
  #' year t
  #' assumed formula: q.x,t = 1 - exp(-m.x,t) (constant
  #' force of mortality across integral ages)
  # ========================================================
  return(1 - exp(-m.x.t))
}

q.vect <- function(age.vect, t, df){
  #' takes as input vector of ages, year, and df and
  #' returns vector of q.x,t values calculated from
  #' mx values in df for ages in age.vect (incl) in year t
  # ========================================================
  # select m.x,t values from df in age.vect at year t
  return(sapply(subset(df,
                       df$age %in% age.vect & df$year == t
                       )$mx,
                q.const
                ))
}

p.const <- function(m.x.t){
  #' takes as input force of mortality for age x at time t and
  #' returns p.x,t, the probability of surviving age x in
  #' year t
  #' assumed formula: p.x,t = exp(-m.x,t) (constant
  #' force of mortality across integral ages)
  # ========================================================
  return(exp(-m.x.t))
}

p.vect <- function(age.vect, t, df){
  #' takes as input vector of ages, year, and df and
  #' returns vector of p.x,t values calculated from
  #' mx values in df for ages in age.vect (incl) in year t
  # ========================================================
  # select m.x,t values from df in age.vect at year t
  return(sapply(subset(df,
                       df$age %in% age.vect & df$year == t
                       )$mx,
                p.const
                ))
}

tp.const <- function(age.vect, t, df){
  #' takes as input vector of ages, year, and df and
  #' returns probability of surviving all ages in age vect in
  #' year t calculated from mx field in df
  #' constant force of mortality between integral ages assumed
  # ========================================================
  return(prod(p.vect(age.vect, t, df)))
}

tq.const <- function(age.vect, t, df){
  #' takes as input vector of ages, year, and df and
  #' returns probability of dying at some age in age vect in
  #' year t calculated from mx field in df
  #' constant force of mortality between integral ages assumed
  # ========================================================
  qx.vect <- q.vect(age.vect, t, df)
  px.vect <- p.vect(age.vect, t, df)
  age.min <- min(age.vect)
  for(i in 2:length(age.vect)){
    qx.vect[i] <- qx.vect[i] * tp.const(age.min:age.min + i, t, df)
  }
  return(sum(qx.vect))
}

e.compl <- function(x, t, df){
  #' takes as input age, year, and df and
  #' returns complete expected future lifetime of
  #' age x in year t
  #' uniform distribution of deaths between
  #' integral ages assumed
  # ========================================================
  return(.5 + e.curt(x, t, df))
}

e.curt <- function(x, t, df){
  #' takes as input age, year, and df and
  #' returns curtate expected future lifetime of
  #' age x in year t
  #' no assumption made about distribution of deaths between
  #' integral ages
  # ========================================================
  age.min <- min(df$age)
  age.max <- max(df$age)
  v <- vector(length = age.max - x)
  for(i in 1:length(v)){
    v[i] <- tp.const(x:(age.min + i - 1), t, df)
  }
  return(sum(v))
}