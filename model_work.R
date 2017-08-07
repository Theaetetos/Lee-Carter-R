# script for assembling data.frame with all elements necessary
# for Lee-Carter model

# import HMD dataset
# this (down to line 18) should be moved to its own module
data <- read.delim('usa_life_tbls.txt',
                   stringsAsFactors = F)

# TODO - handle missing data

# TODO - extend to age 120

# extract relevant data for LC model
work <- data.frame(data$Year, data$Age, data$mx)
colnames(work) <- c('year', 'age', 'mx')

# fix age field - change 110+ to 110 and convert to int
work$age <- as.integer(substr(work$age, 1, 3))

# method chosen - append df fields of a, k, and b

a <- function(x, df){
  #' takes as input age x and clean hmd df
  #' and returns a.x, i.e. first parameter of Lee-Carter
  #' Model for age x (average mortality rate for
  #' age x)
  # ========================================================
  # return mean mu.x,t for age x
  return(mean(subset(df, age == x)$mx))
}

append.ax <- function(df){
  #' takes as input clean hmd df
  #' and returns df with ax field appended
  #' age field must be named 'age'
  #' assumes all years have all ages present
  # ========================================================
  # call a() for each age in df
  ax.vect <- sapply(sort(unique(df$age)), a, df = df)
  # sort df for easy insertion of a.x values
  df <- df[order(df$age), ]
  # insert ax field by repeating each a.x as many times as years
  df$ax <- rep(ax.vect, each = length(unique(df$year)))
  return(df)
}

work <- append.ax(work)
work$logmx <- sapply(work$mx, log)

k <- function(t, df){
  #' takes as input time t and clean hmd df with ax and
  #' logmx fields added
  #' (fields year, age, mx, ax, logmx)
  #' and returns k.t (aka gamma.t), i.e. second parameter
  #' of Lee-Carter Model for time t (mortality improvement
  #' factor for time t)
  # ========================================================
  # calculate sum of differences between log.m.x,t values and
  # a.x values for year t
  temp <- subset(df, year == t)
  return(sum(temp$logmx - temp$ax))
}

append.kt <- function(df){
  #' takes as input clean hmd df with ax and
  #' logmx fields added
  #' (fields year, age, mx, ax, logmx)
  #' and returns df with kt field appended
  #' assumes all ages have all years present
  # ========================================================
  # call k() for each year in df
  kt.vect <- sapply(sort(unique(df$year)), k, df = df)
  # sort df by year so insertion of k.x values is simplified
  df <- df[order(df$year), ]
  # insert kx field by repeating each k.x as many times as ages
  df$kt <- rep(kt.vect, each = length(unique(df$age)))
  return(df)
}

work <- append.kt(work)

b <- function(x, df){
  #' takes as input clean hmd df with ax,
  #' logmx, and kt fields added
  #' (fields year, age, mx, ax, logmx, kt)
  #' and returns b.x, i.e. third parameter of
  #' Lee-Carter Model (sensitivty of age x to
  #' mortality improvement factor k.t)
  #' b.x is slope coefficient resulting from regression
  #' of log(mu.x,t) - a.x on k.t
  # ========================================================
  # subset df by age and necessary fields
  cols <- c('logmx', 'ax', 'kt')
  r.vars <- subset(df, age == x, cols)
  colnames(r.vars) <- cols
  # create field of dependendent variables for regression
  r.vars$y <- r.vars$logmx - r.vars$ax
  r.vars <- subset(r.vars, select = c('kt', 'y'))
  # perform regression and return slope coefficient
  # TODO - create regression pipeline (validate regression
  # assumptions, etc.) and call here
  regression <- lm(
    formula = y ~ kt,
    data = r.vars
  )
  return(as.numeric(coef(regression)["kt"]))
}

append.b <- function(df){
  #' takes as input clean hmd df with
  #' ax, logmx, and kt fields added
  #' (fields year, age, mx, ax, logmx, kt)
  #' and returns df with bx field appended
  # ========================================================
  # call b() for each age in df
  bx.vect <- sapply(sort(unique(df$age)), b, df = df)
  # sort df by age so insertion of b.x values is simplified
  df <- df[order(df$age), ]
  # insert bx field by repeating each b.x as many times as years
  df$bx <- rep(bx.vect, each = length(unique(df$year)))
  return(df)
}

work <- append.b(work)

# TODO - smooth bx?


# output prepped df
# TODO - move to separate module? Output to db?
write.table(work,
            file = 'lc_prepped.txt',
            sep = '\t',
            row.names = F)
