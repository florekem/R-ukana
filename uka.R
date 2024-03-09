library(tidyverse)

# lexical scoping in R
# wartość y bierze z global env. nie z funkcji wywoływanej.
# jak w pythonie
# chyba ze funkcja jest w funkcji, wtedy wartosc bierze z funkcji
# która jest wyżej.

y <- 10

f <- function(x) {
  y <- 2
  y^2 + g(x)
}

g <- function(x) {
  x*y
}

f(3)


cube <- function(x,n) {
  x^3
}

cube(3)


f <- function(x) {
  g <- function(y) {
    y+z
  }
  z <- 4
  x + g(x)
}
z <- 10
f(3)

x <- 5
y <- if(x<3) {
  NA
} else {
  10
}

##############################
# Week2 assignment part 1

library(tidyverse)

setwd("~/Pobrane/rprog_data_specdata/")

pollutantmean <- function(directory, polutant, id) {
  
  id.ma <- matrix(id)
  total <- numeric()
  for (i in id.ma) {
    if (i < 10) {
      path <- paste(directory, '00', i, ".csv", sep='')
    } else if (i < 100) {
      path <- paste(directory, '0', i, ".csv", sep='')
    } else {
      path <- paste(directory, i, ".csv", sep='')
    } 
    file <- read.csv(path)
    file <- file[,polutant]
    total <- append(total, file)
  }
  total.wo.na <- total[!is.na(total)]
  print(mean(total.wo.na))
}


pollutantmean("specdata/", 'nitrate', 3)

# Week2 assignmetn part 2
library(tidyverse)

setwd("~/Pobrane/rprog_data_specdata/")

complete <- function(directory, id) {
  result.df <- data.frame('id'=numeric(), 'no.obs'=numeric()) 
   
  id.ma <- matrix(id)
  for (i in id.ma) {
    if (i < 10) {
      path <- paste(directory, '00', i, ".csv", sep='')
    } else if (i < 100) {
      path <- paste(directory, '0', i, ".csv", sep='')
    } else {
      path <- paste(directory, i, ".csv", sep='')
    } 
    file <- read.csv(path) %>% 
      drop_na()
    no.obs <- nrow(file)
    
    result.df[nrow(result.df) + 1,] <- c(i, no.obs)
    #result.df['no.obs'] <- no.obs
    #result.df['id'] <- i
    
  }
  print(result.df)
}

complete("specdata/", c(2,4,8,10,12))

?nrow



# week 3 mapply() tapply() split()

mapply(rep, 1:4, 4)
mapply(rep, 1:4, 4:1)
mapply(rep, c(1,2,3,4), c(4,3,2,1))
              #arg1         arg2      rep(arg1, arg2)


?gl
f <- gl(3, 10)
x <- c(1:10, 11:20, 21:30)
tapply(x, f, mean)


x <- rnorm(10)
f1 <- gl(2,5)
f2 <- gl(5,2)
f1
f2
interaction <- interaction(f1, f2) # combine factors! # fct_c() | fct_cross()
split(x, list(f1,f2), drop=TRUE) # is the same as:
split(x, interaction, drop=TRUE)
sapply(split(x, interaction, drop=TRUE), mean)

#
by_age <- gss_cat |>
  filter(!is.na(age)) |> 
  count(age, marital) |> 
  group_by(age) |> # prop wyliczana dla sumy kat. wieku
  mutate(
    prop = n / sum(n)
  )

ggplot(by_age, aes(x = age, y = prop, color = fct_reorder2(marital, age, prop))) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") + 
  labs(color = "marital")

gss_cat |> count(partyid)


l <- list(
  a = 1:3, 
  b = "a string", 
  c = pi, 
  d = list(-1, -5)
)

l[1]
l[[1]]
str(l[1])
str(l[[1]])
l[[1]][1]
l[[1]][[1]]










