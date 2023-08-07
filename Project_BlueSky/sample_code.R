## Data types in R ------------------

## character
## integer
## numeric
## Logical
## complex

x <- "dataset"
x2 <- 'dataset'
typeof(x)
typeof(x2)
str(x2)
x3 <- 'a'
y <- 1
typeof(y)
y <- 1L
y2 <- 10L
typeof(y)
typeof(y2)
y3 <- 1.2
typeof(y3)
str(y3)
str(y)
z1 <- TRUE
z2 <- FALSE
typeof(z1)
typeof(z2)


v1 <- 1 + 2i
typeof(v1)
str(v1)

##########################################################################

## data structure in R

## 1. vectors
## 2. lists
## 3. Matrix
## 4. array
## 5. dataframe

### Homogeneous 
#### 1. vectors
#### 2. Matrix
#### 3. Array

#### vectors -----------------------------

x <- c(1L, 2L, 3L)
x1 <- 1:3
x2 <- 1.2:3.2
x3 <- seq(1, 3, by = 1)
x4 <- numeric(3)
x5 <- integer(3)
x6 <- character(3)
x7 <- logical(3)
x7[2] <- TRUE
x4
x4[3] <- 5
x4

x8 <- c(F, T, F, T, F, T)
x8
x8[4] <- F
x8

## coercion

U <- c(F)
U
typeof(U)
U[2] <- 2
typeof(U)
U
U[3] <- T
U

U[4] <- "a"
str(U)


V <- c(1, 2, 3, 4, 5)
names(V) <- c("a", "b", "c", "d", "e")
V
letters[1:5]
LETTERS[1:5]

rm(V, U)
U
V
rm(list = ls())


############################

V = setNames(V, LETTERS[1:5])
V <- c(a1 = 1, a2 = 2, a3 = 3, a4 = 4, a5 = 5)
V
names(V)


is.atomic(V)
is.vector(V)
attr(V, "att1") <- "habib"
V
is.atomic(V)
is.vector(V)
#############################################


## operators in R

1 + 2
2-1
1 /2
1*2
2^2


3 %% 2

5 %/% 2


7 %in% V

3 %in% V

#####################
## logical operators


1 < 2
2 < 1
1 <= 2
2 <= 1
1 > 2
2 > 1
1 >= 2
2 >= 1
x <- 2
y <- 3

x > 3
y < 4
(x > 3) | (y < 4)
(x > 3) | (y < -2)

(x == 2) & (y == 3)

(x == 1) & (y == 3)

(x == 1) | (y == 3)

xor((x == 2), (y == 4))

! x == 2

x != 3

###########################################

length(V) ## number of elements of a vector

Sys.time()
Sys.Date()
R.version


print(V)
V

##############################

x <- c(1, 2, 3, 4, 5)
y <- c(1, 2, 3)



temp1 <- x + y
temp1


temp2 <- x + 1
temp2
x2 <- c(letters[1:3])
x2
x3 <- c(letters[4:6])
x3
temp3 <- c(x2, x3)
temp3

x[x > 3]
x > 3
x[c(1, 2)]
x[c(3, 4)]
x <- c("a", "b", "c")
x[1:2]
#################################

## as.numeric, as.character, as.logical, as.vector
x <- c("1", "2", "3", "a")
str(x)
as.numeric(x)
as.logical(x)

############################################################

## matrix --------------------------------------


x <- matrix(1, nrow = 3, ncol = 3)
x2 <- matrix(c(1, 1, 1), nrow = 3, ncol = 3)
x2
x3 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3, byrow = T)
x3

dimnames(x) = list(rows = letters[1:3], cols = LETTERS[1:3])
x

x3[1, 2]
x3[3, 2]
x3[2, ]
x3[, 2]

x3[x3[, 3] > 4, 3]

#############################################################

x1 <- c(1, 2, 3)
x2 <- c(-1, -2, -3)
x4 <- cbind(x1, x2)
str(x4)

x4 <- rbind(x1, x2)
x4
x4
x4[1, 2] <- "a"
x4

x3 <- matrix(c(1, 2, 3, 4), 2, 2)
x3
x4 <- matrix(c(1, 2, 3, 4, 5, 6), 2, 3)
x4
x3 %*% x4
x4 %*% x3

#############################################

A <- matrix(1, 2, 4)

B <- matrix(2, 4, 4)

C <- rbind(A, B)

D1 <- matrix(-1, 5, 2)
D2 <- matrix(-2, 5, 5)
D1
D2
D3 <- cbind(D1, D2)
D3

A <- matrix(1:9, 3, 3)
A
A_inverse <- solve(A)
A <- matrix(c(-1, 2:9), 3, 3)
A_inverse <- solve(A)
A_inverse
A %*% A_inverse

########################################

library(MASS)

A2 <- matrix(c(1, 2, 3, 4, -2, 3, 4, -3), 2, 4)
A2
A3 <- ginv(A2) 
A3
A2 %*% A3 %*% A2
############################################

E <- eigen(A)
Lambda <- E$values
Vectors <- E$vectors
Lambda
Vectors
qr(A)$rank

AA <- matrix(1:9, 3, 3)
qr(AA)$rank

dim(AA)
dim(A2)

AA
t(AA)

#############################################

print(A)

############## cat command---------------
x1 <- 2
x2 <- 3
x3 <- 4
x4 <- -3
x5 <- 1:3
x6 <- matrix(1:4, 2, 2)
cat("x1 = ", x1, "\n", 
    "x2 = ", x2, "\n", 
    "x3 = ", x3, "\n", 
    "x4 = ", x4, "\n", 
    "x5 = ", x5, "\n", 
    "x6 = ", x6, "\n", sep = "")


########### sprintf

x1 <- 1.234
x2 <- "habib"
x3 <- 2
sprintf("%s meqdar adade x1 ra barabar ba %.2f bedast avarde, 
        tedad tekrarha baraye in mohasebat %i bude hast", x2, x1, x3)

?sprintf

x1 <- "a"; x2 <- 1; x3 <- "b"; x4 <- 2

paste(x1, x2, sep = "")
paste(x3, x4, sep = "-")
x1 <- 1:5; x2 <- "a"
paste(x2, x1, sep = ":")
## y(1), y(2), y(3), y(4)
paste(paste("y(", 1:4, sep = ""), ")", sep = "")
xx <- c("h", "a", "b", "i", "b")
paste(xx, collapse = "")

## dataframe-----------------------------------------

a1 <- data.frame(c1 = 1:10, 
                 c2 = letters[1:10], 
                 c3 = c(F, T, T, F, T, F, T, F, T, T))
print(a1)
class(a1)
str(a1)
dim(a1)

a1$c1
a1$c2
a1$c3
attach(a1)
c1
detach(a1)
c1
a1[, 1]
a1[, 2]
names(a1)
d2 <- a1['c1']
d1 <- a1$c1
d3 <- a1[, 1]
d4 <- a1[['c1']]

c4 <- seq(10, 100, len = 10)
c4
a1$c4 <- c4
a1[['c5']] <- c4
a1[, 'c6'] <- c4
a1
a1$c6 <- NULL
a1[, 'c5'] <- NULL
a1
a1[1:3, ]
a1[1:3, 'c2']
a1[1:3, c('c1', 'c2')]

a1[a1$c1 > 4, 'c4']
a1[!a1$c1 > 4, ]

### subset

subset(a1, select = c(c1, c3))

a2 <- subset(a1, subset = c1 > 4 & c3 == T)
a2
str(a2)

## assign operator

x = 2
x
y <- 2
2 -> z
z
z <- 2
z
z = 2
z
2 -> z
z

z <<- 2
z

###########################################

# Rcourse session 8 --------------------------------------

## lists -----------------------------------

list_1 <- list(1, 2, 3, 4)
str(list_1)
x1 <- data.frame(a1 = 1:4, a2 = letters[1:4], 
                 a3 = rep(c(T, F), each = 2))
x1
x2 <- matrix(NA, 4, 3)
x2

list1 <- list(matrixx = x2, dataframm = x1, 
              vectorr = 1:10)
str(list1)

list1

object1 <- list1$dataframm
object1

object2 <- list1[['matrixx']]
object2

object3 <- list1["matrixx"]
object3


list1[1]

list1$vectorr <- NULL
list1

list1$new_object = data.frame(x1 = rep(1, 4), x2 = rep(2, 4), 
                              x3 = LETTERS[1:4])
list1


list1[['newobject2']] <- matrix(1, 2, 2)
list1

append(list1, c(1, 2, 3, 4))


length(list1)
list1[[4]][1, 1]

lengths(list1)

f <- function(x) x + 1
list1$f = f
list1



######################################################

## factors

y1 <- factor(rep(letters[1:4], each = 5), levels = letters[1:4])
y1


summary(y1)


y2 <- rep(letters[1:4], each = 5)
summary(y2)


table(y2)


y3 <- factor(1:4, ordered = TRUE)
y3

y4 <- factor(c(5, 2, 3, 1, 7), ordered = T)
y4

y5 <- factor(c("sad", "happy", "veryhappy", "neutral"), ordered = T)
y5


y5 <- factor(c("sad", "happy", "veryhappy", "neutral"), 
             levels = c("sad", "neutral", "happy", 
                        "veryhappy"), ordered = T)
y5



## session IX


### functions ---------------------------

f <- function(x) x + 1

#### formals
#### body
#### enviroment

formals(f)

body(f)

environment(f)

### (1) prefix
### (2) infix 

g <- function(x, y) {
  a <- x/2
  b <- y/2
  c <- a * b
  return(c)
}
formals(g)
body(g)
environment(g)

g(x = 2, y = 3)

#### "%*%"

A <- matrix(1, 2, 2)
B <- matrix(2, 2, 2)
C1 <- A %*% B


C2 <- `%*%`(A, B)
C1
C2

1 + 2
`+`(1, 2)

`%++%` <- function(x, y) paste(x, y, sep = " ")
"habib" %++% "R"

`%++%`("habib", "R")

x <- 1:10
x
x[2]
`[`(x, 2)


g2 <- function(x1, x2, x3) {
  a <- log(x1)
  b <- exp(x2)
  d <- sqrt(x3)
  return(a + b + d)
}
g2(1, 2, 1)

g2 <- function(x1 = 1, x2 = 2, x3 = 1) {
  a <- log(x1)
  b <- exp(x2)
  d <- sqrt(x3)
  return(a + b + d)
}

g2()

g2(x1 = 10)
g3 <- function() {
  x <- 10
  y <- x * 2
  return(exp(y) * log(x))

}
g3()

body(sum)


## session X ----------------------------------

# read.table()

dat <- read.table(file.choose(), header = TRUE)
head(dat)

dat2 <- read.csv(file.choose(), header = TRUE)
head(dat2)

# install.packages("readxl")
library(readxl)

dat3 <- read_xlsx(file.choose(), col_names = TRUE)

head(dat3)

library(foreign)
dat4 <- read.spss(file.choose(), to.data.frame = TRUE)

head(dat4)

class(dat3)
class(dat2)
class(dat4)
class(dat)

########################################

## session XI


## rm(), ls() 


x1 <- 1
x2 <- 2
f <- function(x, y) x + y
g <- list(x = 1, y = 2)
d <- data.frame(x = 1, y = 2)
ls()

rm("x1")
rm("x2")
ls()
rm("f", "g")
ls()

rm(list = ls())
ls()


## attach

dat <- data.frame(x1 = c(1, 2, 3), X2 = LETTERS[1:3])
dat
x1
attach(dat)
x1
X2

# letters
# LETTERS

dat2 <- list(v1 = c(1, 2, 3), v2 = letters[1:3])
v1
attach(dat2)
v1
v2

f <- function(x) {
  d <- 2
  x + d
}
d
v1
v2

detach(dat2)
v1

x1
detach(dat)
x1

# replace

x <- c(1, 2, 3, NA, -1, -3)

## which

ind <- which(x < 0)
ind

x2 <- replace(x, ind, NA)
x2




##################################################################################
## session XII ----------------------------

library(ggplot2)

search()


detach("package:ggplot2", unload = TRUE)
search()

# dat <- data.frame(x = rnorm(20), y = rnorm(20))
# ggplot(dat, aes(x, y)) + geom_point()

x1 <- c(1, -2, 3, 4, 10)
y1 <- c(2, -3, 4, -4, 5)

min(x1); max(x1)
# pmin
# pmax

pmin(x1, y1)
pmax(x1, y1)

sum(x1)
cumsum(x1)
prod(x1)
cumprod(x1)
cummin(x1)
cummax(x1)

rev(x1)
x1

## save objects in R 

### format 1 --> RDA
set.seed(123)
x <- data.frame(x = rnorm(10, 2, 2), y = rbinom(10, 1, 0.5))
y <- matrix(NA, 5, 5)

save(x, y, file = "temp1.RDA")

rm(list = ls())
x
y

# zz <- load(file = "temp1.RDA")
# zz
# x
# y


### format 2 --> RDS

x
y


saveRDS(x, file = "temp2.Rds")


rm(list = ls())
x
z <- readRDS("temp2.Rds")
z

######################################################


## session 13 ------------------------------

### (a) apply
### (b) sapply
### (c) lapply
### (d) vapply
### (e) tapply
### (f) mapply


dat13 <- data.frame(x = c(1, 2, 3, 4, 5, -1, -2, -3, -4, -5), 
fac1 = rep(1:2, each = 5), fac2 = rep(c(1, 2, 3), c(3, 3, 4)))
dat13

## apply
apply(dat13, MARGIN = 1, FUN = median)

## sapply
res <- sapply(dat13, FUN = mean, simplify = FALSE)
res
res2 <- vapply(dat13, FUN = fivenum, FUN.VALUE = numeric(5))

## lapply
res3 <- lapply(dat13, mean)
class(res3)

res4 <- lapply(1:3, function(x) rnorm(x, mean = 2, sd = 2))

## tapply
tapply(dat13$x, INDEX = list(dat13$fac1, dat13$fac2), FUN = sum)


## mapply

f1 <- function(x, y, z) x + y + z

mapply(f1, c(1, -2, 3, 4), c(2, 3, 1, -2), c(2, 1, -5, -10))


## replicate

fun <- function() rnorm(5, 1, 1)
fun()
fun()

replicate(10, fun(), simplify = "array")

## any

## all

## identical


## any
x <- c(-1, 2, 3, 2)

cond <- x > 2
any(cond)

## all
all(cond)
all(x > -2)

## identical, all.equal

x <- 1
y <- 1.000
identical(x, y)
x == y

x <- c(1, 2, 3, 4)
y <- c(1.00, 2.00, 3, 4)

identical(x, y)
all(x == y)

all.equal(x, y)

identical(1, NULL)
1 == NULL
all.equal(1, NULL)

f1 <- function(x) mean(x)

f2 <- function(y) mean(y)

identical(f1, f2)
all.equal(f1, f2)


#####################################################

## session 14 

Sys.Date()

x <- "05/07/2023"

xdate <- as.Date(x, format = "%d/%m/%Y")
class(xdate)
class(x)

x1 <- "05-07-2023"
xdate1 <- as.Date(x1, format = "%d-%m-%Y")
xdate1

x2 <- "2023/10/30"
xdate2 <- as.Date(x2, format = "%Y/%m/%d")
xdate2
class(x2)
class(xdate2)

months(xdate, abbreviate = TRUE)
weekdays(xdate)

month.abb

x4 <- "May 20 1991"

xdate3 <- as.Date(x4, format = "%B %d %Y")
xdate3

x5 <- "jul 15 20"
xdate4 <- as.Date(x5, format = "%B %d %y")
xdate4

x6 <- 1900

Origin = "1945-07-10"

xdate6 <- as.Date(x6, Origin = Origin)

temp1 <- format(xdate4, "%w")
class(temp1)
temp1
weekdays(xdate4) 

## |>:   f(x) ---> use native pipe --> x |> f()
## %>% f(x) --> use pipe --> x %>% f

temp1 |> as.numeric()
temp1

###############

xdate6
xdate4

difftime(xdate4, xdate6, units = "weeks") |> as.numeric()


library(lubridate)

date1 <- lubridate :: make_date(year = 1977:2023, 
month = c(11, 11, rep(1:12, each = 4)), day = c(1:28, 1:18))
class(date1)

date2 <- seq(ymd("1977-01-01"), ymd("2023-01-01"), 
          by = "2 years")
date2

#################################

set.seed(1)
dat14 <- data.frame(x1 = rnorm(10, mean = 2, sd = 2) |> round(2), 
          x2 = rnorm(10, mean = 10, sd = 3) |> round(2), 
          x3 = rnorm(10, mean = 5, sd = 7) |> round(2), 
          x4 = factor(rep(c("female", "male"), c(5, 5))), 
          y = rbinom(10, size = 1, prob = 0.6))
dat14

write.csv(dat14, file = "data_csv_session_14.csv")
library(writexl)

write_xlsx(dat14, path = "data_excel_session_14.xlsx")



#########################################################

## session 15 --------------------------------------------


#  match

x <- c(1, 2, 3, -10, 4, -5)
y <- c(7, 2, 1, 1, 1)

match(x, y)


#################################################

# with, within

library(readxl)
dat <- read_xlsx(file.choose(), col_names = TRUE)
dat
x1
dat$x1
attach(dat)
x1
x2
detach(dat)
x2
v <- 3
temp1 <- with(dat, plot(x1, x2, cex = 5, col = "red", pch = 16))


dat2 <- within(dat, xx <- x1 * x2)
dat2
dat

within(dat, plot(x1, x2, pch = 16, cex = 5))
with(dat, x1 + x2)
within(dat, print(x1 + x2))


##############################################################

search()

temp3 <- installed.packages()[, 1]
attributes(temp3)
class(temp3)
dim(temp3)
colnames(temp3)
temp3[, 1]
temp4 <- installed.packages()[, 1]
temp4
temp3[, 16]
packageVersion("tidyverse")

remove.packages("tt")
## install.packages("tt")
##########################################

## read stata file  and sas

library(readstata13)
dat_stata <- read.dta13(file.choose())

class(dat_stata)
head(dat_stata)

lapply(dat_stata, class)

dat_stata$x3[2]

library(haven)

temp_6 <- read_sas(file.choose())
temp_6

##
read_stata(file.choose())
read_sav(file.choose())



#######################################################


f1 <- function(x) {
  a <- ifelse(x > 0, log(x), x ** 2)
  return(a)
}
f1(-2)
curve(f1, from = -2, to = 2, lwd = 2, col = "red")

xx <- c(-2, -1, 1, 1, 2)
f1(xx)


f1 <- function(x) {
  if(x > 0) log(x) else x ** 2
}
f1(xx)

f2 <- Vectorize(f1)
f2(xx)

integrate(f1, -2, 2)$value
curve(f1, from = -2, to = 2, lwd = 2, col = "red")


integrate(f2, -2, 2)$value
curve(f2, from = -2, to = 2, lwd = 2, col = "red")


