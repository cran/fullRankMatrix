## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(fullRankMatrix)

# let's say we have 10 fruit salads and indicate which ingredients are present in each salad
strawberry <- c(1,1,1,1,0,0,0,0,0,0)
poppyseed <- c(0,0,0,0,1,1,1,0,0,0)
orange <- c(1,1,1,1,1,1,1,0,0,0)
pear <- c(0,0,0,1,0,0,0,1,1,1)
mint <- c(1,1,0,0,0,0,0,0,0,0)
apple <- c(0,0,0,0,0,0,1,1,1,1)

# let's pretend we know how each fruit influences the sweetness of a fruit salad
# in this case we say that strawberries and oranges have the biggest influence on sweetness
set.seed(30)
strawberry_sweet <- strawberry * rnorm(10, 4)
poppyseed_sweet <- poppyseed * rnorm(10, 0.1)
orange_sweet <- orange * rnorm(10, 5)
pear_sweet <- pear * rnorm(10, 0.5)
mint_sweet <- mint * rnorm(10, 1)
apple_sweet <- apple * rnorm(10, 2)

sweetness <- strawberry_sweet + poppyseed_sweet+ orange_sweet + pear_sweet +
  mint_sweet + apple_sweet 

mat <- cbind(strawberry,poppyseed,orange,pear,mint,apple)


## -----------------------------------------------------------------------------
caret_result <- caret::findLinearCombos(mat)

## -----------------------------------------------------------------------------
mat_caret <- mat[, -caret_result$remove]
fit <- lm(sweetness ~ mat_caret + 0)
print(summary(fit))

## -----------------------------------------------------------------------------
mat_weightit <- WeightIt::make_full_rank(mat, with.intercept = FALSE)
mat_weightit

## -----------------------------------------------------------------------------
fit <- lm(sweetness ~ mat_weightit + 0)
print(summary(fit))

## -----------------------------------------------------------------------------
plm::detect.lindep(mat)

## -----------------------------------------------------------------------------
c1 <- rbinom(10, 1, .4)
c2 <- 1-c1
c3 <- integer(10)
c4 <- c1
c5 <- 2*c2
c6 <- rbinom(10, 1, .8)
c7 <- c5+c6
mat_test <- as.matrix(data.frame(c1,c2,c3,c4,c5,c6,c7))

plm::detect.lindep(mat_test)

## -----------------------------------------------------------------------------
result <- make_full_rank_matrix(mat_test)
result$matrix

