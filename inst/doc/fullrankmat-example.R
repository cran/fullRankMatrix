## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, out.width="20%", fig.align = 'left'--------------------------
knitr::include_graphics("man/figures/fullRankMatrix.png")

## ----citation-----------------------------------------------------------------
citation("fullRankMatrix")

## ----setup--------------------------------------------------------------------
library(fullRankMatrix)

## -----------------------------------------------------------------------------
c1 <- c(1, 0, 1)
c2 <- c(1, 2, 0)
c3 <- c(2, 4, 0)

mat <- cbind(c1, c2, c3)

make_full_rank_matrix(mat)

## ----out.width="100%", fig.cap="Visualisation of identifying and replacing linearly dependent columns."----
knitr::include_graphics("man/figures/example_vectors.png")

## -----------------------------------------------------------------------------
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

fit <- lm(sweetness ~ mat + 0)
print(summary(fit))

## -----------------------------------------------------------------------------
mat

## -----------------------------------------------------------------------------
library(fullRankMatrix)
result <- make_full_rank_matrix(mat)
mat_fr <- result$matrix
space_list <- result$space_list
mat_fr

## -----------------------------------------------------------------------------
fit <- lm(sweetness ~ mat_fr + 0)
print(summary(fit))

## -----------------------------------------------------------------------------
space_list

