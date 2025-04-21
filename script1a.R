#install.packages('entropy')
library(entropy)

dev.new()
curve(dbeta(x, 10, 2), from = 0, to = 1, n = 2000, col = 'steelblue', lty = 'solid',
        ylab = 'Density')
curve(dbeta(x, 2, 10), from = 0, to = 1, n = 2000, col = 'green', lty = 'solid', add = TRUE)
curve(dbeta(x, 10, 10), from = 0, to = 1, n = 2000, col = 'orange', lty = 'solid', add = TRUE)
curve(dbeta(x, 5, 5), from = 0, to = 1, n = 2000, col = 'cadetblue', lty = 'solid', add = TRUE)
curve(dunif(x), from = 0, to = 1, n = 2000, col = 'red', lty = 'solid', add = TRUE)
grid(10,10)
legend('top', lty = c('solid', 'solid', 'solid', 'solid', 'solid'),
        col = c('steelblue', 'green', 'orange', 'cadetblue', 'red'),
        c('Skew Left', 'Skew Right', 'Symmetric1', 'Symmetric2', 'Uniform'), bty = 'n')

set.seed(2300)
x1 <- rbeta(500, 10, 2)
x2 <- rbeta(500, 2, 10)
x3a <- rbeta(500, 10, 10)
x3b <- rbeta(500, 5, 5)
x4 <- runif(500)

nbin = 20

y1 <- discretize(x1, nbin)
y2 <- discretize(x2, nbin)
y3a <- discretize(x3a, nbin)
y3b <- discretize(x3b, nbin)
y4 <- discretize(x4, nbin)

entropy(y1)
entropy(y2)
entropy(y3a)
entropy(y3b)
entropy(y4)

myentr <- function(a, b, n = 500, reps = 30, nbin = 20, fun = 'beta')
{
    if(fun == 'beta')
        vec <- rbeta(n * reps, a, b)
    else
        vec <- runif(n * reps)
    dim(vec) <- c(n, reps)
    yvec <- apply(vec, MARGIN = 2, FUN = function(x) discretize(x, nbin))
    entr <- apply(yvec, MARGIN = 2, FUN = function(x) entropy(x, unit = 'log2'))
    return(entr)
}

reps <- 30
set.seed(8192)
b1 <- myentr(10, 2)
b2 <- myentr(2, 10)
b3a <- myentr(10, 10)
b3b <- myentr(5,5)
b4 <- myentr(0,1, fun = 'unif')

mydf <- data.frame('Vals' = c(b1, b2, b3a, b3b, b4),
                    'Type' = c(rep('Left', reps), rep('Right', reps), rep('Symm1', reps),
                                    rep('Symm2', reps), rep('Unif', reps)))

dev.new()
boxplot(Vals ~ Type, mydf, horizontal = TRUE, xlab = 'Entropy (Less means more information (better))',
            main = 'Entropy Comparison',
            col = c('steelblue', 'green', 'orange', 'cadetblue', 'red'))
