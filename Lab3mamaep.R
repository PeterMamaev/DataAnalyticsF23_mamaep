# Creating a matrix data with randomly selected numbers with no pattern.
# image() function plots the matrix.
# set.seed creates random numbers each time a random function is called.
set.seed(12345)
help(par)
# Part can be used to set or query graphical parameters
# parameters can be set by specifying them as arguments to par in tag=value form, or by passing them as a list of gagged values
par(mar = rep(0.2,4))
data_matrix <- matrix(rnorm(400), nrow = 40)
# Establishes heatmap image of the actual data matrix - as intended, no real pattern to it.
image(1:10, 1:40, t(data_matrix)[,nrow(data_matrix):1])
# The heatmap() function available by default in R does a hierarchal analysis of the dataset.
help("heatmap")
# The rep function replicates values in x.
help(rep)
par(mar = rep(0.2, 4))
# No useful pattern generated yet.
heatmap(data_matrix)

# Random coin flip data.
# rbinom() function along a for-loop
# rbinom is a binomial distribution function
help("rbinom")
# Range of values.
set.seed(678910)
for (i in 1:40) {
  # Getting data from flipping the coin.
  coin_FLIP <- rbinom(1, size=1, prob=0.5)
  # Loop through all rows, flip a coin on a random row.
  # During the coin flip, a "true" is added a pattern to the data so that one column has a mean of zero and others have a mean of three.
  # If the coin is "Heads", add a common pattern to that row.
  if (coin_FLIP) {
    data_matrix[i, ] <- data_matrix[i, ] + rep(c(0,3), each = 5)
  }
}

# Plotting the data, higher value left hand five columns should be more red in color.
par(mar = rep(0.2, 4))
image(1:10, 1:40, t(data_matrix)[, nrow(data_matrix):1])

# Two sets of columns are easily separated.
# Within the dendrograms, data has splits into two separate clusters on both sides.
par(mar = rep(0.2, 4))
heatmap(data_matrix)

# Marginal means of the rows and columns
# Ten different columns (split into five in each of the two clusters) 

# Hierarchical cluster analysis on a set of dissimilarities.
help(hclust)
hh <- hclust(dist(data_matrix))
hh
data_matrix_ordered <- data_matrix[hh$order,]
# The mfrow and mfcol parameters create a matrix of plots in one plotting space.
par(mfrow = c(1, 3))
# Formats the ordered image.
image(t(data_matrix_ordered)[, nrow(data_matrix_ordered):1])
plot(rowMeans(data_matrix_ordered), 40:1, , xlab = "The Row Mean", ylab = "Row", pch=19)
plot(colMeans(data_matrix_ordered), xlab = "Column", ylab = "The Column Mean", pch = 19)

