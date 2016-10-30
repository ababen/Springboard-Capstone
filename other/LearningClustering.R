x = c(0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0)
y = c(0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0)

## Using the Distance Matrix Computatuin packacge dist

dist(rbind(x, y), method = "euclidean")

# dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

#as.dist(m, diag = FALSE, upper = FALSE)
### Default S3 method:
#as.dist(m, diag = FALSE, upper = FALSE)

### S3 method for class 'dist'
#print(x, diag = NULL, upper = NULL,
#      digits = getOption("digits"), justify = "none",
#      right = TRUE, ...)

### S3 method for class 'dist'
#as.matrix(x, ...)

# Reminder for myself to build a function or figure out how to make a shortcut for this.

#  Using the zscore function, you can convert all the values in the data set to use the same proportional scale. See zscore for more information.
## Hierarchical Clustering
# https://www.mathworks.com/help/stats/hierarchical-clustering.html?requestedDomain=www.mathworks.com