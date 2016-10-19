x = c(0,1,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0)
y = c(0,1,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0)

## Using the Distance Matrix Computatuin packacge dist

dist(rbind(x, y), method = "euclidean")

dist(x, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

as.dist(m, diag = FALSE, upper = FALSE)
## Default S3 method:
as.dist(m, diag = FALSE, upper = FALSE)

## S3 method for class 'dist'
print(x, diag = NULL, upper = NULL,
      digits = getOption("digits"), justify = "none",
      right = TRUE, ...)

## S3 method for class 'dist'
as.matrix(x, ...)