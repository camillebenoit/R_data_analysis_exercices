.libPaths("C:/Program Files (x86)/R/win-library/4.0")

# ----TASK 1----

my_function <- function(data, rows, columns){
  subset <- data[rows, columns, drop = FALSE]
  res <- list(subset)
  for (col in colnames(subset)){
    if (is.numeric(subset[[col]])){
      meancol <- list(mean(subset[[col]]))
      res <- c(res, meancol)
    } else {
      tablecol <- list(table(subset[[col]]))
      res <- c(res, tablecol)
    }
  }
  names(res) <- c("Subset", colnames(subset))
  res
}

my_function(iris, 101:150 , 2)

my_function(iris, c(1:25,55:65,110:122) , c(4,2,5))

# ----TASK 2----
my_new_function <- function(data, rows, columns, f, splt){
  subset <- data[rows, columns, drop = FALSE]
  spltcol <- data[rows, splt]
  concat_subset <- cbind(subset, spltcol)
  res <- list(subset)
  for (col in colnames(subset)){
    if (is.numeric(subset[[col]])){
      newres <- list(tapply(X = concat_subset[[col]], INDEX = spltcol, FUN = f))
    }else{
      newres <- list(tapply(X = concat_subset[[col]], INDEX = spltcol, FUN = table))
    }
    res <- c(res, newres)
  }
  names(res) <- c("Subset",colnames(subset))
  res
}

my_new_function(iris, 1:150, 2:4, sum, "Species")

my_new_function(iris, 1:150, 1:3, mean, 5)

my_new_function(iris, 6:23, c(1,2), max, 5)

irisnew <- iris
irisnew$garden <- gl(2, 75)
head(irisnew)
str(irisnew)

my_new_function(irisnew, 1:150, 3:5, mean, "garden" )
tapply(X = iris[,1:4], INDEX = iris[,"Species"], FUN = mean)

# ----TASK 4----

library(dplyr)

function_with_dplyr <- function(data, rows, columns, f, splt){
  subset <- data %>% 
    group_by(data[splt]) %>% 
    select(columns) %>% 
    slice(rows) 
  summa <- subset %>% 
    summarize_all(funs(if(is.numeric(.)) f(.) else table(.)))
  res <- list(subset, summa)
  res
}

function_with_dplyr(iris, 1:150, c(1,2,3), mean, "Species")

irisnew <- iris
irisnew$garden <- gl(2, 75)
function_with_dplyr(irisnew, 1:150, c(1,2,3,6), mean, "Species")


