## Function to choice the column name automatedly
colchoice <- function(x){
  as.factor(unique(colnames(x)))
}


## Log10 +1
logarithm10 <- function(x){
  log10(x+1)
}

## Biexponential

## Square-root +1
square_root <- function(x){
  sqrt(x+1)
}

## Logicle

## Arcsinh
arc_sin <- function(x){
  x + sqrt(x^2+1)
}

# Function to filter numeric columns
filter_numeric_columns <- function(df, column, operator, num) {
  if (operator == "==") {
    return(df %>% filter(.data[[column]] == num))
  } else if (operator == ">") {
    return(df %>% filter(.data[[column]] > num))
  } else if (operator == "<") {
    return(df %>% filter(.data[[column]] < num))
  } else if (operator == ">=") {
    return(df %>% filter(.data[[column]] >= num))
  } else {
    return(df %>% filter(.data[[column]] <= num))
  }
}