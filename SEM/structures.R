psychstructsim <- function(df, n){
  let <- dim(df)[2]
  let <- let/n
  leter <- matrix(NA, dim(df)[1], 1)
  for (i in 1:let){leter <- rep(letters[i], let)}
  return(leter)
  # structure.sem(c(letters[1:let]))
}