#' Compute the average precision at k
#'
#' This function computes the average precision at k
#' between two sequences
#'
#' @param k max length of predicted sequence
#' @param actual ground truth set (vector)
#' @param predicted predicted sequence (vector)
#' @export
apk <- function(k, actual, predicted)
{
  score <- 0.0
  cnt <- 0.0
  if(length(predicted) == 0) {
    return(score)
  }
  for (i in 1:min(k,length(predicted)))
  {
    if (predicted[i] %in% actual && !(predicted[i] %in% predicted[0:(i-1)]))
    {
      cnt <- cnt + 1
      score <- score + cnt/i 
    }
  }
  score <- score / min(length(actual), k)
  score
}

#' Compute the mean average precision at k
#'
#' This function computes the mean average precision at k
#' of two lists of sequences.
#'
#' @param k max length of predicted sequence
#' @param actual list of ground truth sets (vectors)
#' @param predicted list of predicted sequences (vectors)
#' @export
mapk <- function (k, actual, predicted)
{
  if( length(actual)==0 || length(predicted)==0 ) 
  {
    return(0.0)
  }
  
  scores <- rep(0, length(actual))
  for (i in 1:length(scores))
  {
    scores[i] <- apk(k, actual[[i]], predicted[[i]])
  }
  score <- mean(scores)
  score
}

my.mapk <- function (k, actual, predicted)
{
  if( length(actual)==0 || length(predicted)==0 ) 
  {
    return(0.0)
  }
  
  scores <- rep(0, length(actual))
  for (i in 1:length(scores))
  {
    if (is.na(actual[i])) {
      scores[i] <- 0
    } else {
      scores[i] <- apk(k, strsplit(actual[i], split = " ")[[1]], strsplit(predicted[i], split = " ")[[1]])
    }
  }
  score <- mean(scores)
  score
}

hasNa <- function(seri) {
  any(is.na(seri))
}

fill.na <- function(seri) {
  if (hasNa(seri)) {
    seri <- as.numeric(as.character(seri))
    seri[is.na(seri)] <- mean(seri, na.rm = TRUE);
  }
  seri
}
# index <- which(sapply(cplall, FUN = hasNa))
# index.hasNa <- names(cplall)[index]

# consame.df <- function(d1, d2){
#   for (col in names(d1)) {
#     if(!all(d1[,col] == d2[,col])) {
#       return(FALSE)
#     }
#   }
#   return(TRUE)
# }
