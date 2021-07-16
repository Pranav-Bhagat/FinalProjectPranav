#' A shuffling Function so that all numbers from 1 through n are shuffled
#'
#' This function returns a shuffling of n numbers 
#' in a random order so that the number does not 
#' match the placement position 
#' @param n A whole number greater than 0
#' @keywords shuffling
#' @export
#' @examples
#' derange()

derange<-function(n)
{
  if(n < 0)
  {
    d <- "Derange can only handle natural numbers (i.e. n > 0)"
  }
  else
  {
    c=0
    while( c==0 )
    {
      d <- sample(n)
      if(sum(d==1:n)==0)
      {
        c=1
      }
    }
  }
  return(d)
}