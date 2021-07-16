#' A function that plots data frame into a line graph
#'
#' This function returns plot of data frame with 
#' specific color scheme of red, blue, or green.
#' @param c(df, color)
#' @keywords plot
#' @export
#' @examples
#' nicePlot()


nicePlot <- function(df, color)
{
  require(ggplot2)
  require(reshape2)
  require(colorspace)
  
  if (class(df) != "data.frame" | sum(color == c("red", "blue", "green")) != 1)
  {
    if (class(df) != "data.frame")
    {
      d <- "Please ensure that your data is in the form of a data frame with two or more columns where your first column is your independent variable."
    }
    else
    {
      d <- "Please ensure the color entered is either red, blue, or green"
    }
  }
  else 
  {
    if(color == "red")
    {
      cl <- "OrRd"
    }
    else if(color == "blue")
    {
      cl <- "BluYl"
    }
    else 
    {
      cl <- "Green-Yellow"
    }
    
    if(ncol(df) > 2)
    {
      n <- names(df)
      data <- melt(df, id.vars = n[1])
      l <- length(unique(data[,2]))
      clr <- sequential_hcl(l, cl)
      names(data) <- c("Col1", "Legend", "Col3")
      p <- ggplot(data, aes(x=Col1, y=Col3, color=Legend)) + geom_line(aes(x=Col1, y=Col3, color=Legend)) + xlab(n[1]) + ylab("Values") 
      d <- p + scale_fill_manual(values = clr)
      
    }
    else if (ncol(df) == 2)
    {
      names(df) <- c("r", "s")
      d <- ggplot(df, aes(x=r, y=s)) + geom_line(aes(x=r, y=r)) + theme(scale_fill_manual(values = sequential_hcl(l, cl)) )
    }
    else
    {
      d <- "Please ensure your data frame has either 2 or more columns with the first column being your independent variable."
    }
    
  }
  
  return(print(d))
}

