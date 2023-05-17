#' A rotatogram is a method of displaying an association which is axis non-dominant. This is achieved in two ways: First, the method of estimating the slope and intercept uses the least-products method rather than more typical least squared error for the "dependent" variable. The least products method has no "dependent" variable and is scale independent. Second, the plot is rotated such that the resulting regression line is vertical, reducing the suggestion that the vertical axis is the dominant one. The slope can be read relative to either axis equally.
#' @title Rotatogram
#' @name rotatogram
#' @keywords association
#' @param x1 (required) A vector containing the data for the x1 variable
#' @param x2 (required) A vector containing the data for the x2 variable
#' @param x1.label (optional) Changes the x1 axis label to the specified name
#' @param x2.label (optional) Changes the x2 axis label to the specified name
#' @param suppress.plot (optional) Suppresses the plot output
#' @param return.values (optional) Exports the stored data for later access (e.g. slope and intercept calculated)
#' @return description The resulting object contains the resulting intercept ($intercept), slope/beta ($beta), the slope in degrees ($slope.degrees)
#' @import ggplot2 stats grid
#' @export

#' @examples
#' # Output a rotatogram using the iris dataset
#' rotatogram(iris$Sepal.Length,iris$Petal.Length,x1.label="Sepal length",x2.label="Petal length")
#'

rotatogram <- function(x1,x2,x1.label="X1",x2.label="X2",
                       suppress.plot=FALSE,return.values=FALSE){
  # Analysis
  {
    # Least products reg
    df <- data.frame(x1,x2)

    beta <- sign(stats::cov(x1,x2))*stats::sd(x2)/stats::sd(x1)
    intercept <- mean(x2) - beta*mean(x1)

    x1.range <- (max(x1)-min(x1))
    x2.range <- (max(x2)-min(x2))
    coord.ratio <- x1.range/x2.range

    # Find coordinates that are 10% longer

    x2.x1min <- intercept + beta*min(x1)
    x2.x1max <- intercept + beta*max(x1)

    slope.degrees <- atan((x2.x1max-x2.x1min)/x2.range)*(180/pi)-90
  }

  # Graphical output and extraction
  {
    # Generate original chart
    p.regular <- ggplot(data=df,aes(x=x1,y=x2))+
      theme_bw()+
      coord_fixed(ratio = coord.ratio) +
      geom_point(alpha=0.5,shape=16,color="aquamarine3") +
      geom_function(fun=Vectorize(function(x) {
        return(beta*x+intercept)
      }), color="darkslateblue",linetype="solid", linewidth=1.5)+
      theme(
        panel.border = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank()
      )+
      xlab(x1.label)+
      ylab(x2.label)+
      geom_hline(yintercept = min(x2)-(max(x2)-min(x2))*.05,color="darkslategrey",linetype=2)+
      geom_vline(xintercept = min(x1)-(max(x1)-min(x1))*.05,color="darkslategrey",linetype=2)

    # Change label angles
    p.pre.rotated <- p.regular +
      theme(
        axis.text.x = element_text(angle = slope.degrees,vjust=.5),
        axis.text.y = element_text(angle = slope.degrees,hjust=.5)
      )

    if (slope.degrees<=0){
      p.pre.rotated <- p.pre.rotated +
        theme(
          axis.title.y = element_text(angle = 270),
          axis.title.x = element_text(angle = 0)
        )
    }
  }

  if (suppress.plot == FALSE){
    grid::grid.newpage()
    suppressWarnings(print(p.pre.rotated,
                           vp = grid::viewport(width = 0.75,
                                         height = 0.75,
                                         angle = -slope.degrees)))
  }
  output <- list("intercept"=intercept,"beta"=beta,"slope.degrees"=slope.degrees)
  if (return.values==TRUE){return(output)}
}

