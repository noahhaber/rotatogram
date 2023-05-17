#' A rotatogram is a method of displaying an association which is axis non-dominant. This is achieved in two ways: First, the method of estimating the slope and intercept uses the least-products method rather than more typical least squared error for the "dependent" variable. The least products method has no "dependent" variable and is scale independent. Second, the plot is rotated such that the resulting regression line is vertical, reducing the suggestion that the vertical axis is the dominant one. The slope can be read relative to either axis equally.
#' @title Rotatogram
#' @name rotatogram
#' @keywords association
#' @param x1 (required) A vector containing the data for the x1 variable
#' @param x2 (required) A vector containing the data for the x2 variable
#' @param x1.label (optional) Changes the x1 axis label to the specified name
#' @param x2.label (optional) Changes the x2 axis label to the specified name
#' @param error.bootstrap (optional) Generates and displays bootstrapped errors to enable confidence bands, standard errors, and confidence intervals
#' @param error.bootstrap.iterations (optional) Sets number of iterations for which to run bootstrap
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
                       error.bootstrap=FALSE,error.bootstrap.iterations = 500,
                       suppress.plot=FALSE,return.values=FALSE){
  # Analysis
  {
    # Least products reg
    df <- data.frame(x1,x2)

    beta <- sign(stats::cov(x1,x2))*stats::sd(x2)/stats::sd(x1)
    intercept <- mean(x2) - beta*mean(x1)

    # Bootstrap
    if (error.bootstrap == TRUE){
      single.bootstrap <- function(){
        df.bootstrap <- df[sample(1:nrow(df),size=nrow(df),replace=TRUE),]
        beta <- sign(stats::cov(df.bootstrap$x1,df.bootstrap$x2))*stats::sd(df.bootstrap$x2)/stats::sd(df.bootstrap$x1)
        intercept <- mean(df.bootstrap$x2) - beta*mean(df.bootstrap$x1)
        return (data.frame(intercept,beta))
      }
      df.bootstrap.params <- do.call(rbind,replicate(error.bootstrap.iterations,single.bootstrap(),simplify=FALSE))
    }

    x1.range <- (max(x1)-min(x1))
    x2.range <- (max(x2)-min(x2))
    coord.ratio <- x1.range/x2.range

    # Find x2 values at x1 min and max
    x2.x1min <- intercept + beta*min(x1)
    x2.x1max <- intercept + beta*max(x1)

    slope.degrees <- atan((x2.x1max-x2.x1min)/x2.range)*(180/pi)-90
  }

  # Graphical output and extraction
  {
    # Generate original chart and themes
    p.regular <- ggplot(data=df,aes(x=x1,y=x2))+
      theme_bw()+
      coord_fixed(ratio = coord.ratio) +
      geom_point(alpha=0.5,shape=16,color="aquamarine3")+
      theme(
        panel.border = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor = element_blank()
      )+
      xlab(x1.label)+
      ylab(x2.label)

    # Add bootstrap lines
    if (error.bootstrap == TRUE){
      for (i in 1:nrow(df.bootstrap.params)){
        p.regular <- p.regular +
          geom_function(fun=Vectorize(function(x,i) {
            return(df.bootstrap.params$beta[i]*x+df.bootstrap.params$intercept[i])
          }), args = list(i = i),
          color="darkslateblue", linewidth=.5,alpha=5*1/(nrow(df.bootstrap.params)))
      }
    }

    # Add main lines
    p.regular <- p.regular +
      geom_function(fun=Vectorize(function(x) {
        return(beta*x+intercept)
      }), color="darkslateblue",linetype="solid", linewidth=1.5)+
      geom_hline(yintercept = min(x2)-(max(x2)-min(x2))*.05,color="darkslategrey",linetype=2)+
      geom_vline(xintercept = min(x1)-(max(x1)-min(x1))*.05,color="darkslategrey",linetype=2)

    # Change label angles
    if (beta>=0){
      p.pre.rotated <- p.regular +
        theme(
          axis.text.x = element_text(angle = slope.degrees,vjust=.5),
          axis.text.y = element_text(angle = slope.degrees,hjust=.5),
          axis.title.x = element_text(angle = 0),
          axis.title.y = element_text(angle = 270)

        )
    } else {
      p.pre.rotated <- p.regular +
        theme(
          axis.text.x = element_text(angle = slope.degrees+180,vjust=.5),
          axis.text.y = element_text(angle = slope.degrees+180,hjust=.5),
          axis.title.x = element_text(angle = 0),
          axis.title.y = element_text(angle = 90)
        )
    }
  }

  if (beta>=0){
    rotate.angle <- -slope.degrees
  } else {
    rotate.angle <- -slope.degrees-180
  }

  if (suppress.plot == FALSE){
    grid::grid.newpage()
    suppressWarnings(print(p.pre.rotated,
                           vp = grid::viewport(width = 0.75,
                                         height = 0.75,
                                         angle = rotate.angle)))
  }

  if (return.values==TRUE){
    output <- list("intercept"=intercept,"beta"=beta,"slope.degrees"=slope.degrees,
                   "p.regular"=p.regular)
    if (error.bootstrap == TRUE){
      output <- c(output,
                  list("intercept.SE" = stats::sd(df.bootstrap.params$intercept),
                    "beta.SE" = stats::sd(df.bootstrap.params$beta)))
    }
    return(output)
  }
}

