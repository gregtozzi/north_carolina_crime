addLogColumns <- function(data, columns) {
  # Adds log transformed columns to a dataframe
  #
  # Args:
  #   data:    a data.frame instance
  #   columns: an integer vector of column indexes
  # Returns:
  #   data:    a transformed data.frame instance
  dataNames <- names(data)
  for(i in columns) {
    currentName <- dataNames[i]
    lName <- paste0('l', currentName)
    data[,lName] <- log(data[,currentName])
  }
  return(data)
}


econHist <- function(x, data, curve = T, bins = 10) {
  # Plots a histogram using a version of the Economist
  # theme in ggplot.  If curve = TRUE, plots a normal
  # curve overlaid on the histogram.
  #
  # Args:
  #   x:     a character string giving the name of the
  #          column of data to be plotted
  #   data:  a data.frame containing the data
  #   curve: a logical indicating whether a normal curve
  #          should be plotted
  #   bins:  an integer giving the number of bins
  # Returns nothing but plots the histogram
  title <- paste('Density of', x)
  mu = mean(data[,x])
  sigma = sd(data[,x])
  if(curve) {
    ggplot(data = data, aes(x = data[,x])) +
      geom_histogram(aes(y = ..density..),
                     color = 'white', bins = bins) +
      xlab(NULL) +
      theme_economist_white(gray_bg = FALSE) +
      ggtitle(label = title,
              subtitle = 'Overlaid with a normal curve') +
      stat_function(fun = dnorm,
                    args = list(mean = mu,
                                sd = sigma),
                    colour = '#e3120b') +
      theme(plot.title = element_text(size=14))
  } else {
    ggplot(data = data, aes(x = data[,x])) +
      geom_histogram(aes(y = ..density..),
                     color = 'white', bins = bins) +
      xlab(x) +
      theme_economist_white(gray_bg = FALSE) +
      ggtitle(label = title) +
      theme(plot.title = element_text(size=14))
  }
}


formattedTable <- function(x, columns) {
  # Outputs a nicely formatted table using kable
  #
  # Args:
  #   x:        a data.frame
  #   columns:  an integer vector giving the columns
  #             to be printed
  # Returns nothing but prints the formatted table
  n <- ncol(x)
  startPoints <- seq(1, n, by = columns)
  endPoints <- pmin(startPoints + columns - 1, n)
  splitTable <- lapply(1:length(startPoints),
                       FUN = function(i) x[, startPoints[i]:endPoints[i]])
  for(j in splitTable) {
    print(kable_styling(kable(j, format = 'latex',
                              booktabs = T),
                        latex_options = 'striped')
    )
  }
}


econPlot <- function(x, y, data) {
  # Plots bivariate correlation using the Economist
  # theme in ggplot
  #
  # Args:
  #   x:    a character string giving the name of the
  #         column in which the x-data is contained
  #   y:    a character string giving the name of the
  #         column in which the y-data is contained
  #   data: a data.frame containing the data
  # Returns nothing but plots a chart
  title <- paste(x, 'vs.', y)
  ggplot(data = data, aes(x = data[,x], y = data[,y])) +
    geom_point() +
    theme_economist_white(gray_bg = FALSE) +
    geom_smooth(method = "lm", color = '#e3120b') +
    ggtitle(label = title,
              subtitle = 'With a regression line') +
    xlab(x) +
    ylab(y) +
    theme(plot.title = element_text(size=14))
}


baseAndLogPHist <- function(x, data, ...) {
  # Plots the histogram of a variable both log-
  # transformed and untransformed using the econHist
  # function above
  basePlot <- econHist(x, data, ...)
  logPlot <- econHist(paste0('l', x), data, ...)
  grid.arrange(basePlot, logPlot, ncol = 2)
}


Z <- function(x) (x - mean(x)) / sd(x)


coefTestTable <- function(x) {
  # Uses kable to generate a table of coefficent
  # estimates with related statistics
  xVars <- length(x[,1])
  xDF <- as.data.frame(x[1:xVars,])
  print(kable_styling(kable(xDF,
                                format = 'latex',
                                booktabs = T),
                          latex_options = 'striped'))
}