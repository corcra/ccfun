#' @title Mosaic plot using ggplot.
#'
#' @description
#' Creates a mosaic plot where the dimensions of the cells of a 
#' confusion matrix represent their marginal proportions.
#'
#' @details
#' Credit for initial iteration to 
#' [Edwin](http://stackoverflow.com/a/19258045/992999)
#' This version adds color brewer options and tidies the labelling

ggMMplot <- function(var1, var2, palette="YlOrRd"){
  require(ggplot2)
  
  levVar1 <- length(levels(var1))
  levVar2 <- length(levels(var2))

  jointTable <- prop.table(table(var1, var2))
  plotData <- as.data.frame(jointTable)
  plotData$marginVar1 <- prop.table(table(var1))
  plotData$marginVar2 <- prop.table(table(var2))
  plotData$var2Height <- plotData$Freq / plotData$marginVar1
  plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) +
    plotData$marginVar1 / 2

  # Define label positions on LEFT (y-axis)
  ylabData <- plotData[plotData$var1==levels(plotData$var1)[1],]
  dd <- (y=c(0, cumsum(ylabData$var2Height)))
  ylabData$ylabCenter <- sapply(1:(length(dd)-1), function(x) dd[x] + (dd[x+1] - dd[x])/2 )

  # Define label positions on the BOTTOM (x-axis)
  xlabData <- plotData[plotData$var2==levels(plotData$var2)[1],]
  dd <- (x=c(0, cumsum(xlabData$marginVar1)))
  xlabData$xlabCenter <- sapply(1:(length(dd)-1), function(x) dd[x] + (dd[x+1] - dd[x])/2 )

  ggplot(plotData, aes(var1Center, var2Height)) +
    geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "White") +
    scale_fill_brewer(type="seq", palette=palette, guide=FALSE) +
    # xlabels
    geom_text(data=xlabData,
              aes(label = as.character(var1), x = xlabCenter, y = -0.05),
              vjust="inward") +
    # ylabels
    geom_text(data=ylabData, 
              aes(label = as.character(var2), y = ylabCenter, x = -0.05),
                  vjust="top", angle=90) +
    xlab("") + scale_x_discrete(labels=NULL) +
    ylab("") + scale_y_discrete(labels=NULL) +
    theme_minimal() +
    theme(plot.margin=margin(rep(20,4)))
}