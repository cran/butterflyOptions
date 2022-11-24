#'Calculates per share Profit and Loss (PnL) at expiration for Short Call Butterfly Option Strategy and draws its Bar Plot displaying PnL in the Plots tab.
#'@description
#'This is a volatility strategy consisting of a short position in an ITM (in the money) call option with a strike price X1L, a long position in two ATM ( at the money) call options with a strike price X, and a short position in an OTM (out of the money) call option with a strike price X3H. The strikes are equidistant: X3H minus X equals to X minus X1L . This is a net credit trade. In this sense, this is an income strategy. The trader or investor has  neutral outlook (Kakushadze & Serur, 2018).
#'@details
#'According to conceptual details given by Cohen (2015), and a closed form solution provided by Kakushadze and Serur (2018), this method is developed, and the given examples are created, to compute per share Profit and Loss at expiration for Short Call Butterfly Option Strategy and draw its graph in the Plots tab.
#'@param ST Spot Price at time T.
#'@param X1L Lower Strike Price or eXercise price for one ITM shorted Call.
#'@param X Strike Price or eXercise price for two ATM bought Calls.
#'@param X3H Higher Strike Price or eXercise price for one OTM shorted Call.
#'@param C1L Call Premium or Call Price received for the first ITM shorted Call.
#'@param C Call Premium or Call Price paid for the two ATM bought Calls.
#'@param C3H Call Premium or Call Price received for the one OTM shorted Call.
#'@param PnL Profit and Loss
#'@param spot Spot Price
#'@param pl Profit and Loss
#'@param myData Data frame
#'@param myTibble tibble
#'@param hl lower bound value for setting lower-limit of x-axis displaying spot price.
#'@param hu upper bound value for setting upper-limit of x-axis displaying spot price.
#'@return graph of the strategy
#'@importFrom magrittr %>%
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 geom_point
#'@importFrom ggplot2 scale_fill_manual
#'@importFrom ggplot2 scale_color_manual
#'@importFrom ggplot2 geom_col
#'@importFrom tibble as_tibble
#'@importFrom dplyr mutate
#'@importFrom ggplot2 aes
#'@importFrom ggplot2 element_line
#'@importFrom ggplot2 element_rect
#'@importFrom ggplot2 element_text
#'@importFrom ggplot2 geom_line
#'@importFrom ggplot2 geom_text
#'@importFrom ggplot2 labs
#'@importFrom ggplot2 scale_colour_manual
#'@importFrom ggplot2 scale_y_continuous
#'@importFrom ggplot2 theme
#'@author MaheshP Kumar, \email{maheshparamjitkumar@@gmail.com}
#'@references
#'Cohen, G. (2015). The Bible of Options Strategies (2nd ed.). Pearson Technology Group.\cr
#'Kakushadze, Z., & Serur, J. A. (2018, August 17). 151 Trading Strategies. Palgrave Macmillan. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3247865 \cr
#'R Graphics Cookbook. (n.d.). Coloring Negative and Positive Bars Differently. https://r-graphics.org/recipe-bar-graph-color-neg \cr
#'Gross C, Ottolinger P (2016)._ggThemeAssist: Add-in to Customize 'ggplot2' Themes_. R package version 0.1.5, <URL: https://CRAN.R-project.org/package=ggThemeAssist>.
#'@examples
#'shortCallbutterfly(400,400,375,425,6,17.5,7.5,hl=0.9,hu=1.1)
#'@export
shortCallbutterfly <- function (ST,X,X1L,X3H,C,C1L,C3H,hl=0,hu=2,spot=spot,pl=pl,myData=myData,myTibble=myTibble,PnL=PnL){
  V0Cr= C1L+ C3H - (2*C)
  myData <- data.frame (spot = c((ST*hl):(ST*hu)))
  myData$pl <- (2* (pmax((myData$spot-X),0))) - (pmax((myData$spot-X1L),0))- (pmax((myData$spot-X3H),0)) + V0Cr
  myData$pl = round(myData$pl, digits=2)
  myData$spot = round(myData$spot, digits=2)
  myTibble <- as_tibble(myData)
  myTbColored <- myTibble %>% mutate(PnL = pl >= 0)
  ggplot(myTbColored,aes(x=spot,y=pl,fill=PnL,label=pl)) +
    geom_col(position = "identity",colour="deeppink1", size=0.4) +
    scale_fill_manual(values = c("darkviolet","#51EDFF"), guide= "none" ) +
    geom_point(aes(color=PnL), size=5)+
    scale_color_manual(values = c("deeppink1","turquoise4"), guide= "none" ) +
    geom_text(nudge_y = 1,size= 3.5, color="olivedrab1")+
    theme(plot.caption = element_text(colour  =  '#A2E1A2'))+
    theme(axis.ticks = element_line(size  =  2,colour = "olivedrab1"))+
    theme(panel.grid.major = element_line(colour  =  'palegreen1',size  =  0.25))+
    theme(panel.grid.minor = element_line(colour  =  'seagreen1',size  =  0.25))+
    theme(axis.title = element_text(colour  =  'lightcyan1', size=14))+
    theme(plot.title = element_text(colour  =  'snow1', vjust  =  1,size=16))+
    theme(panel.background = element_rect(fill  =  '#2BAC15'))+
    theme(plot.background = element_rect(fill  =  '#32C718', colour  =  'aquamarine4', linetype  =  'dashed'))+
    labs(title  =  'Short Call Butterfly Strategy using Options', x  =  'Spot Price ($) at Expiration', y  =  'PnL ($) at Expiration', subtitle  =  'High Volatility / Neutral Outlook', caption  =  'butterflyOptions / MaheshP Kumar')
}


