 summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  # datac <- rename(datac, c(mean = as.name(measurevar)))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


#Function to draw plot visualizing predicted values from OLS regression.
#F is continuous
mplot <- function(y, x, f, t = "Treatment Groups", xlab, title= "Plotted interacion effects"){
        library(ggplot2)
        fit1 <- lm(y ~ x*f)
        fit1_d <- data.frame(expand.grid(x = levels(x), 
                                         f = min(f, na.rm = T):max(f, na.rm = T)))
        fit1_d <- cbind(fit1_d, predict(fit1, fit1_d, interval = "confidence"))
        
        ggplot(fit1_d, aes(x = f, y = fit)) + 
                geom_ribbon(aes(ymin = lwr, ymax = upr, fill = x), alpha = 0.2) +
                geom_line(aes(colour = x)) + 
                scale_colour_discrete(name = t) + 
                scale_fill_discrete(name = t) + 
                ylab("Predicted Value") +
                xlab(xlab) + 
                ggtitle(title) +
                theme_bw()
}

 
#Similar to mplot but f is factor
fplot <- function(y, x, f, t = "Treatment Groups", xlab){
         library(ggplot2)
         fit1 <- lm(y ~ x*f)
         fit1_d <- data.frame(expand.grid(x = levels(x), f = levels(f)))
         fit1_d <- cbind(fit1_d, predict(fit1, fit1_d, interval = "confidence"))
         
         ggplot(fit1_d, aes(x=f, y=fit)) + 
                 geom_errorbar(aes(ymin=lwr, ymax=upr, colour = x), 
                               width=.1, position=position_dodge(0.1)) +
                 geom_point(position=position_dodge(0.1), aes(colour = x)) +
                 scale_colour_discrete(name = t) + 
                 ylab("Predicted Values") +
                 xlab(xlab) 
 }
 
#Function to combine ggplots 
 multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
         library(grid)
         
         # Make a list from the ... arguments and plotlist
         plots <- c(list(...), plotlist)
         
         numPlots = length(plots)
         
         # If layout is NULL, then use 'cols' to determine layout
         if (is.null(layout)) {
                 # Make the panel
                 # ncol: Number of columns of plots
                 # nrow: Number of rows needed, calculated from # of cols
                 layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                  ncol = cols, nrow = ceiling(numPlots/cols))
         }
         
         if (numPlots==1) {
                 print(plots[[1]])
                 
         } else {
                 # Set up the page
                 grid.newpage()
                 pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                 
                 # Make each plot, in the correct location
                 for (i in 1:numPlots) {
                         # Get the i,j matrix positions of the regions that contain this subplot
                         matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                         
                         print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                         layout.pos.col = matchidx$col))
                 }
         }
 }