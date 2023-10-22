TACT_fun_basic <- function(r=NULL, x=NULL, y=NULL, distribution = c("normal", "uniform", "skewed"),
                           n = 10^6,
                     cutoffsx = c(1/3,2/3), cutoffsy = c(1/3,2/3),
                     plot=TRUE, n.plotted = 10^3, Main = "",
                     Xlab = "X", Ylab = "Y",
                     #Char=c(-0x263A,-0x2639,-0x2639,-0x2639,-0x263A,-0x2639,-0x2639,-0x2639,-0x263A),
                     #Char = c("\u2713", "\u2717", "\u2717", "\u2717", "\u2713", "\u2717", "\u2717", "\u2717", "\u2713"),
                     #Col=c("olivedrab2","orangered3","orangered3","orangered3","olivedrab2",
                     #       "orangered3","orangered3","orangered3","olivedrab2"),
                     #Cex=c(1.2,0.9,0.9,0.9,1.2,0.9,0.9,0.9,1.2),
                     
                     #Char=c(-0x2639,-0x2639,-0x263A,-0x2639,-0x263A,-0x2639,-0x263A,-0x2639,-0x2639),
                     Char=c("\u2717","\u2717","\u2713",
                            "\u2717","\u2713","\u2717",
                            "\u2713","\u2717","\u2717"),
                     Col=c("orangered3","orangered3","olivedrab2",
                           "orangered3","olivedrab2","orangered3",
                           "olivedrab2","orangered3","orangered3"),
                     Cex=c(0.9,0.9,1.2,0.9,1.2,0.9,1.2,0.9,0.9),
                     font_size = 1,
                     plot.percents = c(TRUE,TRUE,TRUE,
                                       TRUE,TRUE,TRUE,
                                       TRUE,TRUE,TRUE),
                     language="fi",
                     Cex_multiplier = 1)

{
  #r = .80
  #distribution = "normal"
  #n = 10^6
  #cutoffsx = c(1,2,3,4)/4
  #cutoffsy = c(1,2,3,4)/4
  #cutoffsx = c(1/3,2/3)
  #cutoffsy = c(1/3,2/3)
  #plot.percents = c(TRUE,TRUE,TRUE,TRUE,TRUE, TRUE,TRUE,TRUE,TRUE)
  #plot=TRUE
  #n.plotted = 10^3
  #Xlab = "X"
  #Ylab = "Y"
  #Char=c("\u2717","\u2717","\u2713",
  #       "\u2717","\u2713","\u2717",
  #       "\u2713","\u2717","\u2717")
  #Col=c("orangered3","orangered3","olivedrab2",
  #      "orangered3","olivedrab2","orangered3",
  #      "olivedrab2","orangered3","orangered3")
  #Cex=c(0.9,0.9,1.2,0.9,1.2,0.9,1.2,0.9,0.9)
  #Main = ""
  #font_size = 1
  #Cex_multiplier=1
  
  cut3 <- function(x,c1,c2) cut(x,quantile(x,c(0,c1,c2,1)),
                                c("Low","Medium","High"), include.lowest=TRUE)
  
  # function to make the percentages symmetric
  harmonize <- function(x){
    val <- (x[lower.tri(x)] + t(x)[lower.tri(x)]) / 2
    x[lower.tri(x)] = val
    x <- t(x)
    x[lower.tri(x)] = val
    x
  }
  
  if( !is.null(r) ) {
    # test if variables are only cut in two
    if(diff(cutoffsx) == 0) cutoffsx[1] <- cutoffsx[1] + 1/10^8
    if(diff(cutoffsy) == 0) cutoffsy[1] <- cutoffsy[1] + 1/10^8
    
    # generate normal y from normal x with linear model
    if(distribution == "normal"){
      x <- rnorm(n)
      y <- r * x + sqrt(1 - r^2)*rnorm(n)
    }
    if(distribution == "uniform"){
      x <- runif(n, -1, 1)
      y <- r * x + sqrt(1 - r^2)*runif(n,-1,1)
    }
    if(distribution == "skewed"){
      x <- rbeta(n, 5, 1)
      y <- r * x + sqrt(1 - r^2)*rbeta(n, 5, 1)
    }
    
  } else {
    val <- !is.na(x) & !is.na(y)
    y <- y[val]
    x <- x[val]
    if(sum(!val) > 0) warning("\nMissing values deleted\n")
    n <- length(x)
  }
  
  # allocate observations to tabs based on cutoffs
  tabs <- data.frame(y = cut3(y, cutoffsy[1], cutoffsy[2]),
                     x = cut3(x, cutoffsx[1], cutoffsx[2]))
  # calculate percentages in each tab
  crosstabs <- apply(prop.table(table(tabs)),2,function(x) x/sum(x))
  
  # balance the proportions at low/low and high/high to be the same
  dr = (crosstabs[1,1] + crosstabs[3,3]) / 2
  # harmonize the proportions
  if((1/3) %in% cutoffsx & (2/3) %in% cutoffsx & (1/3) %in% cutoffsy & (2/3) %in% cutoffsy ) {
    crosstabs[1,1] <- dr
    crosstabs[3,3] <- dr
    crosstabs <- harmonize(crosstabs)
  }
  # name rows and columns
  crosstabs <- crosstabs[c("High","Medium","Low"),
                         c("Low","Medium","High")]
  # convert to percentages
  percents <- matrix(paste(round((crosstabs * 100),1),"%", sep=""), ncol=3)
  # request only certain percentages to be plotted
  percents[!plot.percents] <- ""
  # correlation between x and y
  cr <- cor(x,y)
  xorig <- x
  yorig <- y
  
  # xorig[xorig < -4.5] = 4.5
  # xorig[xorig > 4.5] = 4.5
  # yorig[yorig < -4.5] = 4.5
  # yorig[yorig > 4.5] = 4.5
  if(plot){
    if(n > n.plotted){
      # draw a subsample from the original
      samp <- sample(n,n.plotted)
      x <- x[samp]
      y <- y[samp]
      tabssamp <- data.frame(y = cut3(y, cutoffsy[1], cutoffsy[2]),
                             x = cut3(x, cutoffsx[1], cutoffsx[2]))
      # assign a coloring scheme for the categories
      color <- as.factor(apply(tabssamp, 1, paste, collapse=""))
      
    } else {
      color = as.factor(apply(tabs, 1, paste, collapse=""))
    }
    #color <- recode(color, 
    #                "LowHigh"=9,"LowMedium"=6,"LowLow"=3,
    #                "MediumHigh"=8,"MediumMedium"=5,"MediumLow"=2,
    #                "HighHigh"=7,"HighMedium"=4,"HighLow"=1
    #)
    
    if (r >= 0){
      color <- recode(color, 
                      "LowHigh"=9,"LowMedium"=6,"LowLow"=3,
                      "MediumHigh"=8,"MediumMedium"=5,"MediumLow"=2,
                      "HighHigh"=7,"HighMedium"=4,"HighLow"=1)
      
    } else {
      color <- recode(color, 
                      "LowHigh"=3,"LowMedium"=6,"LowLow"=9,
                      "MediumHigh"=8,"MediumMedium"=5,"MediumLow"=2,
                      "HighHigh"=1,"HighMedium"=4,"HighLow"=7)
      
    }
    
    
    plot<-plot(x,y, col="white",xaxt="n", yaxt="n",
               ylim = c(min(yorig), max(yorig)), xlim = c(min(xorig), max(xorig)),
               xlab=Xlab, ylab=Ylab)
    points(x, y,
           cex=Cex[color]*Cex_multiplier,
           col=Col[color],
           pch=Char[color]
    )
    
    xq <- quantile(xorig, cutoffsx)
    yq <- quantile(yorig, cutoffsy)
    
    lines(rep(xq[1],2),c(min(yorig) - abs(min(yorig)),max(yorig) + abs(max(yorig))), col="grey50",lty = 2)
    lines(rep(xq[2],2),c(min(yorig) - abs(min(yorig)),max(yorig) + abs(max(yorig))), col="grey50",lty = 2)
    lines(c(min(xorig) - abs(min(xorig)),max(xorig) + abs(max(xorig))), rep(yq[1],2), col="grey50",lty = 2)
    lines(c(min(xorig) - abs(min(xorig)),max(xorig) + abs(max(xorig))), rep(yq[2],2), col="grey50",lty = 2)
    
    text((xq[1] + min(xorig))/2, (yq[2] + max(yorig))/2, percents[1,1], cex=font_size, font=2, col="black")
    text((xq[1] + min(xorig))/2, (yq[1] + min(yorig))/2, percents[3,1], cex=font_size, font=2, col="black")
    text((xq[2] + max(xorig))/2, (yq[2] + max(yorig))/2, percents[1,3], font=2, cex=font_size, col="black")
    text((xq[2] + max(xorig))/2, (yq[1] + min(yorig))/2, percents[3,3], font=2, cex=font_size, col="black")
    
    if(abs(diff(cutoffsx)) > .001) {
      text(median(xorig), (yq[2] + max(yorig))/2, percents[1,2], cex=font_size, font=2,col="black")
      text(median(xorig), (yq[1] + min(yorig))/2, percents[3,2], cex=font_size, font=2, col="black")
    }
    if(abs(diff(cutoffsy)) > .001){
      text((xq[1] + min(xorig))/2, mean(yorig), percents[2,1], cex=font_size, font=2, col="black")
      text((xq[2] + max(xorig))/2, mean(yorig), percents[2,3], cex=font_size, font=2, col="black")
    }
    
    if(abs(diff(cutoffsx)) > .001 & abs(diff(cutoffsy)) > .001) text(median(xorig), mean(yorig), percents[2,2], cex=font_size, font=2, col="black")
    
    xstand <- (xorig-min(xorig))/(max(xorig)-min(xorig))
    xq1 <- quantile(xstand, cutoffsx)
    ystand <- (yorig-min(yorig))/(max(yorig)-min(yorig))
    yq1 <- quantile(ystand, cutoffsy)
    mtext(Main, side=3, line=2, adj=0)
    mtext(paste(TACT_languages[TACT_languages$language==language,"low"],
                ' (<', paste(round(cutoffsx[1]*100,1),'%)', sep="")),
          side=1, line=1, cex=font_size, adj=(xq1[1])/2.4, col="black")
    mtext(paste(TACT_languages[TACT_languages$language==language,"high"],
                ' (>', paste(round(cutoffsx[2]*100,1),'%)', sep="")),
          side=1, line=1, cex=font_size, adj=(xq1[2] + 1)/1.75, col="black")
    mtext(paste(TACT_languages[TACT_languages$language==language,"low"],
                ' (<', paste(round(cutoffsy[1]*100,1),'%)', sep="")),
          side=2, line=1, cex=font_size, adj=(yq1[1])/2.4)
    mtext(paste(TACT_languages[TACT_languages$language==language,"high"],
                ' (>', paste(round(cutoffsy[2]*100,1),'%)', sep="")),
          side=2, line=1, cex=font_size, adj=(yq1[2] + 1)/1.75 )
    
    
    
    if(abs(diff(cutoffsy)) > 0.001) mtext(TACT_languages[TACT_languages$language==language,"medium"],
                                          side=2, line=1, cex=font_size, adj=median(ystand))
    if(abs(diff(cutoffsx)) > 0.001) mtext(TACT_languages[TACT_languages$language==language,"medium"],
                                          side=1, line=1, cex=font_size, adj=median(xstand), col="black")
    
    
    mtext(paste(TACT_languages[TACT_languages$language==language,"correlation"],
                " =", round(cr,2)), side = 1, line=-1.5, adj=0.04, cex=font_size)
    
    # comment the top text out completely
    #if (r >=0){
    #  if(abs(diff(cutoffsx)) > 0.001 & abs(diff(cutoffsy)) > 0.001 & language!="es")
    #    mtext(paste(round(sum(diag(t(apply(crosstabs, 2, rev)))) / sum(crosstabs),3)*100,
    #                TACT_languages[TACT_languages$language==language,"match_1"],
    #                TACT_languages[TACT_languages$language==language,"match_2"],sep=""),
    #          side = 3, line=-8.0, adj=0.04, cex=font_size)
    #  
    #  if(abs(diff(cutoffsx)) > 0.001 & abs(diff(cutoffsy)) > 0.001 & language=="es")
    #    mtext(paste("Keskmiselt ",
    #                round(sum(diag(t(apply(crosstabs, 2, rev)))) / sum(crosstabs),3)*100,
    #                "% väärtustest kattuvad.\nSeose täieliku puudumise korral kattuks 33.3%", sep=""),
    #          side = 3, line=-8.0, adj=0.04, cex=font_size)
    #  
    #}
    
  }
  
  #if(abs(diff(cutoffsx)) < 0.001) crosstabs[,2] <- 0
  #if(abs(diff(cutoffsx)) > 0.001 & abs(diff(cutoffsy)) > 0.001) cat("\n", "Overall accuracy in matching low, medium and high values:", round(sum(diag(t(apply(crosstabs, 2, rev)))) / sum(crosstabs),3), "(against the random-guess baseline of .333)\n")
  #round(crosstabs,4)
  
  results <- list(
    crosstabs = crosstabs,
    cutoffsx = cutoffsx,
    cutoffsy = cutoffsy,
    plot = plot
  )
  return(results)
}

