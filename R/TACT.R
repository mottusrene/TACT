TACT <- function(r=NULL, x=NULL, y=NULL, distribution = c("normal", "uniform", "skewed"), n = 10^6,
                 cutoffsx = c(1/3,2/3), cutoffsy = c(1/3,2/3),
                 plot=TRUE, n.plotted = 10^3, Main = "",
                 Xlab = "X", Ylab = "Y",
                 Char=c(-0x263A,-0x2639,-0x2639,-0x2639,-0x263A,-0x2639,-0x2639,-0x2639,-0x263A),
                 Col=c("olivedrab2","orangered3","orangered3","orangered3","olivedrab2",
                       "orangered3","orangered3","orangered3","olivedrab2"),
                 Cex=c(1.2,0.9,0.9,0.9,1.2,0.9,0.9,0.9,1.2),
                 plot.percents = c(TRUE,TRUE,TRUE,TRUE,TRUE, TRUE,TRUE,TRUE,TRUE))

{

  cut3 <- function(x,c1,c2) cut(x,quantile(x,c(0,c1,c2,1)),c("Low","Medium","High"), include.lowest=TRUE)

  harmonize <- function(x){
    val <- (x[lower.tri(x)] + t(x)[lower.tri(x)]) / 2
    x[lower.tri(x)] = val
    x <- t(x)
    x[lower.tri(x)] = val
    x
  }

  if( !is.null(r) ) {

    if(diff(cutoffsx) == 0) cutoffsx[1] <- cutoffsx[1] + 1/10^8
    if(diff(cutoffsy) == 0) cutoffsy[1] <- cutoffsy[1] + 1/10^8

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

  tabs <- data.frame(y = cut3(y, cutoffsy[1], cutoffsy[2]), x = cut3(x, cutoffsx[1], cutoffsx[2]))
  crosstabs <- apply(prop.table(table(tabs)),2,function(x) x/sum(x))
  dr = (crosstabs[1,1] + crosstabs[3,3]) / 2
  if((1/3) %in% cutoffsx & (2/3) %in% cutoffsx & (1/3) %in% cutoffsy & (2/3) %in% cutoffsy ) {
    crosstabs[1,1] <- dr
    crosstabs[3,3] <- dr
    crosstabs <- harmonize(crosstabs)
  }
  crosstabs <- crosstabs[c("High","Medium","Low"),c("Low","Medium","High")]
  percents <- matrix(paste(round((crosstabs * 100),1),"%", sep=""), ncol=3)
  percents[!plot.percents] <- ""
  cr <- cor(x,y)
  xorig <- x
  yorig <- y
  # xorig[xorig < -4.5] = 4.5
  # xorig[xorig > 4.5] = 4.5
  # yorig[yorig < -4.5] = 4.5
  # yorig[yorig > 4.5] = 4.5
  if(plot){
    if(n > n.plotted){
      samp <- sample(n,n.plotted)
      x <- x[samp]
      y <- y[samp]
      tabssamp <- data.frame(y = cut3(y, cutoffsy[1], cutoffsy[2]), x = cut3(x, cutoffsx[1], cutoffsx[2]))
      color <- as.factor(apply(tabssamp, 1, paste, collapse=""))
    } else {
      color = as.factor(apply(tabs, 1, paste, collapse=""))
    }
    plot(x,y, col="white",xaxt="n", yaxt="n",
         ylim = c(min(yorig), max(yorig)), xlim = c(min(xorig), max(xorig)),
         xlab=Xlab, ylab=Ylab)
    points(x, y,
           cex=Cex[color],
           col=Col[color],
           pch=Char[color]
    )

    xq <- quantile(xorig, cutoffsx)
    yq <- quantile(yorig, cutoffsy)

    lines(rep(xq[1],2),c(min(yorig) - abs(min(yorig)),max(yorig) + abs(max(yorig))), col="grey90")
    lines(rep(xq[2],2),c(min(yorig) - abs(min(yorig)),max(yorig) + abs(max(yorig))), col="grey90")
    lines(c(min(xorig) - abs(min(xorig)),max(xorig) + abs(max(xorig))), rep(yq[1],2), col="grey90")
    lines(c(min(xorig) - abs(min(xorig)),max(xorig) + abs(max(xorig))), rep(yq[2],2), col="grey90")

    text((xq[1] + min(xorig))/2, (yq[2] + max(yorig))/2, percents[1,1], cex=1, font=2, col="black")
    text((xq[1] + min(xorig))/2, (yq[1] + min(yorig))/2, percents[3,1], cex=1, font=2, col="black")
    text((xq[2] + max(xorig))/2, (yq[2] + max(yorig))/2, percents[1,3], font=2, cex=1, col="black")
    text((xq[2] + max(xorig))/2, (yq[1] + min(yorig))/2, percents[3,3], font=2, cex=1, col="black")

    if(abs(diff(cutoffsx)) > .001) {
      text(median(xorig), (yq[2] + max(yorig))/2, percents[1,2], cex=1, font=2,col="black")
      text(median(xorig), (yq[1] + min(yorig))/2, percents[3,2], cex=1, font=2, col="black")
    }
    if(abs(diff(cutoffsy)) > .001){
      text((xq[1] + min(xorig))/2, mean(yorig), percents[2,1], cex=1, font=2, col="black")
      text((xq[2] + max(xorig))/2, mean(yorig), percents[2,3], cex=1, font=2, col="black")
    }

    if(abs(diff(cutoffsx)) > .001 & abs(diff(cutoffsy)) > .001) text(median(xorig), mean(yorig), percents[2,2], cex=1, font=2, col="black")

    xstand <- (xorig-min(xorig))/(max(xorig)-min(xorig))
    xq1 <- quantile(xstand, cutoffsx)
    ystand <- (yorig-min(yorig))/(max(yorig)-min(yorig))
    yq1 <- quantile(ystand, cutoffsy)
    mtext(Main, side=3, line=2, adj=0)
    mtext(paste('low (<', paste(round(cutoffsx[1]*100,1),'%)', sep="")), side=1, line=1, adj=(xq1[1])/2.4, col="black")
    mtext(paste('high (>', paste(round(cutoffsx[2]*100,1),'%)', sep="")), side=1, line=1, adj=(xq1[2] + 1)/1.75, col="black")
    mtext(paste('low (<', paste(round(cutoffsy[1]*100,1),'%)', sep="")), side=2, line=1, adj=(yq1[1])/2.4)
    mtext(paste('high (>', paste(round(cutoffsy[2]*100,1),'%)', sep="")), side=2, line=1, adj=(yq1[2] + 1)/1.75 )

    if(abs(diff(cutoffsy)) > 0.001) mtext('medium', side=2, line=1, adj=median(ystand))
    if(abs(diff(cutoffsx)) > 0.001) mtext('medium', side=1, line=1, adj=median(xstand), col="black")

    mtext(paste("Correlation =", round(cr,2)), side = 1, line=-1.5, adj=0.04, cex=1)
    if(abs(diff(cutoffsx)) > 0.001 & abs(diff(cutoffsy)) > 0.001)
      mtext(paste(round(sum(diag(t(apply(crosstabs, 2, rev)))) / sum(crosstabs),3)*100, "% of low, medium and high values match", sep=""), side = 3, line=-1.5, adj=0.04, cex=1)

  }

  if(abs(diff(cutoffsx)) < 0.001) crosstabs[,2] <- 0
  if(abs(diff(cutoffsx)) > 0.001 & abs(diff(cutoffsy)) > 0.001) cat("\n", "Overall accuracy in matching low, medium and high values:", round(sum(diag(t(apply(crosstabs, 2, rev)))) / sum(crosstabs),3), "(against the random-guess baseline of .333)\n")
  round(crosstabs,4)
}
