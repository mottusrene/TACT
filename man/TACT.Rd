\name{TACT}
\alias{TACT}
\title{
Trisect and Cross-Tabulate (TACT) continuous variables.
}
\description{
A simple tool for making a correlations between continuous variables interpretable for individuals. Supply two variables or have them simulated, by supplying a correlation and variables' distributions. Both are converted to low, medium and high values and the proportions of low, medium and high values of y are calculated for each x value. To omit the medium level, set the cutoffs equal.
}

\usage{
TACT(r=NULL, x=NULL, y=NULL, distribution = c("normal", "uniform", "skewed"), n = 10^6,
    cutoffsx = c(1/3,2/3), cutoffsy = c(1/3,2/3),
    plot=TRUE, n.plotted = 10^3, Main = "",
    Xlab = "X", Ylab = "Y",
    Char=c(-0x263A,-0x2639,-0x2639,-0x2639,-0x263A,-0x2639,-0x2639,-0x2639,-0x263A),
    Col=c("powderblue","orangered3","orangered3","orangered3","powderblue","orangered3","orangered3","orangered3","powderblue"),
    Cex=c(1.2,0.9,0.9,0.9,1.2,0.9,0.9,0.9,1.2),
    plot.percents = c(TRUE,TRUE,TRUE,TRUE,TRUE, TRUE,TRUE,TRUE,TRUE))
}

\arguments{
  \item{r}{
The correlation for which TACT is calculated. If x and y are supplied, set NULL.
}
  \item{x}{
  Supplied variabe x. To simulate correlation, set NULL
  }
  \item{y}{
  Supplied variabe y. To simulate correlation, set NULL
  }
  \item{distribution}{
  Distribution of the variables involved (normal, uniform or skewed). For more complex scenarios, including when variabels should have different distributions, supply x and y.
  }
  \item{n}{
  The number of data-points (individuals) to be simulated. For stable values, use n > 10^6.
  }
  \item{cutoffsx}{
  The cutoffs between low and medium, and medium and high for variable x (vector of two numbers between 0 and 1). For example, to trisect the variable, use c(1/3,2/3). If both numbers are equal, the variable is bisected.
  }
  \item{cutoffsy}{
  The cutoffs between low and medium, and medium and high for variable x (vector of two numbers between 0 and 1). For example, to trisect the variable, use c(1/3,2/3). If both numbers are equal, the variable is bisected.
  }
  \item{plot}{
Should a plot be drawn? Defaults to TRUE
  }
  \item{n.plotted}{
  Number of data-points that are plotted. Defaults to 10^3. If it is much over 10^5 or 10^6, the function may be slow or the computer may even crash (because a lot of drawing is being done).
  }
  \item{Main}{
  Plot's title.
  }
  \item{Xlab}{
  Label for the x axis.
  }
  \item{Ylab}{
  Label for the y axis
  }
  \item{Char}{
  Specify symbols for each grid slot. A vector of nine characters.
}
  \item{Col}{
  Specify colours for symbols for each grid slot. A vector of nine colours.
  }
  \item{Cex}{
  Specify sizes for symbols for each grid slot. A vector of nine sizes (1 = normal).
  }
  \item{plot.percents}{
  Should percentages be plotted in specific grid slots. A logical of length 9.
}
}

\references{
Under review.
}
\author{
René Mõttus (rene.mottus@ed.ac.uk)
}

\examples{

TACT(r = .10, distribution = "normal")
}
