\name{TACT}
\alias{TACT}
\title{
Trisect and Cross-Tabulate (TACT) continuous variables.
}
\description{
A simple tool for making a correlations between continuous variables interpretable for individuals. Both simulated variables, having a specified correlation, are converted to low, medium and high values and the proportions of low, medium and high values of y are calculated for each x value (low, medium or high).
}
\usage{
TACT(r, distribution = c("normal", "uniform", "skewed"), n = 10^6,
cutoffsx = c(1/3, 2/3), cutoffsy = c(1/3, 2/3),
plot = TRUE, n.plotted = 10^3,
Main = "Cross-Tabulation of X and Y", Xlab = "X", Ylab = "Y")
}
\arguments{
  \item{r}{
The correlation for which TACT is calculated
}
  \item{distribution}{
Distribution of the variables involved (normal, uniform or skewed)
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
