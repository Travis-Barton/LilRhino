\name{Percent}
\alias{Percent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Percent of confusion matrix
%%  ~~function to do ... ~~
}
\description{ For finding the accuracy of confusion matricies with true/pred values
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Percent(true, test)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{true}{ The true values
%%     ~~Describe \code{x} here~~
}
  \item{test}{the test values}
}
\details{ Make sure your strings have the right values and create a square matrix.
%%  ~~ If necessary, more details than the description above ~~
}
\value{ the percent acc.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{ Travis Barton
%%  ~~who you are~~
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
true <- rep(1:10, 10)
test <- rep(1:10, 10)
test[c(2, 22, 33, 89)] = 1
Percent(true, test)
#or
#percent(table(true, test))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
