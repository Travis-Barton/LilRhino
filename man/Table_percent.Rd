\name{Table_percent}
\alias{Table_percent}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Table Percent
%%  ~~function to do ... ~~
}
\description{ Finds the acc of square tables.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Table_percent(in_table)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in_table}{ a confusion matrix
%%     ~~Describe \code{x} here~~
}
}
\details{ The table must be square
%%  ~~ If necessary, more details than the description above ~~
}


\author{ Travis Barton
%%  ~~who you are~~
}
\note{ make sure its square.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{

true <- rep(1:10, 10)
test <- rep(1:10, 10)
test[c(2, 22, 33, 89)] = 1
Table_percent(table(true, test))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
