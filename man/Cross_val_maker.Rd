\name{Cross_val_maker}
\alias{Cross_val_maker}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ For Creating a test and train set from a whole set
%%  ~~function to do ... ~~
}
\description{ for making one dataset into two (test and train)

}
\usage{
Cross_val_maker(data, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ matrix of data you want to split}
  \item{alpha}{ the percent of data to split
%%     ~~Describe \code{x} here~~
}
}

\value{ returns a list with accessable with the '$' sign. Test and Train are labeled as such.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{ Travis Barton
%%  ~~who you are~~
}

\examples{
dat <- Cross_val_maker(iris, .1)
train <- dat$Train
test <- dat$Test
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
