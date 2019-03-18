\name{Cross_val_maker}
\alias{Cross_val_maker}
\title{ For Creating a test and train set from a whole set
}
\description{ for making one dataset into two (test and train)

}
\usage{
Cross_val_maker(data, alpha)
}
\arguments{
  \item{data}{ matrix of data you want to split}
  \item{alpha}{ the percent of data to split
}
}

\value{ returns a list with accessable with the '$' sign. Test and Train are labeled as such.
}

\author{ Travis Barton
}

\examples{
dat <- Cross_val_maker(iris, .1)
train <- dat$Train
test <- dat$Test
}
