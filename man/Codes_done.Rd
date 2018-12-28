\name{Codes_done}
\alias{Codes_done}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ For announcing when code is done.
%%  ~~function to do ... ~~
}
\description{ for alerting you when your code is done.

}
\usage{
Codes_done(title, msg, sound = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{title}{ The title of the notification}
  \item{msg}{ The message to be sent}
  \item{sound}{ Optional sound to blurt as well
%%     ~~Describe \code{x} here~~
}
}
\details{ Only for Linix (as far as I know)
%%  ~~ If necessary, more details than the description above ~~
}

\references{ https://stackoverflow.com/questions/3365657/is-there-a-way-to-make-r-beep-play-a-sound-at-the-end-of-a-script
%% ~put references to the literature/web site here ~
}
\author{ smacdonald (stack overflow) with modificaion by Travis Barton
%%  ~~who you are~~
}

\examples{
Codes_done("done", "check it", sound = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
