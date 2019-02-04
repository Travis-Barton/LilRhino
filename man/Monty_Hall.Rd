\name{Monty_Hall}
\alias{Monty_Hall}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Monty Hall Simulator
%%  ~~function to do ... ~~
}
\description{ A simulator for the famous Monty Hall Problem
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Monty_Hall(Games = 10, Choice = "Stay")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Games}{ The number of games to run on the simulation
%%     ~~Describe \code{x} here~~
}
  \item{Choice}{Wether you would like the simulation to either 'Stay' with the first chosen door, 'Switch' to the other door, or 'Random' where you randomly decide to either stay or switch.
  }
}
\details{ This is just a toy example of the famous Monty Hall problem. It returns a ggplot bar chart showing the counts for wins or loses in the simulation.
%%  ~~ If necessary, more details than the description above ~~
}
\value{ A ggplot graph is produced. There is no return value.
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
Monty_Hall(100, 'Stay')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
