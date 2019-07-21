\name{Num_Al_Sep}
\alias{Num_Al_Sep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Number/alpha numeric seperator for strings.
}
\description{
A Function for the separating of numbers from letters. 'b4' for example would be converted to 'b 4'.
}
\usage{
Num_Al_Sep(vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{
The string vector in which you wish to separate the numbers from the letters.
}
}

\value{

 \item{output}{The separated vector.}
}

\author{
Travis Barton
}
\note{
This is a really simple function really used inside other functions.
}


\examples{
test_vec = 'The most iconic American weapon has to be the AR15'
res = Num_Al_Sep(test_vec)
print(res)
}
