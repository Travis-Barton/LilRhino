\name{Stopword_Maker}
\alias{Stopword_Maker}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
For the finding of the $N$ most populous words in a corpus.
}
\description{
This function finds the $N$ most used words in a corpus. This is done to identify stop words to better prune data sets before training.
}
\usage{
Stopword_Maker(titles, cutoff = 20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{titles}{
The documents in which the most populous words are sought.
}
  \item{cutoff}{
The number of $N$ top most used words to keep as stop words.
}
}

\value{

\item{output }{A vector of the $N$ most populous words.}

}
\author{
Travis Barton
}

\examples{
test_set = c('this is a testset', 'I am searching for a list of words',
'I like turtles',
'A rocket would be a fast way of getting to work, but I do not think it is very practical')
res = Stopword_Maker(test_set, 4)
print(res)
}
