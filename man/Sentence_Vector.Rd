\name{Sentence_Vector}
\alias{Sentence_Vector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for extracting the sentence vector from an embeddings matrix.
%%  ~~function to do ... ~~
}
\description{Function for extracting the sentence vector from an embeddings matrix in a fast and convenient manner.
}
\usage{
Sentence_Vector(Sentence, emb_matrix, dimension, stopwords)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Sentence}{The sentence to find the vector of.
%%     ~~Describe \code{x} here~~
}
  \item{emb_matrix}{The embeddings matrix to search.
%%     ~~Describe \code{x} here~~
}
  \item{dimension}{The dimension of the vector to return.
%%     ~~Describe \code{x} here~~
}
  \item{stopwords}{Words that should not be included in the averaging proccess.
%%     ~~Describe \code{x} here~~
}
}
\details{The function splits the sentence into words, eliminates all stopwords, finds the vectors of each word, then averages the word vectors into a sentence vector.
%%  ~~ If necessary, more details than the description above ~~
}
\value{The sentence vector from an embeddings matrix.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Travis Barton
%%  ~~who you are~~
}

\examples{
  emb = data.frame(matrix(c(1, 2, 3, 4, 5, 5,
  4, 3, 2, 1, 1, 5, 3, 2, 4), nrow = 3),
  row.names = c('sentence', 'in', 'question'))

  Sentence_Vector(c('this is the sentence in question'), emb, 5, c('this', 'is', 'the'))


}
