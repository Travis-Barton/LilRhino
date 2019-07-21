\name{Vector_Puller}
\alias{Vector_Puller}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function for extacting word vectors from embeddings.
%%  ~~function to do ... ~~
}
\description{Function for extacting word vectors from embeddings. This function is an internal function for 'Sentence_Puller'. It averages the word vectors and returns the average of these vectors.
}
\usage{
Vector_Puller(words, emb_matrix, dimension)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{words}{The word to be extracted.
%%     ~~Describe \code{x} here~~
}
  \item{emb_matrix}{The embeddings matrix. It must be a data frame.
%%     ~~Describe \code{x} here~~
}
  \item{dimension}{The Dimension of the embeddings to extract. They do not have to match that of the matrix, but they cannot exceed its maximum column count.
%%     ~~Describe \code{x} here~~
}
}
\details{This is a simple and fast internal function.
%%  ~~ If necessary, more details than the description above ~~
}
\value{The vector that corresponds to the average of the word vectors.
}

\author{Travis Barton
}

\examples{
# This is an example emb_matrix

emb = data.frame(matrix(c(1, 2, 3, 4, 5, 5, 4, 3, 2, 1), nrow = 2), row.names = c('cow', 'moo'))

Vector_Puller(c('cow', 'moo'), emb, 5)

}
