\name{Bootstrap_Vocab}
\alias{Bootstrap_Vocab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{An internal function for Bootstrap_Data_Frame.
%%  ~~function to do ... ~~
}
\description{This function takes a selection of documents and bootstraps words from said sentences until there are N total sentences (both sudo and original).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Bootstrap_Vocab(vocab, N, stopwds, min_length = 7, max_length = 15)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vocab}{The collection of documents to boostrap.
%%     ~~Describe \code{x} here~~
}
  \item{N}{The total amount of sentences to end up with}
  \item{stopwds}{A list of stopwords to not include in the bootstrapping proccess}
  \item{min_length}{The shortest allowable bootstrapped doument}
  \item{max_length}{The longest allowable bootstrapped document}
}
\details{The min and max length arguements to not gaurantee that a sentence will reach that length. These senteces will be nonsensical.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A vector of bootstrapped sentences.
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


testing_set = c(paste('this is test',  as.character(seq(1, 10, 1))))

Bootstrap_Vocab(testing_set, 20, c('this'))

}
