\name{Pretreatment}
\alias{Pretreatment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pretreatment of textual documents for NLP.
}
\description{
This function goes through a number of pretreatment steps in preparation for vectorization. These steps are designed to help the data become more standard so that there are fewer outliers when training during NLP. The following effects are applied:
1. Non-alpha/numerics are removed.
2. Numbers are separated from letters.
3. Numbers are replaced with their word equivalents.
4. Words are stemmed (optional).
5. Words are lowercased (optinal).
}
\usage{
Pretreatment(title_vec, stem = TRUE, lower = TRUE, parallel = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{title_vec}{
Vector of documents to be pre-treated.
}
  \item{stem}{
  Boolian variable to decide whether to stem or not.
  }
  \item{lower}{
  Boolian variable to decide whether to lowercase words or not.
  }
  \item{parallel}{
  Boolian variable to decide whether to run this function in parallel or not.
  }
}
\details{
This function returns a list. It should be able to accept any format that the function lapply would accept. The parallelization is done with the function Mcapply from the package 'parallel' and will only work on systems that allow forking (Sorry windows users). Future updates will allow for socketing.
}
\value{

  \item{output }{The list of character strings post-pretreatment}
}
\author{
Travis Barton
}

\examples{
\dontrun{ # for some reason it takes longer than 5 seconds on CRAN's computers
test_vec = c('This is a test', 'Ahoy!', 'my battle-ship is on... b6!')
res = Pretreatment(test_vec)
print(res)
}
}
