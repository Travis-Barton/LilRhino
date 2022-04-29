\name{Load_Glove_Embeddings}
\alias{Load_Glove_Embeddings}
\title{Function for loading in pre-trained or personal word embedding softwares.
}
\description{Loads in GloVes' pretrained 42 billion token embeddings, trained on the common crawl.
}
\usage{
Load_Glove_Embeddings(path = 'glove.42B.300d.txt', d = 300)
}
\arguments{
  \item{path}{
  The path to the embeddings file.
}
  \item{d}{
  The dimension of the embeddings file.
}
}
\details{The embeddings file should be the word, followed by numeric values, ending with a carriage return.
}
\value{The embeddings matrix.

}

\author{Travis Barton
}

\examples{
#This code only works if you have the 5g file found here: <https://nlp.stanford.edu/projects/glove/>

\dontrun{emb = Load_Glove_Embeddings()}
}
