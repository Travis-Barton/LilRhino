\name{Bootstrap_Data_Frame}
\alias{Bootstrap_Data_Frame}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function for bootstraping textual data so that all levels have the same number of entries.
%%  ~~function to do ... ~~
}
\description{ This function takes a corpus and a set of labels and uses Bootstrap_Vocab to increase the size of each label until they are all the same length. Stop words are not bootstrapped.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Bootstrap_Data_Frame(text, tags, stopwords, min_length = 7, max_length = 15)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{text is the collection of textual data to bootstrap up.
%%     ~~Describe \code{x} here~~
}
  \item{tags}{tags are the collection of tags that will be used to bootstrap. There should be one for every entry in 'text'. They do not have to be unique.
  }
  \item{stopwords}{stopwords to make sure are not apart of the bootstrapping process. It is advised to eliminate the most common words. See Stop_Word_Maker()}
  \item{min_length}{The shortest length allowable for bootstrapped words}
  \item{max_length}{The longest length allowable for bootstrapped words}
}
\details{Most of the bootstrapped words will be nonseneical. The intention of this package is not to create new sentences, but to instead trick your model into thinking it has equal lengthed levels. This method is meant for bag of words style models.
%%  ~~ If necessary, more details than the description above ~~
}
\value{A data frame of your original documents along with the bootstrapped ones (column 1) along with their tags (column 2).
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{Travis Barton
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
test_set = c('I like cats', 'I like dogs', 'we love animals', 'I am a vet',
             'US politics bore me', 'I dont like to vote',
             'The rainbow looked nice today dont you think tommy')
test_tags = c('animals', 'animals', 'animals', 'animals',
             'politics', 'politics',
             'misc')

Bootstrap_Data_Frame(test_set, test_tags, c("I", "we"), min_length = 3, max_length = 8)
}
