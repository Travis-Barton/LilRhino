\name{Random_Brains}
\alias{Random_Brains}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Random Brains: Neural Network Implementation of Random Forest
%%  ~~function to do ... ~~
}
\description{Creates a random forest style collection of neural networks for classification
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Random_Brains(data, y, x_test,
variables = ceiling(ncol(data)/10),
brains = floor(sqrt(ncol(data))),
hiddens = c(3, 4))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{The data that holds the predictors ONLY.}
\item{y}{The responce variable}
\item{x_test}{The testing predictors}
\item{variables}{The number of predictors to select for each brain in 'data'. The default is one tenth of the number of columns in 'data'.}
\item{brains}{The number of neural networks to create. The default is the square root of the number of columns in 'data'.}
\item{hiddens}{The is a vector with length equal to the desired number of hidden layers. Each entry in the vector corresponds to the number of nodes in that layer. The default is c(3, 4) which is a two layer network with 3 and 4 nodes in the layers respectively.}
}

\details{This function is meant to mirror the classic random forest function exctly. The only difference being that it uses shallow neural networks to build the forest instead of decision trees.
%%  ~~ If necessary, more details than the description above ~~

}
\value{
\item{predictions}{The predictions for x_test.}
\item{num_brains}{The number of neural networks used to decide the predictions.}
\item{predictors_per_brain}{The number of variabled used for the neural networks used to decide the predictions.}
\item{hidden_layers}{The vector describing the number of layers, as well as how many there were.}
\item{preds_per_brain}{This matrix describes which columns where selected by each brain. Each row is a new brain. each column describes the index of the column used.}
\item{raw_results}{The matrix of raw predictions from the brains. Each row is the cummulative predictions of all the brains. Which prediciton won by majority vote can be seen in 'predictions}

}
\author{Travis Barton
%%  ~~who you are~~
}
\note{The neural networks are created using the neuralnet package!
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

dat = Cross_val_maker(iris, .2)

train = dat$Train
test = dat$Test

Final_Test = Random_Brains(train[,-5],
  train$Species, as.matrix(test[,-5]),
  variables = 3, brains = 2)
table(Final_Test$predictions, as.numeric(test$Species))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
