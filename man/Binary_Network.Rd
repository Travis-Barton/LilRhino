\name{Binary_Network}
\alias{Binary_Network}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Binary Decision Neural Network Wrapper
%%  ~~function to do ... ~~
}
\description{Used as a function of Feed_Reduction, Binary_Networt uses a 3 layer neural network with an adam optimizer, leaky RELU for the first two activation functions, followed by a softmax on the last layer. The loss function is binary_crossentropy. This is a keras wrapper, and uses tensorflow in the backend.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Binary_Network(X, Y, X_test, val_split, nodes, epochs, batch_size, verbose = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{Training data.
%%     ~~Describe \code{X} here~~
}
  \item{Y}{Training Labels. These must be binary.
%%     ~~Describe \code{Y} here~~
}
  \item{X_test}{The test Data
%%     ~~Describe \code{X_test} here~~
}
  \item{val_split}{The validation split for keras.
%%     ~~Describe \code{val_split} here~~
}
  \item{nodes}{The number of nodes in the hidden layers.
%%     ~~Describe \code{nodes} here~~
}
  \item{epochs}{The number of epochs for the network
%%     ~~Describe \code{epochs} here~~
}
  \item{batch_size}{ The batch size for the network
%%     ~~Describe \code{batch_size} here~~
}
  \item{verbose}{ Weither or not you want details about the run as its happening. 0 = silent, 1 = progress bar, 2 = one line per epoch.
%%     ~~Describe \code{verbose} here~~
}
}
\details{This function is a subset for the larger function Feed_Network. The output is the list containing the training and testing data converted into an approximation of probability space for that binary decision.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{Train}{The training data in approximate probability space}
\item{Test}{The testing data in 'double' approximate probability space}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Check out http://wbbpredictions.com/wp-content/uploads/2018/12/Redditbot_Paper.pdf and Keras for details
%% ~put references to the literature/web site here ~
}
\author{ Travis Barton
%%  ~~who you are~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ Feed_Network
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\donttest{
if(8 * .Machine$sizeof.pointer == 64){
  #Feed Network Testing
  library(keras)
  library(dplyr)
    install_keras()
    dat <- keras::dataset_mnist()
    X_train = array_reshape(dat$train$x/255, c(nrow(dat$train$x/255), 784))
    y_train = to_categorical(dat$train$y, 10)
    X_test = array_reshape(dat$test$x/255, c(nrow(dat$test$x/255), 784))
    y_test =to_categorical(dat$test$y, 10)


    index_train = which(dat$train$y == 6 | dat$train$y == 5)
    index_train = sample(index_train, length(index_train))
    index_test = which(dat$test$y == 6 | dat$test$y == 5)
    index_test = sample(index_test, length(index_test))

    temp = Binary_Network(X_train[index_train,],
    y_train[index_train,c(7, 6)], X_test[index_test,], .3, 350, 30, 50)
  }
  }
}


\keyword{NeuralNetwork}
\keyword{FeedNetwork}
