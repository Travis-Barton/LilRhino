\name{Feed_Reduction}
\alias{Feed_Reduction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A Function for converting data into approximations of probability space.
%%  ~~function to do ... ~~
}
\description{It takes the number of unique labels in the training data and tries to predict a one vs all binary neural network for each unique label. The output is an approximation of the probability that each individual input does not not match the label.
Travis Barton (2018) http://wbbpredictions.com/wp-content/uploads/2018/12/Redditbot_Paper.pdf
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Feed_Reduction(X, Y, X_test, val_split = .1,
               nodes = NULL, epochs = 15,
               batch_size = 30, verbose = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{ Training data
%%     ~~Describe \code{x} here~~
}
  \item{Y}{Training labels}
  \item{X_test}{Testing data}
  \item{val_split}{The validation split for the keras, binary, neural networks}
  \item{nodes}{The number nodes for the hidden layers, default is 1/4 of the length of the training data.}
  \item{epochs}{The number of epochs for the fitting of the networks}
  \item{batch_size}{The batch size for the networks}
  \item{verbose}{Weither or not you want details about the run as its happening. 0 = silent, 1 = progress bar, 2 = one line per epoch.}
}
\details{This is a new technique for dimensionality reduction of my own creation. Data is converted to the same number of dimensions as there are unique labels. Each dimension is an approximation of the probability that the data point is inside the a unique label. The return value is a list the training and test data with their dimensionality reduced.
}
\value{
  \item{Train}{The training data in the new probability space}
  \item{Test}{The testing data in the new probability space}

}
\references{Check out http://wbbpredictions.com/wp-content/uploads/2018/12/Redditbot_Paper.pdf for details on the proccess
%% ~put references to the literature/web site here ~
}
\author{Travis Barton.
%%  ~~who you are~~
}
\seealso{Binary_Network
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\donttest{
if(8 * .Machine$sizeof.pointer == 64){
#Feed Network Testing
library(keras)

  install_keras()
  dat <- keras::dataset_mnist()
  X_train = array_reshape(dat$train$x/255, c(nrow(dat$train$x/255), 784))
  y_train = dat$train$y
  X_test = array_reshape(dat$test$x/255, c(nrow(dat$test$x/255), 784))
  y_test = dat$test$y

  Reduced_Data2 = Feed_Reduction(X_train, y_train, X_test,
                                val_split = .3, nodes = 350,
                                30, 50, verbose = 1)

  library(e1071)
  names(Reduced_Data2$test) = names(Reduced_Data2$train)
  newdat = as.data.frame(cbind(rbind(Reduced_Data2$train, Reduced_Data2$test), c(y_train, y_test)))
  colnames(newdat) = c(paste("V", c(1:11), sep = ""))
  mod = svm(V11~., data = newdat, subset = c(1:60000),
           kernel = 'linear', cost = 1, type = 'C-classification')
  preds = predict(mod, newdat[60001:70000,-11])
  sum(preds == y_test)/10000

}
}
}
