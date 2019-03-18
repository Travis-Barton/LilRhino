#Travis's Personally Useful Functions:
Percent <- function(true, test)
{
  return(sum(diag(table(true, test)))/sum(table(true, test)))
}

Table_percent <- function(in_table)
{
  return(sum(diag(in_table))/sum(in_table))
}

Cross_val_maker <- function(data, alpha)
{
  if(alpha > 1 || alpha <= 0)
  {
    return("Alpha must be between 0 and 1")
  }
  index <- sample(c(1:nrow(data)), round(nrow(data)*alpha))
  train <- data[-index,]
  test <- data[index,]
  return(list("Train" = as.data.frame(train), "Test" = as.data.frame(test)))
}

Codes_done <- function(title = "Codes Done", msg = " ", sound = FALSE, effect = 1)
{
  theTitle <- title
  theMsg <- msg
  if (Sys.info()["sysname"] == "Darwin")
  {
    cmd <- paste("osascript -e ", "'display notification ", '"', theMsg, '"', ' with title ', '"', theTitle, '"', "'", sep='')
    system(cmd)
  }
  else if(.Platform$OS.type == "windows")
  {
    system('CMD /C "ECHO The R process has finished running && PAUSE"',
           invisible=FALSE, wait=FALSE)
  }
  else if(.Platform$OS.type == "unix")
  {
    cmd <- paste("osascript -e ", "'display notification ", '"', theMsg, '"', ' with title ', '"', theTitle, '"', "'", sep='')
    system(cmd)
  }
  else
  {
    print(title)
    print(msg)
  }

  if(sound == T)
  {
    beepr::beep(effect)
  }
}

Nearest_Centroid <- function(X_train, X_test, Y_train)
{
  names(X_test) = names(X_train)
  results = matrix(0, nrow(X_test), 10)
  indexindex = vector("list", length = 10)
  neighbors = list(vector("list", length = 10))
  s = c(rep(NA, 10))
  time = c(rep(0, 10))
  for(i in 1:10){
    indexindex[[i]] <- X_train[which(Y_train == (i-1)), ]
    neighbors[[i]] <- FNN::get.knnx(indexindex[[i]], X_test,  k=11, algorithm=c("kd_tree"))$nn.index[,-1]
  }
  for(i in 1:nrow(X_test)){
    point_mat <- matrix(0, 10, 10)
    for(k in 1:10){
      s[k] = Sys.time()
      if(k == 1)
      {
        for(l in 1:10){
          candidate = (indexindex[[l]][neighbors[[l]][i, 1:k],])
          point_mat[l, k] = sqrt(sum((X_test[i,] - candidate)^2))
        }
      }
      else{
        for(l in 1:10){
          candidate = apply(indexindex[[l]][neighbors[[l]][i, 1:k],],2, mean)
          point_mat[l, k] = sqrt(sum((X_test[i,] - candidate)^2))
        }
      }
      results[i, k] = max(which(point_mat[, k] == min(point_mat[,k]))) - 1
      s[k] = Sys.time() - s[k]
    }
    time = rbind(time, s)
  }
  return(results)
  #return(list(res = results, time = time))
}

Monty_Hall = function(Games = 10, Choice = "Stay")
{
  choice1 = function()
  {
    return(sample(c(1, 2, 3), 1))
  }

  choice2 = function(Choice = "Random")
  {
    if(Choice == "Random"){
      return(sample(c(1, 2), 1))
    }
    else if (Choice == "Stay")
    {
      return(1)
    }
    else if (Choice == "Switch")
    {
      return(2)
    }
    else
    {
      print("You must choose a valid choice")
    }
  }


  Single_Game = function(Choice = "Random")
  {
    winning_door = sample(c(1, 2, 3), 1)
    round1 = choice1()
    round2 = choice2(Choice)

    if(round2 == 1){
      if(round1 == winning_door){
        return(1)
      }
      else{
        return(0)
      }
    }
    else{
      if(round1 != winning_door){
        return(1)
      }
      else{
        return(0)
      }
    }

  }


  results = {}
  for (i in 1:Games) {
    results = c(results, Single_Game(Choice))

  }

  dat = data.frame(V1 = c(sum(results), Games-sum(results)), V2 = 	c("Win", "Lose"))
  ggplot2::ggplot(data = dat, ggplot2::aes(x = dat$V2, y = dat$V1)) +
    ggplot2::geom_bar(stat = "identity", fill = c("pink", "light blue")) +
    ggplot2::geom_text(ggplot2::aes(label = dat$V1), vjust = 1.1) +
    ggplot2::ylab("Game Count") + ggplot2::xlab(" ") +
    ggplot2::ggtitle("Heres how you did!") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = .5))
}


Binary_Network = function(X, Y, X_test, val_split, nodes, epochs, batch_size, verbose = 0)
{
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = nodes, input_shape = ncol(X)) %>%
    layer_activation_leaky_relu(alpha = .001) %>%
    layer_dropout(.4) %>%
    layer_dense(units = nodes) %>%
    layer_activation_leaky_relu(.001) %>%
    layer_dense(units = 2, activation = "softmax")
  model %>% compile(
    loss = 'binary_crossentropy',
    optimizer = optimizer_adam(),
    metrics = c('accuracy')
  )
  history <- model %>% fit(
    X, Y,
    epochs = epochs,
    batch_size = batch_size,
    validation_split = val_split
  )

  train = model %>% predict_proba(X)
  test = model %>% predict_proba(X_test)
  dat = list(train = train, test = test)
  return(dat)
}

Feed_Reduction = function(X, Y, X_test, val_split = .1, nodes = NULL, epochs = 15, batch_size = 30, verbose = 0)
{
  if(is.null(nodes) == TRUE){
    nodes = round(ncol(X)/4)
  }
  labels = sort(unique(Y), decreasing = F)
  final_train = matrix(0, nrow = nrow(X), ncol = length(labels))
  final_test = matrix(0, nrow = nrow(X_test), ncol = length(labels))
  i = 1
  for(label in labels){
    index = which(Y == label)
    y <- Y
    y[index] = 1
    y[-index] = 0
    y = to_categorical(y, 2)
    temp = Binary_Network(X, y, X_test, val_split, nodes, epochs, batch_size, verbose)
    final_train[, i] = temp$train[,1]
    final_test[, i] = temp$test[,1]
    i = i + 1

  }

  return(list(train = final_train, test = final_test))
}
