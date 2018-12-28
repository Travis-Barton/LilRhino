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

Codes_done <- function(title, msg, sound = FALSE)
{
  theTitle <- title
  theMsg <- msg

  cmd <- paste("osascript -e ", "'display notification ", '"', theMsg, '"', ' with title ', '"', theTitle, '"', "'", sep='')
  system(cmd)
  if(sound == T)
  {
    beepr::beep()
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
    neighbors[[i]] <- get.knnx(indexindex[[i]], X_test,  k=11, algorithm=c("kd_tree"))$nn.index[,-1]
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


