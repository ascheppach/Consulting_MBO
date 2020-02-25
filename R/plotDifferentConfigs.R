plotDifferentConfigs = function (results, repls, names, iters, y.name, data.name) {
  test = as.data.frame(results$result)
  
  p = as.character(c(0:iters))
  x = NULL
  n =  length(names)
  nn = length(test)/n
  
  for (j in 1:n) {
    if (j == 1) {
      for (i in 1:(iters+1)) {
        x[[j]] = rbind(x[[j]], t(test[i,seq(1+(nn*(j-1)),nn+(nn*(j-1)),2)]))
      }
    }
    
    if (j >= 2) {
      x[[j]] = t(test[1,seq(1+(nn*(j-1)),nn+(nn*(j-1)),2)])
      for (i in 2:(iters+1)) {
        x[[j]] = rbind(x[[j]], t(test[i,seq(1+(nn*(j-1)),nn+(nn*(j-1)),2)]))
      }
    }
    x[[j]] = data.frame(x[[j]])
    x[[j]]$class = names[j]
    x[[j]]$iteration = c(rep(0:iters, each = repls))
    x[[j]]$iteration = ordered(x[[j]]$iteration, levels = p)
  }
  
  if (n == 6) {
    x = rbind(x[[1]],x[[2]],x[[3]],x[[4]],x[[5]],x[[6]])
  }
  if (n == 5) {
    x = rbind(x[[1]],x[[2]],x[[3]],x[[4]],x[[5]])
  }
  if (n == 4) {
    x = rbind(x[[1]],x[[2]],x[[3]],x[[4]])
  }
  if (n == 3) {
    x = rbind(x[[1]],x[[2]],x[[3]])
  }
  if (n == 2) {
    x = rbind(x[[1]],x[[2]])
  }
  if (n == 1) {
    x = rbind(x[[1]])
  }
  
  x$class = as.factor(x$class)
  
  ggplot(x, aes(x = iteration, y = X1, fill = class)) + geom_boxplot() +
    #stat_summary(fun.y = median, geom = "line", aes(group = 1)) +
    ylab(y.name) + labs(title = data.name)
}