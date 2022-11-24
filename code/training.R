require(xgboost)
require(randomForest)
require(rpart)

recall <- function(guess, reference){
  v = sapply(3:8, function(i)sum((guess %in% i) & (reference %in% i))/ sum(reference %in% i))
  v[is.na(v)] <- 1
  sum(v) / 6
}

# cross validation
cv <- function(evulate_type, method, test, val, train){
  if(method == "xgboost"){
    # model using xgboost
    bst <- xgboost(data = train[,-12], label = train[,12], max.depth = 6, eta = 1,
                   nround = 10, objective = "multi:softmax", num_class = 9)    
  } else if (method == "randomForest"){
    bst <- randomForest(quality ~ ., data = train)
  } else {
    # model using random forest and desion tree
    bst <- rpart(quality ~ ., data = train)  
  }
  
  if(evulate_type == "recall"){
    if(method == "xgboost"){
      list("result" = c(recall(round(predict(bst, train[,-12])), train[,12]),
                        recall(round(predict(bst, val[,-12])), val[,12]),
                        recall(round(predict(bst, test[,-12])), test[,12])),
           "model" = bst)
    } else{
      list("result" = c(recall(round(predict(bst, train)), train[,12]),
                        recall(round(predict(bst, val)), val[,12]),
                        recall(round(predict(bst, test)), test[,12])),
           "model" = bst) 
    }
  } else{
    if(method == "xgboost"){
      list("result" = c(sum(train[,12] %in% round(predict(bst, train[,-12])))/length(train[,12]),
                        sum(val[,12] %in% round(predict(bst, val[,-12])))/length(val[,12]),
                        sum(test[,12] %in% round(predict(bst, test[,-12])))/length(test[,12])),
           "model" = bst) 
    } else{
      list("result" = c(sum(train[,12] %in% round(predict(bst, train)))/length(train[,12]),
                        sum(val[,12] %in% round(predict(bst, val)))/length(val[,12]),
                        sum(test[,12] %in% round(predict(bst, test)))/length(test[,12])),
           "model" = bst) 
    }
  }
}

# use k fold cross validation to evulate predit model
evulate <- function(evulate_type, method, df, k){
  # shuffle and split rows
  df <- df[sample(nrow(df)),]
  if(method == "xgboost"){
    datas <- data.matrix(df)
  } else{
    datas <- df
  }
  
  # cross validation
  nr = nrow(datas)
  each <- ceiling(nr/k)
  result <- data.frame(training=double(), validation=double(), test=double())
  max <- 0
  for(i in c(1:(k-1))){
    tmp <- cv(evulate_type, method, datas[((i - 1) * each + 1):(i * each),],
              datas[(i * each + 1):min(c(((i + 1) * each), nr)), ],
              datas[-(((i - 1) * each + 1):((i + 1) * each)), ])
    result[nrow(result) + 1,] <- tmp[["result"]]
    print(result)
    if(max < result[nrow(result), 3]){
      model <- tmp[["model"]]
      max <- result[nrow(result), 3]
    }
  }
  tmp <- cv(evulate_type, method, datas[((k - 1) * each + 1):nr, ],
            datas[1:each, ],
            datas[(each + 1):((k-1) * each), ])
  result[nrow(result) + 1,] <- tmp[["result"]]
  if(max < result[nrow(result), 3]){
    model <- tmp[["model"]]
  }
  list("result" = result, "model" = model)
}

handle_output <- function(out_df, out_files){
  # write file
  out_files <- file.path(out_files)
  path <- strsplit(out_files, "/")[[1]]
  out_dir <- paste(path[1:length(path)-1], collapse = "/")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(out_df, file = out_files, row.names = FALSE, quote = FALSE)  
}

# handle input and output
handle_data <- function(args){
  # check args
  i<-1 
  while(i < length(args))
  {
    if(args[i] == "--fold"){
      fold<-args[i+1]
      i<-i+1
    }else if(args[i] == "--train"){
      train<-args[i+1]
      i<-i+1
    }else if(args[i] == "--report"){
      report<-args[i+1]
      i<-i+1
    }else if(args[i] == "--method"){
      method<-args[i+1]
      i<-i+1
    }else if(args[i] == "--evulate"){
      evulate_type<-args[i+1]
      i<-i+1
    }else{
      stop(paste("Unknown flag", args[i]), call.=FALSE)
    }
    i<-i+1
  }
  if(!exists("fold")){
    stop("missing argument --fold ", call.=FALSE)
  } else if (!exists("train")){
    stop("missing argument --train ", call.=FALSE)
  } else if (!exists("report")){
    stop("missing argument --report ", call.=FALSE)
  } else if (!exists("method")){
    stop("missing argument --method ", call.=FALSE)
  } else if (!exists("evulate_type")){
    evulate_type = "accurate"
  }
  
  # read file and clean up
  train_df = read.csv(train)

  # cross validation
  tmp <- evulate(evulate_type, method, train_df, strtoi(fold))
  cv_result <- tmp[["result"]]
  bst <- tmp[["model"]]
  
  # add avg.
  cv_result[nrow(cv_result) + 1,] = apply(cv_result, 2, function(x)mean(x))
  set <- sapply(c(1:strtoi(fold)), function(x)sprintf("fold%d", x))
  set <- append(set, c("ave."))
  cv_result <- cbind(set = set, cv_result, row.names = NULL)
  
  # write file
  handle_output(cv_result, report)
  saveRDS(bst, paste0(method, ".rds"))
}

# read input
args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0){
  # local test
  system("Rscript training.R --fold 5 --train winequality-red.csv --report randomForest_performance_recall.csv --method randomForest --evulate recall")
} else {
  handle_data(args)
}