#require(xgboost)
#require(randomForest)
require(rpart)

# cross validation
cv <- function(test, val, train){
  # model using xgboost
  #bst <- xgboost(data = train[,-12], label = train[,12], max.depth = 6, eta = 1,
  #               nround = 100, objective = "multi:softmax", num_class = 9)

  # predit
  #list("result" = c(sum(train[,12] %in% predict(bst, train[,-12]))/length(train[,12]),
  #                  sum(val[,12] %in% predict(bst, val[,-12]))/length(val[,12]),
  #                  sum(test[,12] %in% predict(bst, test[,-12]))/length(test[,12])),
  #     "model" = bst)

  # model using random forest and desion tree
  bst <- rpart(quality ~ ., data = train)
  print((train[,12]))
  print((predict(bst, train)))
  list("result" = c(sum(train[,12] %in% round(predict(bst, train)))/length(train[,12]),
                    sum(val[,12] %in% round(predict(bst, val)))/length(val[,12]),
                    sum(test[,12] %in% round(predict(bst, test)))/length(test[,12])),
       "model" = bst)  
  
}

# use k fold cross validation to evulate predit model
evulate <- function(df, k){
  # shuffle and split rows
  df <- df[sample(nrow(df)),]
  datas <- df#data.matrix(df)
  
  # cross validation
  nr = nrow(datas)
  each <- ceiling(nr/k)
  result <- data.frame(training=double(), validation=double(), test=double())
  max <- 0
  for(i in c(1:(k-1))){
    tmp <- cv(datas[((i - 1) * each + 1):(i * each),],
              datas[(i * each + 1):min(c(((i + 1) * each), nr)), ],
              datas[-(((i - 1) * each + 1):((i + 1) * each)), ])
    result[nrow(result) + 1,] <- tmp[["result"]]
    print(result)
    if(max < result[nrow(result), 3]){
      model <- tmp[["model"]]
      max <- result[nrow(result), 3]
    }
  }
  tmp <- cv(datas[((k - 1) * each + 1):nr, ],
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
  }
  
  # read file and clean up
  train_df = read.csv(train)

  # cross validation
  tmp <- evulate(train_df, strtoi(fold))
  cv_result <- tmp[["result"]]
  bst <- tmp[["model"]]
  
  # add avg.
  cv_result[nrow(cv_result) + 1,] = apply(cv_result, 2, function(x)mean(x))
  set <- sapply(c(1:strtoi(fold)), function(x)sprintf("fold%d", x))
  set <- append(set, c("ave."))
  cv_result <- cbind(set = set, cv_result, row.names = NULL)
  
  # write file
  handle_output(cv_result, report)
  saveRDS(bst, "CART.rds")
}

# read input
args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0){
  # local test
  system("Rscript final.R --fold 5 --train winequality-red.csv --report CART_performance.csv")
} else {
  handle_data(args)
}