require(DescTools)
require(xgboost)
require(randomForest)

LinScale <- function(x, low, high, newlow, newhigh, reverse = F) {
  if(reverse){
    x <- -1 * x + high
  }
  maxx <- high
  minx <- low
  out <- (newhigh - newlow) * (x - minx)
  out <- out / (maxx - minx)
  out + newlow
}

# null model function
null_evlaute <- function(data){
  data[,1] <- LinScale(data[,1], low = 4.6, high = 15.9, newlow = 3, newhigh = 8)
  data[,2] <- LinScale(data[,2], low = 0.12, high = 1.58, newlow = 3, newhigh = 8, reverse = T)
  data[,3] <- LinScale(data[,3], low = 0, high = 1, newlow = 3, newhigh = 8)
  data[,4] <- LinScale(data[,4], low = 0.9, high = 15.5, newlow = 3, newhigh = 8)
  data[,5] <- LinScale(data[,5], low = 0.012, high = 0.611, newlow = 3, newhigh = 8, reverse = T)
  data[,6] <- LinScale(data[,6], low = 1, high = 72, newlow = 3, newhigh = 8)
  data[,7] <- LinScale(data[,7], low = 6, high = 289, newlow = 3, newhigh = 8)
  data[,8] <- LinScale(data[,8], low = 0.9907, high = 1.00369, newlow = 3, newhigh = 8)
  data[,9] <- LinScale(data[,9], low = 3.18, high = 2.74, newlow = 3, newhigh = 8)
  data[,10] <- LinScale(data[10], low = 0.33, high = 2, newlow = 3, newhigh = 8)
  data[,11] <- LinScale(data[,11], low = 8.4, high = 14.9, newlow = 3, newhigh = 8)
  
  rowMeans(data)
}

recall <- function(guess, reference){
  v = sapply(3:8, function(i)sum((guess %in% i) & (reference %in% i))/ sum(reference %in% i))
  v[is.na(v)] <- 1
  sum(v) / 6
}

# cross validation
cv <- function(evulate_type, test, val, train){
  # model using xgboost
  #bst <- xgboost(data = train[,-12], label = train[,12], max.depth = 6, eta = 1,
  #               nround = 100, objective = "multi:softmax", num_class = 9)
  
  # predit
  # bst <- readRDS(file = "xgboost.rds")
  if(evulate_type == "recall"){
    list("result" = c(recall(round(null_evlaute(train[,-12])), train[,12]),
                      recall(round(null_evlaute(train[,-12])), val[,12]),
                      recall(round(null_evlaute(train[,-12])), test[,12])),
         "model" = cv)
  } else{
    list("result" = c(sum(train[,12] %in% round(null_evlaute(train[,-12])))/length(train[,12]),
                      sum(val[,12] %in% round(null_evlaute(val[,-12])))/length(val[,12]),
                      sum(test[,12] %in% round(null_evlaute(test[,-12])))/length(test[,12])),
         "model" = cv)
  }
}

# use k fold cross validation to evulate predit model
evulate <- function(evulate_type, df, k){
  # shuffle and split rows
  df <- df[sample(nrow(df)),]
  datas <- df#data.matrix(df)
  
  # cross validation
  nr = nrow(datas)
  each <- ceiling(nr/k)
  result <- data.frame(training=double(), validation=double(), test=double())
  max <- 0
  for(i in c(1:(k-1))){
    tmp <- cv(evulate_type, datas[((i - 1) * each + 1):(i * each),],
              datas[(i * each + 1):min(c(((i + 1) * each), nr)), ],
              datas[-(((i - 1) * each + 1):((i + 1) * each)), ])
    result[nrow(result) + 1,] <- tmp[["result"]]
    print(result)
    if(max < result[nrow(result), 3]){
      model <- tmp[["model"]]
      max <- result[nrow(result), 3]
    }
  }
  tmp <- cv(evulate_type, datas[((k - 1) * each + 1):nr, ],
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
  } else if (!exists("evulate_type")){
    evulate_type = "accurate"
  }
  
  # read file and clean up
  train_df = read.csv(train)
  
  # cross validation
  tmp <- evulate(evulate_type, train_df, strtoi(fold))
  cv_result <- tmp[["result"]]
  bst <- tmp[["model"]]
  
  # add avg.
  cv_result[nrow(cv_result) + 1,] = apply(cv_result, 2, function(x)mean(x))
  set <- sapply(c(1:strtoi(fold)), function(x)sprintf("fold%d", x))
  set <- append(set, c("ave."))
  cv_result <- cbind(set = set, cv_result, row.names = NULL)
  
  # write file
  handle_output(cv_result, report)
  saveRDS(bst, "nullModel.rds")
}

# read input
args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0){
  # local test
  system("Rscript null_model.R --fold 5 --train winequality-red.csv --report nullModel.rds_performance_recall.csv --evulate recall")
} else {
  handle_data(args)
}