setwd("/Users/lukakrstev/YelpChallenge")
load(file = "restoraunts_df.Rda")
load(file = "YelpDataframes/review_df.Rda")
load(file = "YelpDataframes/user_df.Rda")
load(file = "YelpDataframes/busines_df.Rda")
setwd("/Users/lukakrstev/YelpChallenge/YelpRecommender/")


library(dplyr)


restoraunt_reviews <- semi_join(review_df[,c(2,4,8)],restoraunts_df, by = "business_id")
#revievs only for Las Vegas, Montral, Edinburg, Charlotte and Pittsburgh seperately
restoraunt_reviews_las_vegas <- semi_join(review_df[,c(2,4,8)],subset(restoraunts_df,state =="Las Vegas"), by = "business_id")
restoraunt_reviews_montreal <- semi_join(review_df[,c(2,4,8)],subset(restoraunts_df,state =="Montreal"), by = "business_id")
restoraunt_reviews_edinburgh <- semi_join(review_df[,c(2,4,8)],subset(restoraunts_df,state =="Edinburgh"), by = "business_id")
restoraunt_reviews_charlotte <- semi_join(review_df[,c(2,4,8)],subset(restoraunts_df,state =="Charlotte"), by = "business_id")
restoraunt_reviews_pittsburgh <- semi_join(review_df[,c(2,4,8)],subset(restoraunts_df,state =="Pittsburgh"), by = "business_id")


options(scipen = 999)
prop.table(table(cut(user_df$review_count,c(0,0.5,1,5,10,20,Inf),labels = c("0","1","2-5","6-10","11-20","20+") ,include.lowest = TRUE)))
mean(user_df$review_count)

library(data.table)
library(recommenderlab)

evaluation_recommenderlab <- function(df){
  toRealRatingMatrix <- function(df1){
    
    #posto sam uocio da ima 50k duplikata, tj vise ocena jednog usera za jedan restoran
    keys <- c("user_id","business_id")
    X <- as.data.table(df1)
    Y <- as.data.frame(X[,list(stars= mean(stars)),keys])
    m <- xtabs(stars~., Y, sparse = TRUE)
    
    r <- new("realRatingMatrix", data = m)
    r
  }
  
  
  r <- toRealRatingMatrix(df)  
  
  eval_scheme <- evaluationScheme(r, method = "cross-validation", k = 10,given = 1)
  #eval_scheme <- evaluationScheme(r_ed, method="split", train=0.9, given=1)
  
  r1 <- Recommender(getData(eval_scheme, "train"), method="UBCF",
                    param=list(normalize = "Z-score",method="Jaccard", nn=50))
  #r1
  
  r2 <- Recommender(getData(eval_scheme, "train"), method="POPULAR")
  #r2
  
  r3 <- Recommender(getData(eval_scheme, "train"),method="IBCF", param=list(normalize = "Z-score",method="Jaccard",nn=50))
  #r3
  
  r4 <- Recommender(getData(eval_scheme, "train"), method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=50))
  #r4
  
  
  p1 <- predict(r1, getData(eval_scheme, "known"), type="ratings")
  #p1
  
  p2 <- predict(r2, getData(eval_scheme, "known"), type="ratings")
  #p2
  
  p3 <- predict(r3, getData(eval_scheme, "known"), type="ratings")
  #p3
  
  p4 <- predict(r4, getData(eval_scheme, "known"), type="ratings")
  #p4
  
  
  #calculate error between the prediction and the unknown part of the test data
  error <- rbind(
    calcPredictionAccuracy(p1, getData(eval_scheme, "unknown")),
    calcPredictionAccuracy(p2, getData(eval_scheme, "unknown")),
    calcPredictionAccuracy(p3, getData(eval_scheme, "unknown")),
    calcPredictionAccuracy(p4, getData(eval_scheme, "unknown"))
  )
  
  rownames(error) <- c("UBCF-Jaccard","POPULAR","IBCF-Jaccard","UBCF-Cosine")
  error
  
}

index_of_matrix <- function(ind, m){
  ij <- rep(0,2)
  ij[1] <- ind - floor(ind / nrow(m)) *nrow(m)
  if(ij[1]==0){ij[1]=1}
  ij[2] <- ceiling(ind / nrow(m))
  ij
}
rmse <- function(error){
  sqrt(mean(error^2))
}
nzmean <- function(x) {
  zvals <- x==0
  if (all(zvals)) 0 else mean(x[!zvals])
}
overall_rmse <- function(x){
  sum_of_squares <- 0
  for(i in 1:length(x)){
    sum_of_squares = sum_of_squares + x[i]^2
  }
  sqrt(sum_of_squares/length(x))
}


