load(file = "restoraunt_reviews.Rda")
object.size(restoraunt_reviews)
library(Matrix)
library(data.table)
library(recommenderlab)


overall_rmse <- function(x){
  sum_of_squares <- 0
  for(i in 1:length(x)){
    sum_of_squares = sum_of_squares + x[i]^2
  }
  sqrt(sum_of_squares/length(x))
}

#################
######MODEL######
#################




model_builder2 <- function(df){
  
  keys <- c("user_id","business_id")
  X <- as.data.table(df)
  Y <- as.data.frame(X[,list(stars= mean(stars)),keys])
  m_yelp <- xtabs(stars~., Y, sparse = TRUE)
  #napravljena je sparse matrica review-a, gde su u redovima useri a u kolonama restorani.
  
  r <- new("realRatingMatrix", data = m_yelp)
  # 'realRatingMatrica' iz koje cu lakse racunati colMean, rowMean i binarnu matricu.
  
  b <- binarize(r, minRating=1)
  #pravljenje binarne sparse matrice, 1 je gde ima ocene, a 0 gde nema
  avail_m_yelp <- as(b, "dgCMatrix")
  #vracanje u "dgCMatrix" tip 
  
  
  
  library(caret)
  flds <- createFolds(m_yelp@x,10)
  #pravljenje 10 foldova uz pomoc `caret` paketa
  
  cv_df <- data.frame("RMSE_Mean_Plus"=NA, "RMSE_Mean"=NA)
  #data.frame u koji cu smestati RMSE svakog folda
  
  for(i in 1:length(flds)){
    #pravljenje logickog vektora uz za i-ti fold
    ind_log <- 1:length(m_yelp@x) %in% flds[[i]]  
    
    #izvlacenje vektora stvarnih vrednosti testiranog folda
    test_edg <- m_yelp@x[ind_log]
    
    #pravljenje nove sparse matrice istih dimenzija bez vrednosti na mestima i-tog folda
    train_matrix <- sparseMatrix(i = summary(m_yelp)$i[!ind_log] ,j =summary(m_yelp)$j[!ind_log] , x=summary(m_yelp)$x[!ind_log], dims = m_yelp@Dim, dimnames = m_yelp@Dimnames)
    
    #racunanje aritmeticke sredine training m
    train_mean_m_edg <- mean(train_matrix@x)
    
    r1 <- new("realRatingMatrix", data = train_matrix)
    
    train_m_mean <- sparseMatrix(i = summary(m_yelp)$i ,j =summary(m_yelp)$j , x=train_mean_m_edg, dims = m_yelp@Dim, dimnames = m_yelp@Dimnames)
    #pravljenje sparse matrice istih dimenzija kao i m_yelp(inicijalna matrica) samo sto su svi elementi aritmeticka sredina inicijalne matrice(gde nisu 0)
    
    #izvlacenje srednje vrednosti kolona i redova train matrice i korigovanje NaN vrednosti
    train_colMeans_m_edg <- colMeans(r1)
    train_colMeans_m_edg[is.nan(train_colMeans_m_edg)==TRUE] <- train_mean_m_edg
    
    train_rowMeans_m_edg <- rowMeans(r1)
    train_rowMeans_m_edg[is.nan(train_rowMeans_m_edg)==TRUE] <- train_mean_m_edg
    
    #racunanje razlike u srednoj oceni za redove(usere) i kolone(restorane), dobija se vektor
    train_diff_rowMean <- train_rowMeans_m_edg - train_mean_m_edg
    train_diff_colMean <- train_colMeans_m_edg - train_mean_m_edg
    
    #pretvaranje gore stvorenog vektora u sparse matricu sa elementima na istim pozicijama kao i inicijalna matrica
    train_sparse_row_diff <- train_diff_rowMean * avail_m_yelp
    train_sparse_col_diff <- t(train_diff_colMean * t(avail_m_yelp))
    
    train_rij_sparse <- train_m_mean + train_sparse_row_diff + train_sparse_col_diff
    
    #korigovanje vrednosti vecih od 5 i manjih od 1
    ind_below_1 <- train_rij_sparse@x < 1
    train_rij_sparse@x[ind_below_1] <- 1
    ind_above_5 <- train_rij_sparse@x > 5
    train_rij_sparse@x[ind_above_5] <- 5
    
    #
    predicted_edg <- train_rij_sparse@x[ind_log]
    predicted_mean <- rep(train_mean_m_edg, length(flds[[i]])) 
    
    cv_df[i,1] <- RMSE(test_edg,predicted_edg)
    cv_df[i,2] <- RMSE(test_edg,predicted_mean)
    
  }
  
  cv_df
}

model_builder2(restoraunt_reviews)

ptm <- proc.time()
ev1 <- model_builder2(restoraunt_reviews)
proc.time() - ptm

ev11 <- data.frame("RMSE" =c(overall_rmse(ev1$RMSE_Mean_Plus), overall_rmse(ev1$RMSE_Mean)))
rownames(ev11) <- c("Mean Predicotor +","Mean Predictor")
ev11

