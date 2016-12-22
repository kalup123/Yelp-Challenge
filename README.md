# Yelp-Challenge
My solution for Yelp Challenge, explorative analysis and building Recommender System


Datasets can be find on https://www.yelp.com/dataset_challenge

First, I did exploartive analysis on all 5 databases in link above, in following to that I build 
recommender system:

1. using package `recommenderlab` from CRAN and implementing :

  - User Based Collaborative Filtering
  - Item Based Collaborative Filtering
  - Popular Method
  
2. using baseline and Mean Predictor+

  - Baseline predictor: Mean Replacement
  - Mean Predictor Plus  
  
    
  
  
,of all models above best scoring (RMSE) achived Mean Predictor Plus (1.29).  

In future, I plan to implement some latent-based collaborative filtering algorithyms, such as:

  - *SVD*(Singular Value Decomposition)
  - *NMF*(NonNegative Matrix Factorialization) and others...
  
Which all be input for some kind of *Hybrid* recommender system in oreder to minimize RMSE.
