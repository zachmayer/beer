
gc(reset=TRUE)
set.seed(6739)

#Load Libraries
require('data.table')
require('Matrix')
require('irlba')

#Custom functions
#see xkmeans::xdist
row_normalize <- function(m){
  row_norms <- sqrt(rowSums(m^2))
  row_norms <- t(crossprod(sign(m), Diagonal(x=row_norms)))
  row_norms@x <- 1/row_norms@x
  m_norm <- m * row_norms
  return(m_norm)
}
cosine_sim <- function(x, y=NULL){
  x <- drop0(x)
  x <- row_normalize(x)
  if(! is.null(y)){
    y <- row_normalize(y)
    return(tcrossprod(x, y))
  } else{
    return(tcrossprod(x))
  }
}
nnz <- function(x){
  length(x@i)
}
pnz <- function(x){
  length(x@i)/prod(dim(x))
}

##########################################
# Load data
##########################################

dat_orig <- fread('beer_reviews.csv')

#Setup dataset and remove nulls/blanks
dat <- data.table(dat_orig)
dat <- dat[!is.na(beer_name),]
dat <- dat[!is.na(review_profilename),]
dat <- dat[beer_name != '',]
dat <- dat[brewery_name != '',]
dat <- dat[review_overall != 0,]
dat <- dat[, beer := paste0(brewery_name, ' - ', beer_name)]
dat <- dat[, review_profilename := factor(review_profilename)]
dat <- dat[, beer := factor(beer)]
dat <- dat[, list(
  review_profilename,
  beer,
  brewery_name,
  beer_name,
  review_time,
  review_overall
  )]

#Beer list
beer_list_trunc <- dat[,list(N=.N), by=c('brewery_name', 'beer_name')]
beer_list_trunc <- beer_list_trunc[N>400,]
beer_vec_trunc <- beer_list_trunc[order(N, decreasing=TRUE), paste0(brewery_name, ' - ', beer_name)]
beer_list_trunc <- split(beer_list_trunc$beer_name, factor(beer_list_trunc$brewery_name))

#Keep most recent review
dat[,last_review := which.max(review_time) == 1:.N, by=c('review_profilename', 'beer')]
dat <- dat[last_review==TRUE,]
dat[, review_time := NULL]
dat[, last_review := NULL]

##########################################
# Make user similarity matrix
##########################################

#user - beer
user_beer <- sparseMatrix(
  i=dat[, as.integer(review_profilename)],
  j=dat[, as.integer(beer)],
  x=dat[, as.integer(review_overall-3)],
  dims = c(
    max(as.integer(dat$review_profilename)),
    max(as.integer(dat$beer))
  )
)
colnames(user_beer) <- as.character(levels(dat$beer))
row.names(user_beer) <- as.character(levels(dat$review_profilename))
user_beer <- drop0(user_beer)

#user - beer - 5 star
user_beer_5 <- sparseMatrix(
  i=dat[review_overall == 5, as.integer(review_profilename)],
  j=dat[review_overall == 5, as.integer(beer)],
  x=1,
  dims = c(
    max(as.integer(dat$review_profilename)),
    max(as.integer(dat$beer))
  )
)
colnames(user_beer_5) <- as.character(levels(dat$beer))
row.names(user_beer_5) <- as.character(levels(dat$review_profilename))
user_beer_5 <- drop0(user_beer_5)

#user - user, by 5 star beers
user_user_5 <- tcrossprod(drop0(user_beer_5))
diag(user_user_5) <- 0
user_user_5 <- sign(user_user_5)
user_user_5 <- drop0(user_user_5)

#user - user, by cosine sim
system.time(user_user_cos <- cosine_sim(user_beer))
diag(user_user_cos) <- 0
user_user_cos <- drop0(user_user_cos)
summary(user_user_cos@x)
user_user_cos <- drop0(user_user_cos)

#user - user, PCA sim
pca_model <- irlba(user_beer, nu=1000, nv=1000)
row.names(pca_model$v) <- colnames(user_beer)
row.names(pca_model$u) <- row.names(user_beer)

##########################################
# Prediction functions
##########################################

beer_vec <- colnames(user_beer)
beers_to_mat <- function(x, names){
  require('Matrix')
  j <- which(names %in% x)
  sparseMatrix(
    i = rep(1, length=length(j)),
    j = j,
    x = 1,
    dims = c(1, length(names))
  )
}
beers_to_recs <- function(
  x,
  names,
  method='cos',
  sim_cutoff=.20,
  N=25){

  require('data.table')
  stopifnot(method %in% c('sum', 'cos', 'pca'))
  user_selection_mat <- beers_to_mat(x, names)

  if(method=='pca'){
    user_loadings <- user_selection_mat %*% pca_model$v
    recs <- tcrossprod(user_loadings, pca_model$v)
    recs <- t(recs)
    sim_users <- 1
  } else{
    if(method=='sum'){
      sim_users <- tcrossprod(user_beer_5, user_selection_mat)
      sim_users <- sign(sim_users)
    } else if(method=='cos'){
      sim_users <- cosine_sim(user_beer, user_selection_mat)
    }
    diag(sim_users) <- 0
    sim_users@x[sim_users@x<sim_cutoff] <- 0
    sim_users <- drop0(sim_users)
    recs <- crossprod(sign(user_beer_5), sim_users)
  }

  recs_dt <- data.table(
    beer = row.names(recs),
    pct_match = recs[,1] / sum(sim_users),
    key='pct_match'
  )
  recs_dt <- recs_dt[pct_match>0,]
  recs_dt <- recs_dt[! beer %in% x,]
  recs_dt <- recs_dt[,pct_match := round(pct_match * 100, 1)]
  return(head(recs_dt[order(pct_match, decreasing=TRUE),], N))
}
beers_to_recs(
  x = c(
    '21st Amendment Brewery - 21st Amendment IPA',
    'Southern Tier Brewing Company - 2XIPA'
  ),
  names = beer_vec,
  method = 'cos',
  N = 5
)
beers_to_recs(
  x = c(
    '21st Amendment Brewery - 21st Amendment IPA',
    'Southern Tier Brewing Company - 2XIPA'
  ),
  names = beer_vec,
  method = 'sum',
  N = 5
)
beers_to_recs(
  x = c(
    '21st Amendment Brewery - 21st Amendment IPA',
    'Southern Tier Brewing Company - 2XIPA'
  ),
  names = beer_vec,
  method = 'pca',
  N = 5,
  sim_cutoff=0
)

#Save data
save(
  beer_list_trunc,
  beer_vec,
  beer_vec_trunc,
  user_beer_5,
  user_beer,
  pca_model,
  beers_to_mat,
  beers_to_recs,
  file='beer.RData'
)
