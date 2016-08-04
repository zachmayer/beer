
##########################################
# SETUP
##########################################

stop('Prevent accidently running the whole script')

#Setup
rm(list = ls(all = TRUE)) 
setwd('~/source/beer_rec')
gc(reset=TRUE)
set.seed(2)

#Load Libraries
require('data.table')
require('Matrix')
require('irlba')
library('rbm')
source('~/Dropbox/Projects/GC/GC Practice.R')
options(gc_path='/users/zach/source/graphchi')
options(gc_cache='/users/zach/gc_cache/')

##########################################
# Load data
##########################################

filename <- 'beer_reviews.RData'
if(! file.exists(filename)){
  dat <- read.csv('beer_reviews.csv')
  save(dat, file=filename)
} else{
  load(filename)
}

#Setup dataset
dat <- data.table(dat)
dat[,review_profilename := factor(review_profilename)]
dat[,beer_name := factor(beer_name)]
setkeyv(dat, c('review_profilename', 'beer_beerid', 'review_time'))

#Remove blank reviewers
dat <- dat[review_profilename != '',]

#Keep most recent review
dim(dat)
dat[,last_review := which.max(review_time) == 1:.N, by=c('review_profilename', 'beer_beerid')]
dat <- dat[last_review==TRUE,]
dat[, last_review:=NULL]

#Normalize ratings
dat[, review_overall := review_overall/5]

##########################################
# Make sparse matrix
##########################################
mat <- sparseMatrix(i=as.integer(dat$review_profilename), j=as.integer(dat$beer_beerid), x=dat$review_overall)
colnames(mat) <- as.character(1:ncol(mat))
row.names(mat) <- levels(dat$review_profilename)

mat <- mat[,colSums(mat)>1]

##########################################
# Rebuild matrix
##########################################

length(mat@x)/prod(dim(mat))

mod <- irlba(mat, nu=100, nv=100)
mod$u <- Matrix(mod$u, sparse=TRUE)
mod$v <- Matrix(mod$v, sparse=TRUE)
mod$u[abs(mod$u) < 1e-4] <- 0
mod$v[abs(mod$v) < 1e-4] <- 0
mod$u <- drop0(mod$u)
mod$v <- drop0(mod$v)

mat_approx <- mod $u %*% Diagonal(x= mod $d)[1:ncol(mod $u),1:ncol(mod $v)] %*% t(mod $v)
length(mat@x)/prod(dim(mat))

##########################################
# Shannon entropy
##########################################

#Cross product
a_and_b <- crossprod(sign(mat>.9))
length(a_and_b@x)
a_and_b <- drop0(a_and_b)
length(a_and_b@x)

#A not B
b_not_a <- crossprod(a_and_b, Diagonal(x=diag(a_and_b)))
a_not_b <- t(b_not_a)

a_not_b <- a_not_b - a_and_b
b_not_a <- b_not_a - a_and_b


a_and_b <- as('dgCMatrix', a_and_b, strict=FALSE)


recs <- a_and_b['90 Minute IPA',]
head(sort(recs[recs!=0], decreasing=TRUE), 10)

##########################################
# RBM test
##########################################

r <- stacked_rbm(
  mat, 
  layers = c(50, 500, 50), 
  use_gpu=FALSE,
  learning_rate=.5,
  verbose_stack=TRUE,
  verbose=TRUE, 
  use_mini_batches=TRUE, 
  batch_size=100, 
  max_epochs=1000)
plot(r)
min(r$error)

ratings <- sort(mat[29049,][mat[29049,]>0], decreasing=TRUE)
ratings

hidden_probs <- predict(r, mat[29049:29050,,drop=FALSE])
hidden_probs <- cBind(1, hidden_probs)
hidden_probs <- rBind(1, hidden_probs)
preds <- tcrossprod(tcrossprod(tcrossprod(hidden_probs, r$rbm_list[[3]]$rotation), r$rbm_list[[2]]$rotation), r$rbm_list[[1]]$rotation)
preds <- preds[,-1,drop=FALSE]
preds <- preds[-1,]
preds <- preds[1,]
visible_probs <- preds

head(sort(visible_probs[!names(visible_probs) %in% names(ratings)], decreasing=TRUE))

##########################################
# SVD
##########################################


model <- irlba(mat, nu = 100, nv = 100)

#Choose a user
user_id <- 29246
user_id <- 29049
ratings <- sort(mat[user_id,][mat[user_id,]>0], decreasing=TRUE)
ratings

#Make preductiins
preds <- (Matrix(model$u[user_id,,drop=FALSE]) %*% Matrix(diag(model$d))) %*% t(Matrix(model$v))
preds <- as.vector(as.matrix(preds))
names(preds) <- colnames(mat)
preds <- preds[preds>0]
preds <- preds + (mean(ratings)-mean(preds))
head(sort(preds[!names(preds) %in% names(ratings)], decreasing=TRUE))


##########################################
# GraphChi
##########################################

gc_dat <- copy(dat)
gc_dat[, review_profilename := as.numeric(factor(review_profilename))]
gc_dat[, beer_name := as.numeric(factor(beer_name))]
gc_dat <- as.data.frame(gc_dat)

namemap <- levels(factor(dat$beer_name))

train_rows <- gc_dat$review_profilename %in% sample(gc_dat$review_profilename, 2/3*length(unique(gc_dat$review_profilename)))
test_rows <- !train_rows

table(train_rows, test_rows)

xvars <- c('brewery_name', 'beer_style', 'beer_abv')

#Fit model
rm(model)
myControl <- gcControl(
  cold_start=3, 
  implicitratingtype=1,
  implicitratingpercentage=.50,
  implicitratingvalue=mean(dat$review_overall),
  gensgd_mult_dec=0.99, 
  gensgd_rate1=1e-1, 
  gensgd_rate2=1e-1,
  gensgd_rate3=1e-1, 
  minval=0,
  maxval=1,
  R_output_format=1,
  max_iter=500, 
  D=25,
  method='gensgd',
  quiet=1, 
  biassgd_lambda=1e-4)

#Fit model
model <- GC(
  gc_dat[,c('review_profilename', 'beer_name', 'review_overall', xvars)], 
  row_var='review_profilename', col_var='beer_name', value_var='review_overall', 
  features=xvars,
  control=myControl,
  train_rows=train_rows, test_rows=test_rows, valid_rows=test_rows)
model

test <- gc_dat[test_rows,]
test$pred <- model$pred 

mean(abs(test$pred*5-test$review_overall*5))

#Choose a user
user_id <- 29246
user_id <- 29049
ratings <- test[test$review_profilename==user_id,'review_overall']
names(ratings) <- namemap[test[test$review_profilename==user_id,'beer_name']]
sort(ratings, decreasing=TRUE)

#Make preductiins
preds <- test[test$review_profilename==user_id,'pred']
names(preds) <- namemap[test[test$review_profilename==user_id,'beer_name']]
sort(preds, decreasing=TRUE)

test$beer_name <- namemap[test$beer_name]
test[test$review_profilename==user_id,c('beer_name','review_overall', 'pred')]

##########################################
# SVD test/RBM test
##########################################
set.seed(42)
(m <- matrix(sample(0:1, 50, replace=TRUE), nrow=5))
colnames(m) <- paste0('V', 1:ncol(m))
s <- svd(m, nu=4, nv=4)
(p <- round(s$u %*% diag(s$d)[1:ncol(s$u),1:ncol(s$v)] %*% t(s$v), 2))

new_user <- matrix(sample(0:1, 10, replace=TRUE), nrow=1)
colnames(new_user) <- colnames(m)
diag(s$d)[1:ncol(s$u),1:ncol(s$v)] %*% t(new_user %*% s$v)
t(new_user %*% s$v)

r <- rbm(m, verbose=TRUE)
hidden_probs <- predict(r, new_user, omit_bias=FALSE)
hidden_probs[,1] <- 1
visible_probs <- r$activation_function(tcrossprod(hidden_probs, r$rotation)[,-1])

