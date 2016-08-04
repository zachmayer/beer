
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

#Add profile id
dat[,profile_id := as.integer(factor(review_profilename))]

#Clean abv
dat[,na_abv := as.integer(is.na(beer_abv))]
dat[is.na(beer_abv), beer_abv := 0]

#Remove NAs
dat <- dat[!is.na(beer_beerid),]

##########################################
# Add my ratings
##########################################

beer_table <- dat[,list(n=.N),by=c('brewery_name', 'beer_name', 'beer_beerid', 'beer_abv', 'na_abv', 'beer_style', 'brewery_id')]
setkeyv(beer_table, 'beer_beerid')

#Lookup beers
unique(dat[brewery_id == 610,list(beer_name, beer_beerid)][order(beer_beerid),])
unique(dat[brewery_id == 735,list(beer_name, beer_beerid)][order(beer_beerid),])
unique(dat[brewery_id == 3818,list(beer_name, beer_beerid)][order(beer_beerid),])

#My ratings
my_ratings <- data.table(
  beer_beerid=c(2751, 20781, 57908, 16814, 28577, 2093, 29619, 30420, 9086, 34085, 63, 332, 99, 101, 355, 3968, 667, 4083, 1119),
  rating=c(5,5,5,5,5,5,5,5,5,5,3,0,3,1,4,4,3,4,4)
)
setkeyv(my_ratings, 'beer_beerid')
my_ratings <- beer_table[my_ratings,]
my_ratings[,list(beer_name, rating)]

#Trainset
extra_train <- my_ratings

#Pred set
pred_set <- beer_table[! beer_beerid %in% my_ratings$beer_beerid,]
pred_set[,profile_id := 0]
pred_set[,review_overall := 0]
pred_set <- pred_set[n>500,]

##########################################
# Write VW
##########################################

clean_text <- function(x){
  out <- gsub('[[:punct:]]|[[:space:]]+', ' ', tolower(x))
  out <- gsub(' +', ' ', out)
  out
}

write_vw <- function(dataset, path, ...){
  
  VW_dat <- dataset[,list(
    review_overall/5,
    " |u u", profile_id,
    " |b b", beer_beerid, ' abv:', log1p(beer_abv), ' na_abv:', na_abv,
    " |bs ", clean_text(beer_style),
    " |brew b", clean_text(brewery_id)
  )]
  
  write.table(
    VW_dat, 
    path,
    sep='', col.names=FALSE, row.names=FALSE, quote=FALSE, ...)
  system(paste('head', path))
  system(paste('tail', path))
}

write_vw(dat, 'vw_dat.txt')
write_vw(extra_train, 'vw_dat.txt', append=TRUE)

##########################################
# VW model
##########################################

thin_vw_wrapper <- function(...){
  
  #Add runtime args
  args <- list(...)
  
  #Make args a character vector
  args <- paste(names(args), unlist(args), sep='=')
  args <- paste0(' --', args)
  args <- gsub('=TRUE', '', args)
  
  #Run model
  call <- paste(c('vw ', args), collapse='')
  print(call)
  system(call, intern=FALSE)
}

thin_vw_wrapper(
  data=path.expand('vw_dat.txt'),
  final_regressor='vw_model.vw',
  kill_cache=TRUE,
  cache=TRUE,
  compressed=TRUE,
  passes=2,
  learning_rate=0.01,
  decay_learning_rate=.90,
  loss_function='quantile',
  quadratic='ub',
  rank=25,
  quantile_tau=.90,
  bit_precision=22,
  l1=1e-10,
  l2=1e-10
)

##########################################
# Make recs
##########################################

#Write tempfiles
pred_dat <- tempfile()
predictions <- tempfile()
write_vw(pred_set, pred_dat)

#Make predicitons
thin_vw_wrapper(
  initial_regressor='vw_model.vw',
  data=pred_dat,
  testonly=TRUE,
  predictions=predictions,
  min_prediction=-10,
  max_prediction=10
)
p <- fread(predictions)[,V1]
pred_set[,p_rate := p]
pred_set <- pred_set[order(p_rate, decreasing=TRUE),]
head(pred_set[,list(p_rate, id=beer_beerid, brewery_name, beer_name, beer_style, beer_abv)], 25)
tail(pred_set[,list(p_rate, id=beer_beerid, brewery_name, beer_name, beer_style, beer_abv)], 25)

#Cleanup
unlink(pred_dat)
unlink(predictions)
