
##########################################
# SETUP
##########################################

stop('Prevent accidently running the whole script')

#Setup
rm(list = ls(all = TRUE))
gc(reset=TRUE)
set.seed(6739)

#Load Libraries
require('data.table')
require('Matrix')
require('irlba')

#Custom functions
cosine_sim <- function(m){
  m <- drop0(m)
  row_norms <- sqrt(rowSums(m^2))
  row_norms <- t(crossprod(sign(m), Diagonal(x=row_norms)))
  row_norms@x <- 1/row_norms@x
  m_norm <- m * row_norms
  return(tcrossprod(m_norm))
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

filename <- 'beer_reviews.RData'
if(! file.exists(filename)){
  dat <- read.csv('beer_reviews.csv')
  save(dat, file=filename)
} else{
  load(filename)
}

#Setup dataset and remove nulls/blanks
dat <- data.table(dat)
dat <- dat[!is.na(beer_name),]
dat <- dat[!is.na(review_profilename),]
dat <- dat[beer_name != '',]
dat <- dat[review_profilename != '',]

#Make factors
dat[, review_profilename := review_profilename]
dat[,review_profilename := factor(review_profilename)]
dat[,beer_name := factor(beer_name)]
setkeyv(dat, c('review_profilename', 'beer_beerid', 'review_time'))

#Keep most recent review
dat[,last_review := which.max(review_time) == 1:.N, by=c('review_profilename', 'beer_beerid')]
dat <- dat[last_review==TRUE,]
dat[, last_review:=NULL]

#Clean abv
dat[,na_abv := as.integer(is.na(beer_abv))]
dat[is.na(beer_abv), beer_abv := 0]

#Smaller dataset
#dat <- dat[sample(1:.N, 5000),]

#Split train/test
all_users <- dat[,sort(unique(review_profilename))]
test_users <- sample(all_users, floor(.25 * length(all_users)))
length(all_users) - length(test_users)
length(test_users)
dat[, set := 'train']
dat[review_profilename %in% test_users, set := 'test']

##########################################
# Add my ratings
##########################################

#Lookup beers
unique(dat[brewery_id == 610,list(beer_name, beer_beerid)][order(beer_beerid),])
unique(dat[brewery_id == 735,list(beer_name, beer_beerid)][order(beer_beerid),])
unique(dat[brewery_id == 3818,list(beer_name, beer_beerid)][order(beer_beerid),])

#My ratings
my_ratings <- data.table(
  review_profilename='zachmayer',
  beer_beerid=c(2751, 20781, 57908, 16814, 28577, 2093, 29619, 30420, 9086, 34085, 63, 332, 99, 101, 355, 3968, 667, 4083, 1119, 1093, 88, 1904,11757,276),
  review_overall=c(5,5,5,5,5,5,5,5,5,5,3,0,3,1,4,4,3,4,4,5,5,5,4,3),
  set='test'
)

#Add my ratings to main dataset
ratings <- rbind(
  dat[,list(review_profilename, beer_beerid, review_overall, set)],
  my_ratings,
  use.names=TRUE
  )
setkeyv(ratings, c('beer_beerid', 'review_profilename'))

#Scale ratings
ratings[,review_scaled := scale(review_overall)]

##########################################
# Make a beer map
##########################################

beer_n_ratings <- dat[,list(n=.N),keyby='beer_beerid']

beer_map <- unique(dat[,list(
  beer_beerid,
  beer_newid=as.integer(factor(beer_beerid)),
  brewery_name,
  beer_name,
  clean_name=make.names(beer_name)
)])
setkeyv(beer_map, 'beer_beerid')

beer_map <- merge(beer_map, beer_n_ratings, by='beer_beerid', all=TRUE)

ratings <- merge(ratings, beer_map, by='beer_beerid', all=TRUE)
ratings[,brewery_name := NULL]
ratings[,beer_name := NULL]

setkeyv(beer_map, 'beer_newid')

##########################################
# Make sparse matricies
##########################################

#User - beer matrix
user_all_beers <- sparseMatrix(
  i=ratings[, as.integer(review_profilename)],
  j=ratings[, as.integer(beer_newid)],
  x=ratings[, review_scaled],
  dims = c(
    max(as.integer(ratings$review_profilename)),
    max(beer_map$beer_newid)
  )
)
colnames(user_all_beers) <- as.character(beer_map$beer_newid)
row.names(user_all_beers) <- levels(ratings$review_profilename)
user_all_beers <- drop0(user_all_beers)

#User - 5 star beer matrix
star5_ratings <- sparseMatrix(
  i=ratings[review_overall == 5, as.integer(review_profilename)],
  j=ratings[review_overall == 5, as.integer(beer_newid)],
  x=1L,
  dims = c(
    max(as.integer(ratings$review_profilename)),
    max(beer_map$beer_newid)
    )
  )
colnames(star5_ratings) <- as.character(beer_map$beer_newid)
row.names(star5_ratings) <- levels(ratings$review_profilename)

#Check
stopifnot(all.equal(dim(user_all_beers), dim(star5_ratings)))

##########################################
# Various graphs / calculations
##########################################

#user - user graph
user_user <- tcrossprod(drop0(star5_ratings))
diag(user_user) <- 0
user_user <- sign(user_user)
user_user <- drop0(user_user)
pnz(user_user)

#Cosine similarity
system.time(user_user_cos_sim <- cosine_sim(user_all_beers))
diag(user_user_cos_sim) <- 0
user_user_cos_sim <- drop0(user_user_cos_sim)
user_user_cos_sim@x[abs(user_user_cos_sim@x)<.05] <- 0
user_user_cos_sim <- drop0(user_user_cos_sim)
pnz(user_user_cos_sim)
nnz(user_user_cos_sim) / nnz(user_user)

#"PCA" based user-user similarity
#http://stats.stackexchange.com/a/81877/2817
users_pca <- irlba(user_all_beers, nu=10, nv=0)
m <- users_pca$u
m[abs(m)<0.001] <- 0
system.time(user_sim <- cosine_sim(m))
user_sim@x[abs(user_sim@x)<.05] <- 0
user_sim <- drop0(user_sim)
pnz(user_sim)
nnz(user_sim) / nnz(user_user)

dimnames(user_sim) <- dimnames(user_user)

##########################################
# Rec engines
##########################################

#Use user-user to reccomend beers
system.time(user_beer <- user_user %*% star5_ratings)

#Sim-weighted graph
system.time(weighted_user_beer <- user_user_cos_sim %*% star5_ratings)

#Graph metrics
max(star5_ratings)
max(user_user)
max(user_beer)
max(weighted_user_beer)

##########################################
# Make recs
##########################################

recs_mat <- weighted_user_beer
user_sim_mat <- user_user_cos_sim

#Remove already drank beers
user_beer_recs <- recs_mat - (star5_ratings * (max(user_beer) + 1))
length(user_beer_recs@x)
user_beer_recs@x[user_beer@x<0] <- 0
user_beer_recs <- drop0(user_beer_recs)
length(user_beer_recs@x)

#Lookit my beers
recs <- data.table(
  beer_newid = as.integer(names(user_beer_recs['zachmayer',])),
  links = user_beer_recs['zachmayer',]
  )
recs <- recs[links>0,]
recs <- merge(recs, beer_map, by='beer_newid')
recs[, strength := links / sum(user_sim_mat['zachmayer',])]
recs[, novelty := strength / n]

#Recs by abs strength
recs <- recs[order(strength, decreasing=TRUE),]
recs[1:25,list(brewery_name, beer_name, beer_beerid, str=round(strength, 3) * 100)]

#Recs by abs strength, novelty weighted
recs <- recs[order(novelty, strength, decreasing=TRUE),]
recs[1:25,list(brewery_name, beer_name, round(novelty, 4) * 100)]
