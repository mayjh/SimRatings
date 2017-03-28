## R analysis for MDS results

################ first read in the data ######
setwd("/Applications/MAMP/htdocs/MDS/data/v3")

## Data Analysis for MDS similarity ratings
## Jianhong Shen, Jun 2016
rm(list=ls())
graphics.off()
se <- function(x) {sd(x)/sqrt(length(x))}

# Load the data (Contains subjNums, dat)
# Specify the subfolder
pathname = "."    

# Specify the beginnings of files
filePattern = "*_MDS.csv"
imClass = read.csv('/Users/jianhongshen/Documents/Lab/Farrell Bird Images/cub_format_new/imClass.csv',
                   stringsAsFactors = F)

# List the data files
fileNameVec = dir( path=pathname, pattern=filePattern )

# Number of subjs
nSubj = length(fileNameVec) 
npairs = 210; nbird = 20
sim_index = c(7:216); id_index = c(218:237)
subj=1
#dat=NULL  # use this one if dat is a big data frame
dat=list() # use this one if dat is a list with each participant's data as an elment of the list
sim_vars = c('response','rt','stim1','stim2','bird1','bird2')
id_vars = c('response','rt','key','acc')
subnames = matrix(unlist(strsplit(fileNameVec,'_')),ncol=7,byrow = T)[,1]
sim = array(NA,dim=c(length(sim_vars),nSubj,npairs),dimnames = list(sim_vars,subnames))
id_post = array(NA,dim=c(length(id_vars),nSubj,nbird),dimnames = list(id_vars,subnames))
lengthn = rep_len(NA,nSubj)

for ( subj in 1:nSubj ) {
  
  dataFileName = paste(pathname, "/", fileNameVec[subj], sep="" )
  dat[[subj]] = read.csv(dataFileName,stringsAsFactors = F)
  sim['response',subj,] = as.numeric(dat[[subj]]$sim_score[sim_index])
  sim['rt',subj,] = dat[[subj]]$rt[sim_index]
  stim_temp = dat[[subj]]$stimulus[sim_index]
  sim['stim1',subj,] = matrix(unlist(strsplit(stim_temp,'"')),ncol=5,byrow=T)[,2]
  sim['stim2',subj,] = matrix(unlist(strsplit(stim_temp,'"')),ncol=5,byrow=T)[,4]
  sim['bird1',subj,] = matrix(unlist(strsplit(sim['stim1',subj,],'/')),ncol=3,byrow=T)[,2]
  sim['bird2',subj,] = matrix(unlist(strsplit(sim['stim2',subj,],'/')),ncol=3,byrow=T)[,2]
  
  id_temp = dat[[subj]]$responses[id_index]
  id_post['response',subj,] = matrix(unlist(strsplit(id_temp,'"')),nrow=20,byrow = T)[,4]
  lengthn[subj] = length(unlist(strsplit(id_temp, "\"")))
  id_post['rt',subj,] = dat[[subj]]$rt[id_index]
  id_post['key',subj,] = dat[[subj]]$answer[id_index]
  id_post['acc',subj,] = as.numeric(id_post['key',subj,]==id_post['response',subj,])
}
dimnames(id_post)[[3]] = id_post['key',subj,]

id_score = apply(id_post['acc',,],1,function(x) {sum(as.numeric(x))})
## calculate 
birds = unique(sim['bird1',1,]) 
birdsc = unique(imClass[imClass$class %in% as.numeric(birds),c('class','H1label')])
birdsc$classc = paste("0",birdsc$class,sep="")
rownames(birdsc) = birdsc$H1label
ind = dist_MDS = id_postMDS = array(0,dim=c(nSubj,nbird,nbird),dimnames = list(subnames,birds,birds))
#dist_MDS = list()
sim_same = array(NA,dim=c(nSubj,nbird),dimnames = list(subnames,birds))
for (subi in subnames) {
  for (i in birds){
    for (j in birds) {
      ind[subi,i,j] = which((sim['bird1',subi,]==i & sim['bird2',subi,]==j) |
                              (sim['bird2',subi,]==i & sim['bird1',subi,]==j))
      dist_MDS[subi,i,j] = dist_MDS[subi,j,i] = as.numeric(sim['response',subi,ind[subi,i,j]])
    }
    bird_i = as.character(birdsc[birdsc$classc==i,'H1label'])
    choice_i = id_post['response',subi,bird_i]
    if (!choice_i=="NA") id_postMDS[subi,i,birdsc[choice_i,'classc']] = 1
    sim_same[subi,i] = dist_MDS[subi,i,i]
  }
}

## get Ratings
Ratings = 8-dist_MDS
npair = nbird*(nbird-1)/2
Ratings_flat = array(NA,dim = c(nSubj,npair),dimnames = list(subnames))	
for (i in 2:nbird){
  for (j in 1:(i-1)){
    t <- j+(i-1)*(i-2)/2
    for (subi in subnames){
      Ratings_flat[subi,t] <- Ratings[subi,i,j]
    }
  }
}

## identify bad participants
sd_Ratings = apply(Ratings_flat,1,sd)

# the mean of people rating identical pairs, 7 for very dissimilar
mean_R_diag = apply(dist_MDS,1,function(x){mean(diag(x))})
sd_R_diag = apply(dist_MDS,1,function(x){sd(diag(x))})
hist(mean_R_diag)

# check the relationship between saying very dissimilar & identification accuracy
count_7 = apply(Ratings_flat,1,function(x){sum(x>5)})
cor.test(count_7,id_score)
# check the relationship between saying very similar & identification accuracy
count_1 = apply(Ratings_flat,1,function(x){sum(x<3)})
cor.test(count_1,id_score)

## get the number of minimumly used choice
min_count = apply(Ratings_flat,1,function(x){min(table(x))})
x=which(min_count<5)
for (i in x) {hist(Ratings_flat[i,],main=paste('Sub:',subnames[i],', Score:',id_score[i]))}

# get how many choices were used for each subject
col_count = apply(Ratings_flat,1,function(x){length(table(x))})
print(which(col_count<7))

# sanity check RT
rt = array(NA,dim=c(nSubj,npairs),dimnames=list(subnames)) 
for (subi in subnames) rt[subi,] = as.numeric(sim['rt',subi,])
rt_mean = apply(rt,1,mean)
rt_median = apply(rt,1,median)

# check the number of trials below 500ms
nLE500 = apply(rt,1,function(x){sum(x<500)})
nLE1000 = apply(rt,1,function(x){sum(x<1000)})

# check the number of trials below 500ms
nGE50000 = apply(rt,1,function(x){sum(x>50000)})
nGE10000 = apply(rt,1,function(x){sum(x>10000)})

rt1 = id_post['rt',,]; rt_y = apply(rt1,1,function(x) mean(as.numeric(x)))
use = which(sd_Ratings>1 & rt_Ratings<10000) # 

dist = lapply(seq(dim(dist_MDS)[1]), function(x) dist_MDS[x,,])
cor(id_score,apply(sim_same,1,sum))
save(dist_MDS,id_postMDS,birdsc,Ratings,Ratings_flat,id_post,sim,file='dat_MDS.Rdata')

##########MDS in R ###########
# library("smacof")
# data("perception")
# res1 = smacofIndDiff(dist, constraint = "indscal", ndim=3)   ## diagonally restricted weights
# summary(res1)
# 
# xy <- res1$gspace
# 
# require('jpeg')
# ims = dir(path="/Applications/MAMP/htdocs/MDS/img",pattern='0*.jpg',recursive = T)
# tmp = matrix(unlist(strsplit(ims,split='[/.]')),nrow=length(ims),byrow=T)
# ims_plot = list()
# 
# for (i in 1:length(birds)) {
#   birdi = birds[i]
#   imsi = ims[sample(which(tmp[,1]==birdi,arr.ind = T),1)]
#   ims_plot[[i]] = readJPEG(paste("/Applications/MAMP/htdocs/MDS/img/",imsi,sep=""))
# }
# 
# thumbnails <- function(x, y, images, width = 0.1*diff(range(x)), 
#                        height = 0.1*diff(range(y))){
#   
#   # images <- replicate(length(x), images, simplify=FALSE)
#   stopifnot(length(x) == length(y))
#   
#   for (ii in seq_along(x)){
#     rasterImage(images[[ii]], xleft=x[ii] - 0.5*width,
#                 ybottom= y[ii] - 0.5*height,
#                 xright=x[ii] + 0.5*width, 
#                 ytop= y[ii] + 0.5*height, interpolate=FALSE)
#   }
# }
# 
# pdf(file="../mds.pdf")
# plot(xy[,c(1,2)], t="n")
# thumbnails(xy[,1], xy[,2], ims_plot)
# plot(xy[,c(1,3)], t="n")
# thumbnails(xy[,1], xy[,3], ims_plot)
# plot(xy[,c(2,3)], t="n")
# thumbnails(xy[,2], xy[,3], ims_plot)
# dev.off()
# 
# ### do item analysis to see if there are weird items  ########
# 
# library(reshape2)
# data = melt(sim['response',,]); 
# colnames(data) = c("person","item","y")
# data$y = as.numeric(data$y)
# data$item = as.character(data$item)
# 
# data_sim = acast(data,person~item)
# data_sim = data_sim
# plot(id_score,apply(data_sim,1,sd))
