#Function to get "scores" for unlisted plant genera (i.e. how much would bee species counts/network properties be increased by adding new species?)
# mat = regional plant genus - bee species interaction matrix (i.e. potential interactions)
#   - rows = plants, cols = bees
# plGen = vector of current plant genus names from each location
# exclude = character vector of plant genera that should not be considered (e.g. noxious weed)

getPlGenScores <- function(mat,plGen,exclude=NULL){
  missPlGen <- !plGen %in% rownames(mat) #Plants in plGen that aren't found in the regional network
  if(!any(!missPlGen)) stop('No listed plant genera found in network')
  if(any(missPlGen)){
    warning(paste0('Some plant genera not found in network (no observed visitors):\n',
                   paste0(plGen[missPlGen],collapse='\n'),'\n'))
    plGen <- plGen[!missPlGen]
  }
  
  #Known and potential networks
  matKnw <- mat[rownames(mat) %in% plGen,,drop=FALSE] #From plant list
  matPot <- mat[!rownames(mat) %in% plGen,,drop=FALSE] #Anything not in plant list
  
  if(!is.null(exclude) & any(rownames(matPot) %in% exclude)){
    warning(paste0('Some excluded genera were not considered:\n',
                   paste0(rownames(matPot)[rownames(matPot) %in% exclude],collapse='\n'),'\n'))
    matPot <- matPot[!rownames(matPot) %in% exclude,]
  }
  
  ret <- data.frame('PlantGenus'=rownames(matPot))
  
  #Which potential plants from network host new pollinators?
  ret$N <- apply(matPot[,colnames(matKnw)[apply(matKnw,2,function(x) !any(x>0))] ]>0,1,sum) 
  #Chao1 (bee) richness estimates
  richEst <- t(estimateR(outer(rep(1,nrow(matPot)),colSums(matKnw))+matPot))
  dChao1 <- (richEst[,2]-estimateR(colSums(matKnw))[2]) #Difference between known and known+potential_i matrix
  dChao1SE <- (richEst[,3]+estimateR(colSums(matKnw))[3]) #Summed Chao1 SE
  Chao1Z <- dChao1/dChao1SE #Z-score of difference in Chao1 scores
  
  ret$Chao1 <- dChao1
  ret$Chao1Z <- Chao1Z
  
  #Proportion increased Shannon-Weiner diversity
  ret$PropShannon <- (diversity(outer(rep(1,nrow(matPot)),colSums(matKnw))+matPot)-
                        diversity(colSums(matKnw)))/diversity(colSums(matKnw))
  
  # #Network connectance (proportion of possible links) - not sure this means much
  # ret$Connectance <- apply(matPot,1,function(x) networklevel(web = rbind(matKnw,x),index = 'connectance'))-networklevel(web = matKnw,index = 'connectance')
  
  #Proportion new network links
  ret$PropNewLinks <- apply(matPot,1,function(x) sum(x>0))/sum(matKnw>0)
  
  #Proportion increase in network complexity (linkage density/weighted # of links per spp)
  getLDens <- function(x) networklevel(web = rbind(matKnw,x),index = 'linkage density')
  
  ret$PropComplexity <- (apply(matPot,1,getLDens)-networklevel(web = matKnw,index = 'linkage density'))/
    networklevel(web = matKnw,index = 'linkage density')
  
  #Get names of potential new pollinators
  getNames <- function(x){
    paste0(names(x)[x],collapse=', ')
  } 
  ret$newSpp <- apply(matPot[,colnames(matKnw)[apply(matKnw,2,function(x) !any(x>0))] ]>0,1,getNames) 
  
  ret <- ret[ret$N>0,] #Remove zeros
  ret <- ret[with(ret,order(N,decreasing=TRUE)),] #Sort
  rownames(ret) <- 1:nrow(ret)
  
  return(ret)
}

# getPlGenScores(sppMat,unique(vinePlDat$plGenus)) %>% head
# getPlGenScores(sppMat,unique(vinePlDat$plGenus),exclude='Lotus') %>% head
# debugonce(getPlGenScores)