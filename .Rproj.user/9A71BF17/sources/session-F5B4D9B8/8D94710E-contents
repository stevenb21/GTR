




cathelp <- function(df,lon){
  m <- length(lon)
  if (m==0) {return(c())}
  if (m==1){return(df[lon[1]])}
  else{
    splitdf <- df[lon[1]]
    for (i in 2:m){
      splitdf <- cbind(splitdf,df[lon[i]])
    }
  }
  return(data.frame(splitdf))
}






catsplit <- function(df,catvec){
  cat_idx <- which(catvec==1)
  return(cathelp(df,cat_idx))
}


contsplit <- function(df,catvec){
  cont_idx <- which(catvec==0)
  return(cathelp(df,cont_idx))
}





na_transform <- function(myval){
  na.strings <- c(""," ","NaN","Inf","NULL")
  if (myval %in% na.strings){
    return(NA)}
  else{
    return(myval)}
}



mysummaryfeat_cont <- function(feat){
  feat <- sapply(feat,function(x) na_transform(x),USE.NAMES=FALSE)
  lout <- append(typeof(feat),summary(feat,digits=4))
  if (!any(is.na(feat))){
    lout <- append(lout,0)
  }
  lout <- append(lout,'')
  lout <- append(lout,round(sd(feat,na.rm=TRUE),2))
  lout <- append(lout,round(skewness(feat,na.rm=TRUE),2))
  lout <- append(lout,round(kurtosis(feat,na.rm=TRUE),2))
  lout <- append(lout,object.size(feat))
  names(lout) <- ''
  return(lout)
}





mysummaryfeat_cat <- function(feat){
  feat <- sapply(feat,function(x) na_transform(x),USE.NAMES=FALSE)
  lout <- append(typeof(feat),summary(feat,digits=4))
  if (!any(is.na(feat))){
    lout <- append(lout,0)
  }
  lout <- append(lout,length(unique(feat))) # replace sd() w/ nunique function
  lout <- append(lout,round(sd(feat,na.rm=TRUE),2))
  lout <- append(lout,round(skewness(feat,na.rm=TRUE),2))
  lout <- append(lout,round(kurtosis(feat,na.rm=TRUE),2))
  lout <- append(lout,object.size(feat))
  names(lout) <- ''
  return(lout)
}



mysummaryfeat <- function(feat,cat=FALSE){
  if (!cat){return(mysummaryfeat_cont(feat))}else{return(mysummaryfeat_cat(feat))}}







mysummaryfactor <- function(fact){
  fact <- sapply(fact,function(x) na_transform(x),USE.NAMES=FALSE)
  na_count <- sum(is.na(fact))
  n_unique <- length(unique(fact))
  oz <- object.size(fact)
  lout <- c("factor",NA,NA,NA,NA,NA,NA,na_count,n_unique,NA,NA,NA,oz)
  return(lout)
}





mysummarydf <- function(df,catvec){
  mysmry <- c()
  for (i in 1:ncol(df)){
    if (is.factor(df[,i]))
    {mysmry <- cbind(mysmry,mysummaryfactor(df[i][,1]))}
    else
    {mysmry <- cbind(mysmry,mysummaryfeat(df[i][,1],cat=catvec[i]))}
  }
  colnames(mysmry) <- names(df)
  Features <- c("type","Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","NA.s","n_unique","sd","skewness","kurtosis","size(bytes)")
  mysmry <- cbind(Features,mysmry)
  rownames(mysmry) <- NULL
  return(data.frame(mysmry))
}













gghist <- function(df, feat_name){
  feat <- ensym(feat_name)
  binsize <- as.numeric(summarise(df,binsize=(sd(!! feat,na.rm=TRUE))*(nrow(df)^(-1/3))))
  ggp <- ggplot(df %>% drop_na, aes(x =!! feat,y=..density..)) +
    geom_histogram(binwidth=binsize,fill="white",colour="black") +
    geom_density()
  return(ggp)
}




contplot <- function(df){
  m <- ncol(df)
  p <- gghist(df,!! sym(names(df)[1]))
  if (m==1){return(p)}
  else{
    for(i in 2:m){
      p <- p+ gghist(df,!! sym(names(df)[i]))
    }
    return(p)
  }
}







ggbar <- function(df, feat_name){
  feat <- enquo(feat_name)
  ggp <- ggplot(df %>% drop_na, aes(x =!! feat)) +
    geom_bar(fill="lightgreen",colour="black") +
    geom_text(aes(label = ..count..),stat="count", vjust = 1.5, colour = "black")
  return(ggp)
}







catplot <- function(df){
  m <- ncol(df)
  p <- ggbar(df,!! sym(names(df)[1]))
  if (m==1)
  {return(p)}
  else{
    for(i in 2:m){
      p <- p+ggbar(df,!! sym(names(df)[i]))
    }
    return(p)
  }
}







