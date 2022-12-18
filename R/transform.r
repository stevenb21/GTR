

ggqq <- function(df, feat_name){
  feat <- ensym(feat_name)
  ggp <- ggplot(df %>% drop_na,aes(sample=!! feat)) +
    stat_qq()
  return(ggp)
}



ggloghist <- function(df, feat_name){
  feat <- ensym(feat_name)
  binsize <- as.numeric(summarise(df,binsize=(sd(log10(!! feat),na.rm=TRUE))*(nrow(df)^(-1/3))))
  ggp <- ggplot(df %>% drop_na, aes(x =log10(!! feat),y=..density..)) +
    geom_histogram(binwidth=binsize,fill="white",colour="black") +
    geom_density()
  return(ggp)
}




ggsqrthist <- function(df, feat_name){
  feat <- ensym(feat_name)
  binsize <- as.numeric(summarise(df,binsize=(sd(sqrt(!! feat),na.rm=TRUE))*(nrow(df)^(-1/3))))
  ggp <- ggplot(df %>% drop_na, aes(x =sqrt(!! feat),y=..density..)) +
    geom_histogram(binwidth=binsize,fill="white",colour="black") +
    geom_density()
  return(ggp)
}



gg01 <- function(df, feat_name){
  feat <- ensym(feat_name)
  mu <- as.numeric(summarise(df %>% drop_na,fmean=mean(!! feat)))
  sigma <- as.numeric(summarise(df %>% drop_na,fs=sd(!! feat)))
  binsize <- as.numeric(summarise(df %>% drop_na,binsize=nrow(df)^(-1/3)))
  ggp <- ggplot(df %>% drop_na, aes(x =((!! feat - mu)/sigma),y=..density..)) +
    geom_histogram(binwidth=binsize,fill="white",colour="black") +
    geom_density()
  return(ggp)
}




ggimputehist <- function(df, feat_name){
  feat <- ensym(feat_name)
  df <- df %>% mutate(!! feat := ifelse(is.na(!! feat), mean(!! feat,na.rm=TRUE), !! feat))
  binsize <- as.numeric(summarise(df,binsize=(sd(!! feat))*(nrow(df)^(-1/3))))
  ggp <- ggplot(df, aes(x =!! feat,y=..density..)) +
    geom_histogram(binwidth=binsize,fill="white",colour="black") +
    geom_density() +
    xlab(paste0("impute(",feat,")"))
  return(ggp)
}







pca_plot <- function(df) {
  df <- df %>% drop_na %>% select(!where(is.factor))
  pca <- prcomp(df, scale = TRUE)
  var_explained = pca$sdev ^ 2 / sum(pca$sdev ^ 2)
  return(qplot(c(1:ncol(df)), var_explained) +
           geom_line() +
           xlab("Principal Component") +
           ylab("Variance Explained") +
           ggtitle("Scree Plot") +
           ylim(0, 1))
}




rfplot <- function(df,dv){
  dv_idx <- which(colnames(df) == dv)
  depvar <- ensym(dv)
  RandomForest <- randomForest(
    formula = df[,dv_idx] ~ .,
    data = df %>% drop_na)
  imp <- as.numeric(RandomForest$importance)
  rf <- sort(imp,decreasing=TRUE)
  idf <- data.frame(id=names(df),importance=rf[1:min(30,ncol(df))])
  ggp <- ggplot(idf,aes(x=id,y=importance))+
    geom_col() +
    theme(axis.text.x = element_text(angle = 90))
  return(ggp)
}





content <- function(contfeat){
  return(entropy(discretize(contfeat , numBins=as.integer((1+3.322*log(length(contfeat)))))))
}


entropyplot <- function(df, catvec){
  df <- df %>% drop_na
  entropylist <- c()
  for (i in 1:length(catvec)){
    if((is.factor(df[,i]))){
      entropylist <- append(entropylist,0)
      next
    }
    if(catvec[i]){
      entropylist <- append(entropylist,entropy(df[,i]))
    }else{
      entropylist <- append(entropylist,content(df[,i]))
    }
  }
  feature_entropy <- data.frame(id=names(df),entropy=sort(entropylist,decreasing=TRUE))
  ggp <- entplot <- ggplot(feature_entropy,aes(x=id,y=entropy))+
    geom_col() +
    theme(axis.text.x = element_text(angle = 90))
  return(ggp)
}








