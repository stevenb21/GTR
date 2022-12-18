


chisqfeat <- function(feat,dv){
  xsq <- chisq.test(table(feat,dv))
  if(xsq$p.value <= .0001){xsq$p.value <- -1}
  return(c(round(xsq$p.value,4),xsq$method))
}


chisqhelper <- function(df, dv) {
  dv_idx <- which(colnames(df) == dv)
  chilist <- c()
  for (i in 1:ncol(df)) {
    if (i == dv_idx) {
      next
    }
    chifeat <- chisqfeat(df[,i],df[,dv_idx])
    chilist <- rbind(chilist,append(names(df)[i],chifeat))
  }
  return(chilist)
}




anovafeat <- function(feat,dv){
  p.value <- summary(aov(dv ~ feat))[[1]][["Pr(>F)"]][1]
  if(p.value <= .0001){p.value <- -1}
  return(c(round(p.value,4),"ANOVA"))
}




anovahelper <- function(df, dv) {
  dv_idx <- which(colnames(df) == dv)
  anovalist <- c()
  for (i in 1:ncol(df)) {
    if (i == dv_idx) {
      next
    }
    myfeat <- anovafeat(df[,i],df[,dv_idx])
    anovalist <- rbind(anovalist,append(names(df)[i],myfeat))
  }
  return(anovalist)
}




linregfeat <- function(feat,dv){
  lm.fit <- lm(dv ~ feat)
  p.value <- summary(lm.fit)$coefficients[,"Pr(>|t|)"][2]
  names(p.value) <- NULL
  if(p.value <= .0001){p.value <- -1}
  return(c(round(p.value,4),"Linear Regression"))
}




linreghelper <- function(df, dv) {
  dv_idx <- which(colnames(df) == dv)
  linreglist <- c()
  for (i in 1:ncol(df)) {
    if (i == dv_idx) {
      next
    }
    myfeat <- linregfeat(df[,i],df[,dv_idx])
    linreglist <- rbind(linreglist,append(names(df)[i],myfeat))
  }
  return(linreglist)
}







logregfeat <- function(feat,dv){
  glm.fit <- glm(dv ~feat)
  lm.fit <- lm(dv ~ feat)
  p.value <- summary(glm.fit)$coefficients[,"Pr(>|t|)"][2]
  names(p.value) <- NULL
  if(p.value <= .0001){p.value <- -1}
  return(c(round(p.value,4),"Logistic Regression"))
}

logreghelper <- function(df, dv) {
  dv_idx <- which(colnames(df) == dv)
  logreglist <- c()
  for (i in 1:ncol(df)) {
    if (i == dv_idx) {
      next
    }
    myfeat <- logregfeat(df[,i],df[,dv_idx])
    logreglist <- rbind(logreglist,append(names(df)[i],myfeat))
  }
  return(logreglist)
}




multifeat <- function(feat,dv){
  mlg.fit <- multinom(dv ~ feat,trace=FALSE)
  z <- summary(mlg.fit)$coefficients/summary(mlg.fit)$standard.errors
  p <- (1 - pnorm(abs(z[,2]), 0, 1)) * 2
  for(i in 1:length(p)){
    if(p[i] <= .0001){p[1] <- -1}
    p[i] <- round(p[i],4)
  }
  #p <- cbind(names(p),p)
  #rownames(p) <- NULL
  out <- cbind(p,rep("MultinomReg 2-tail Z-test",length(p)))
  return(out)
}





multiloghelper <- function(df, dv) {
  dv_idx <- which(colnames(df) == dv)
  multireglist <- c()
  for (i in 1:ncol(df)) {
    if (i == dv_idx) {
      next
    }
    myfeat <- multifeat(df[,i],df[,dv_idx])
    featname <- paste0(names(df)[dv_idx],
                       ":",
                       rownames(myfeat),
                       "|",
                       names(df)[i])
    rownames(myfeat) <- NULL
    myfeat <- cbind(featname,myfeat)
    multireglist <- rbind(multireglist,myfeat)
  }
  return(multireglist)
}






flowchart <- function(df,dv,is_df_cat,dv_is_cat,dv_is_bin){
  if(is_df_cat){
    if(dv_is_cat){
      return(chisqhelper(df,dv))
    }
    else{
      return(anovahelper(df,dv))
    }
  }
  else{ #df is continuous
    if(!dv_is_cat){
      return(linreghelper(df,dv))
    }
    else{
      if(dv_is_bin){return(logreghelper(df,dv))}
      else{return(multiloghelper(df,dv))}
    }
  }
}












contcontp <- function(df,f1,f2){
  feat1 <- ensym(f1)
  feat2 <- ensym(f2)
  ggp <- ggplot(df %>% drop_na,aes(x =!! feat1, y =!! feat2)) +
    geom_point()
  return(ggp)
}




catcontp <- function(df,catf,contf){
  feat1 <- ensym(catf)
  feat2 <- ensym(contf)
  ggp <- ggplot(df %>% drop_na,aes(x=factor(!! feat1),y=!! feat2))+
    geom_boxplot()
  return(ggp)
}




catcatp <- function(df,f1,f2){
  feat1 <- ensym(f1)
  feat2 <- ensym(f2)
  ggp <- ggplot(df %>% drop_na,aes(x=factor(!! feat1),fill=factor(!! feat2)))+
    geom_bar(position="dodge",colour="black")
  return(ggp)
}



process_cormat <- function(contdf){
  cormat <- round(cor(contdf),2)
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
  cormat[lower.tri(cormat)]<- NA
  cormat <- melt(cormat,na.rm=TRUE)
  return(cormat)
}

ggheat <- function(cormat){
  ggheatmap <- ggplot(cormat, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "blue",
      high = "red",
      mid = "white",
      midpoint = 0,
      limit = c(-1, 1),
      space = "Lab",
      name = "Pearson\nCorrelation"
    ) +
    theme_minimal() + # minimal theme
    theme(axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      size = 12,
      hjust = 1
    )) +
    coord_fixed() +
    geom_text(aes(Var2, Var1, label = value),
              color = "black",
              size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal"
    ) +
    guides(fill = guide_colorbar(
      barwidth = 7,
      barheight = 1,
      title.position = "top",
      title.hjust = 0.5
    ))
  return(ggheatmap)
}































