

# This script contains functions used in plots.R and stats.R scripts
# Please do not run this script

# Author: Mustafa S. Pir

fisher_pairwise_test<- function (df, significance = FALSE) {
  x<-combn(1:length(df[[1]]),2)
  y<-length(x)/2
  dfm<-as.matrix(df[,2:length(df[1,])])
  rownames(dfm)<-df[[1]]
  m<-matrix("-",nrow = length(df[[1]]), ncol = length(df[[1]]))
  ms<-matrix("-",nrow = length(df[[1]]), ncol = length(df[[1]]))
  for (i in 1:y){
    t<-fisher.test(dfm[c(x[1,i],x[2,i]),])
    m[x[1,i],x[2,i]]<-t$p.value
    if (t$p.value < 0.05 && t$p.value >= 0.01){
      ms[x[1,i],x[2,i]]<-"*"
    }
    else if (t$p.value < 0.01 && t$p.value >= 0.001){
      ms[x[1,i],x[2,i]]<-"**"
    }
    else if (t$p.value < 0.001 && t$p.value >= 0.0001){
      ms[x[1,i],x[2,i]]<-"***"
    }
    else if (t$p.value < 0.0001){
      ms[x[1,i],x[2,i]]<-"****"
    }
    else {ms[x[1,i],x[2,i]]<-"ns"}
  }
  rownames(m)<-df[[1]]
  colnames(m)<-df[[1]]
  rownames(ms)<-df[[1]]
  colnames(ms)<-df[[1]]
  m<-t(m)
  ms<-t(ms)
  m<-as_tibble(m, rownames = NA)
  ms<-as_tibble(ms, rownames = NA)
  ifelse(significance == TRUE, return(ms), return(m))
}



wilcox_pairwise_test<- function (df, genotype, length, significance = FALSE) {
  genelist<-unique(df[[genotype]])
  x<-combn(genelist,2)
  y<-length(x)/2
  x1<-parse_expr(genotype)
  x2<-parse_expr(length)
  m<-matrix("-",nrow = length(genelist), ncol = length(genelist))
  ms<-matrix("-",nrow = length(genelist), ncol = length(genelist))
  for (i in 1:y){
    #print(c(x[1,i],x[2,i]))
    #print(i)
    t<-wilcox.test(eval(x2) ~ eval(x1), data = df[df[[genotype]] %in% c(x[1,i],x[2,i]),], exact = FALSE)
    
    m[which(genelist == x[1,i]),which(genelist == x[2,i])]<-t$p.value
    if (t$p.value < 0.05 && t$p.value >= 0.01){
      ms[which(genelist == x[1,i]),which(genelist == x[2,i])]<-"*"
    }
    else if (t$p.value < 0.01 && t$p.value >= 0.001){
      ms[which(genelist == x[1,i]),which(genelist == x[2,i])]<-"**"
    }
    else if (t$p.value < 0.001 && t$p.value >= 0.0001){
      ms[which(genelist == x[1,i]),which(genelist == x[2,i])]<-"***"
    }
    else if (t$p.value < 0.0001){
      ms[which(genelist == x[1,i]),which(genelist == x[2,i])]<-"****"
    }
    else {ms[which(genelist == x[1,i]),which(genelist == x[2,i])]<-"ns"}
  }
  rownames(m)<-genelist
  colnames(m)<-genelist
  rownames(ms)<-genelist
  colnames(ms)<-genelist
  m<-t(m)
  ms<-t(ms)
  m<-as_tibble(m, rownames = NA)
  ms<-as_tibble(ms, rownames = NA)
  ifelse(significance == TRUE, return(ms), return(m))
}


