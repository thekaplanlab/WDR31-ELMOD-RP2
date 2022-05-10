
source("data_wrangling.R")

ifts<-list()

for (i in 1:4){
  
  iftx<-suppressMessages(read_xlsx("IFT_Velocity_IFT_frequency_edited.xlsx", sheet = i))
  
  iftx<-iftx[-1,]
  ift_long<-pivot_longer(iftx, everything(), names_to = "Genotype", values_to = "Speed1", values_drop_na = TRUE)
  
  ifty<-suppressMessages(read_xlsx("IFT_Velocity_IFT_frequency_edited.xlsx", sheet = i, skip = 1))
  ift_long_2<-pivot_longer(ifty, everything(), names_to = "Part", values_to = "Speed", values_drop_na = TRUE)
  
  ift_all<-cbind(ift_long, ift_long_2)
  ift_all<-ift_all %>% select(-Speed1)
  ift_all[,1:2]<-as.data.frame(lapply(ift_all[,1:2], function(x) gsub("\\.\\.\\.[0-9]+", "", x)))
  
  ifts[[i]]<-ift_all
}


ift2<-ifts[[1]]
ift2$Marker<-"IFT-74::GFP"

for(i in 2:4){
  ift22<-ifts[[i]]
  genelist<-c("IFT-74::GFP","OSM-6::GFP","OSM-3::GFP","CHE-11::GFP")
  ift22$Marker<-genelist[i]
  ift2<-rbind(ift2, ift22)
}
colnames(ift2)[2:3]<-c("IFT","Number.of.Particles")

# IFT

df<-ift2
df$IFT<-as.character(df$IFT)
df$Genotype<-as.character(df$Genotype)
df$Marker<-as.character(df$Marker)
Length<-"Number.of.Particles"
genotype<-"Genotype"  

markers<-unique(df$Marker)
markers<-gsub(":", "", markers)

# Velocity
types<-unique(df$IFT)
for (k in 1:length(types)){
  for (j in 1:length(markers)){
    dfx<-df %>%
      filter(Marker == unique(df$Marker)[j] & IFT == types[k]) %>%
      dplyr::select(Number.of.Particles, Genotype)
    
    genelist<-unique(dfx[[genotype]])
    x<-combn(genelist,2)
    y<-length(x)/2
    x1<-parse_expr(genotype)
    x2<-parse_expr(Length)
    m<-matrix("-",nrow = length(genelist), ncol = length(genelist))
    ms<-matrix("-",nrow = length(genelist), ncol = length(genelist))
    
    m<-matrix("-",nrow = length(genelist), ncol = length(genelist))
    ms<-matrix("-",nrow = length(genelist), ncol = length(genelist))
    for (i in 1:y){
      #print(c(x[1,i],x[2,i]))
      #print(i)
      s<-shapiro.test(dfx$Number.of.Particles[dfx[[genotype]] %in% c(x[1,i],x[2,i])])
      
      if (s$p.value < 0.05){
        t<-wilcox.test(eval(x2) ~ eval(x1), data = dfx[dfx[[genotype]] %in% c(x[1,i],x[2,i]),], exact = FALSE)
      }
      else {
        t<-t.test(eval(x2) ~ eval(x1), data = dfx[dfx[[genotype]] %in% c(x[1,i],x[2,i]),], alternative = "two.sided", var.equal = FALSE)
      }
      
      m[which(genelist == x[1,i]),which(genelist == x[2,i])]<-as.numeric(t$p.value)
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
    write.xlsx(m, paste0(types[k], "_velocity.xlsx"), sheetName = paste(markers[j]), append = TRUE)
    write.xlsx(ms, paste0(types[k], "_velocity_sig.xlsx"), sheetName = paste(markers[j]), append = TRUE)
  }
  
}

df2<-iftz_all
colnames(df2)<-c("Marker","Genotype","IFT","Number.of.Particles")

df2$IFT<-as.character(df2$IFT)
df2$Genotype<-as.character(df2$Genotype)
df2$Marker<-as.character(df2$Marker)
Length<-"Number.of.Particles"
genotype<-"Genotype"  

markers<-unique(df2$Marker)
markers<-gsub(":", "", markers)

# Frequency
types<-unique(df2$IFT)
for (k in 1:length(types)){
  for (j in 1:length(markers)){
    dfx<-df2 %>%
      filter(Marker == unique(df2$Marker)[j] & IFT == types[k]) %>%
      dplyr::select(Number.of.Particles, Genotype)
    
    genelist<-unique(dfx[[genotype]])
    x<-combn(genelist,2)
    y<-length(x)/2
    x1<-parse_expr(genotype)
    x2<-parse_expr(Length)
    m<-matrix("-",nrow = length(genelist), ncol = length(genelist))
    ms<-matrix("-",nrow = length(genelist), ncol = length(genelist))
    
    m<-matrix("-",nrow = length(genelist), ncol = length(genelist))
    ms<-matrix("-",nrow = length(genelist), ncol = length(genelist))
    for (i in 1:y){
      #print(c(x[1,i],x[2,i]))
      #print(i)
      s<-shapiro.test(dfx$Number.of.Particles[dfx[[genotype]] %in% c(x[1,i],x[2,i])])
      
      if (s$p.value < 0.05){
        t<-wilcox.test(eval(x2) ~ eval(x1), data = dfx[dfx[[genotype]] %in% c(x[1,i],x[2,i]),], exact = FALSE)
      }
      else {
        t<-t.test(eval(x2) ~ eval(x1), data = dfx[dfx[[genotype]] %in% c(x[1,i],x[2,i]),], alternative = "two.sided", var.equal = FALSE)
      }
      
      m[which(genelist == x[1,i]),which(genelist == x[2,i])]<-as.numeric(t$p.value)
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
    write.xlsx(m, paste0(types[k], "_freq.xlsx"), sheetName = paste(markers[j]), append = TRUE)
    write.xlsx(ms, paste0(types[k], "_freq_sig.xlsx"), sheetName = paste(markers[j]), append = TRUE)
  }
  
}
