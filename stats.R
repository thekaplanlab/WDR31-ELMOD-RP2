

# This script generates excel files containing statistical analysis of data generated in the paper
# Author: Mustafa S. Pir

source("data_wrangling.R")
source("functions.R")


a<-fisher_pairwise_test(data_fhead, significance = TRUE)
b<-fisher_pairwise_test(data_fhead)

c<-fisher_pairwise_test(data_ftail, significance = TRUE)
d<-fisher_pairwise_test(data_ftail)

a1<-fisher_pairwise_test(wdr31_dyf_head, significance = TRUE)
b1<-fisher_pairwise_test(wdr31_dyf_head)

c1<-fisher_pairwise_test(wdr31_dyf_tail, significance = TRUE)
d1<-fisher_pairwise_test(wdr31_dyf_tail)


write.xlsx(a, "dyf_assay_stats.xlsx", sheetName = "head_significance", col.names = TRUE, append = FALSE)
write.xlsx(c, "dyf_assay_stats.xlsx", sheetName = "tail_significance", col.names = TRUE, append = TRUE)
write.xlsx(b, "dyf_assay_stats.xlsx", sheetName = "head_p_val", col.names = TRUE, append = TRUE)
write.xlsx(d, "dyf_assay_stats.xlsx", sheetName = "tail_p_val", col.names = TRUE, append = TRUE)
write.xlsx(a1, "dyf_assay_stats.xlsx", sheetName = "wdr31_head_significance", col.names = TRUE, append = TRUE)
write.xlsx(c1, "dyf_assay_stats.xlsx", sheetName = "wdr31_tail_significance", col.names = TRUE, append = TRUE)
write.xlsx(b1, "dyf_assay_stats.xlsx", sheetName = "wdr31_head_p_val", col.names = TRUE, append = TRUE)
write.xlsx(d1, "dyf_assay_stats.xlsx", sheetName = "wdr31_tail_p_val", col.names = TRUE, append = TRUE)




a2<-fisher_pairwise_test(morphology1_stat, significance = TRUE)
b2<-fisher_pairwise_test(morphology1_stat)

c2<-fisher_pairwise_test(morphology2_stat, significance = TRUE)
d2<-fisher_pairwise_test(morphology2_stat)

a3<-fisher_pairwise_test(morphology3_stat, significance = TRUE)
b3<-fisher_pairwise_test(morphology3_stat)

c3<-fisher_pairwise_test(morphology4_stat, significance = TRUE)
d3<-fisher_pairwise_test(morphology4_stat)


write.xlsx(a2, "morphology.xlsx", sheetName = "all_significance", col.names = TRUE)
write.xlsx(c2, "morphology.xlsx", sheetName = "wdr31_significance", col.names = TRUE, append = TRUE)
write.xlsx(a3, "morphology.xlsx", sheetName = "ankr26_significance", col.names = TRUE, append = TRUE)
write.xlsx(c3, "morphology.xlsx", sheetName = "rescue_significance", col.names = TRUE, append = TRUE)
write.xlsx(b2, "morphology.xlsx", sheetName = "all_p_val", col.names = TRUE, append = TRUE)
write.xlsx(d2, "morphology.xlsx", sheetName = "wdr31_p_val", col.names = TRUE, append = TRUE)
write.xlsx(b3, "morphology.xlsx", sheetName = "ankr26_p_val", col.names = TRUE, append = TRUE)
write.xlsx(d3, "morphology.xlsx", sheetName = "rescue_p_val", col.names = TRUE, append = TRUE)



short_length_s<-wilcox_pairwise_test(length_short1, "Genotype", "Length", significance = TRUE)
short_length<-wilcox_pairwise_test(length_short1, "Genotype", "Length")
long_length_s<-wilcox_pairwise_test(length_long1, "Genotype", "Length", significance = TRUE)
long_length<-wilcox_pairwise_test(length_long1, "Genotype", "Length")

srb_length_s<-wilcox_pairwise_test(srb6_length1, "Genotype", "Length", significance = TRUE)
srb_length_p<-wilcox_pairwise_test(srb6_length1, "Genotype", "Length")

gcy_length1_s<-wilcox_pairwise_test(gcy_length1, "Genotype", "Length", significance = TRUE)
gcy_length1_p<-wilcox_pairwise_test(gcy_length1, "Genotype", "Length")



write.xlsx(long_length_s, "cilia_length.xlsx", sheetName = "Long_cilia_significance", col.names = TRUE)
write.xlsx(short_length_s, "cilia_length.xlsx", sheetName = "Short_cilia_significance", col.names = TRUE, append = TRUE)
write.xlsx(long_length, "cilia_length.xlsx", sheetName = "Long_cilia_p_val", col.names = TRUE, append = TRUE)
write.xlsx(short_length, "cilia_length.xlsx", sheetName = "Short_cilia_p_val", col.names = TRUE, append = TRUE)
write.xlsx(srb_length_s, "cilia_length.xlsx", sheetName = "PHA_PHB_cilia_significance", col.names = TRUE, append = TRUE)
write.xlsx(srb_length_p, "cilia_length.xlsx", sheetName = "PHA_PHB_cilia_p_val", col.names = TRUE, append = TRUE)
write.xlsx(gcy_length1_s, "cilia_length.xlsx", sheetName = "ASER_cilia_significance", col.names = TRUE, append = TRUE)
write.xlsx(gcy_length1_p, "cilia_length.xlsx", sheetName = "ASER_cilia_p_val", col.names = TRUE, append = TRUE)


# IFT

df<-ift
df$IFT<-as.character(df$IFT)
df$Genotype<-as.character(df$Genotype)
df$Marker<-as.character(df$Marker)
Length<-"Number.of.Particles"
genotype<-"Genotype"  

genelist<-unique(df[[genotype]])
x<-combn(genelist,2)
y<-length(x)/2
x1<-parse_expr(genotype)
x2<-parse_expr(Length)
m<-matrix("-",nrow = length(genelist), ncol = length(genelist))
ms<-matrix("-",nrow = length(genelist), ncol = length(genelist))
markers<-unique(df$Marker)
markers<-gsub(":", "", markers)

# Retrograde
for (j in 1:length(markers)){
  dfx<-df %>%
    filter(Marker == unique(df$Marker)[j] & IFT == "Retrograde") %>%
    dplyr::select(Number.of.Particles, Genotype)
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
  write.xlsx(m, "Retrograde.xlsx", sheetName = paste(markers[j]), append = TRUE)
  write.xlsx(ms, "Retrograde_sig.xlsx", sheetName = paste(markers[j]), append = TRUE)
}



# Anterograde
for (j in 1:length(markers)){
  dfy<-df %>%
    filter(Marker == unique(df$Marker)[j] & IFT == "Anterograde") %>%
    dplyr::select(Number.of.Particles, Genotype)
  m<-matrix("-",nrow = length(genelist), ncol = length(genelist))
  ms<-matrix("-",nrow = length(genelist), ncol = length(genelist))
  for (i in 1:y){
    #print(c(x[1,i],x[2,i]))
    #print(i)
    s<-shapiro.test(dfy$Number.of.Particles[dfy[[genotype]] %in% c(x[1,i],x[2,i])])
    
    if (s$p.value < 0.05){
      t<-wilcox.test(eval(x2) ~ eval(x1), data = dfy[dfy[[genotype]] %in% c(x[1,i],x[2,i]),], exact = FALSE)
    }
    else {
      t<-t.test(eval(x2) ~ eval(x1), data = dfy[dfy[[genotype]] %in% c(x[1,i],x[2,i]),], alternative = "two.sided", var.equal = FALSE)
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
  write.xlsx(m, "Anterograde.xlsx", sheetName = paste(markers[j]), append = TRUE)
  write.xlsx(ms, "Anterograde_sig.xlsx", sheetName = paste(markers[j]), append = TRUE)
}

