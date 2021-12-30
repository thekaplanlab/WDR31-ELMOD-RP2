

# This script prepares the data to be used in plots.R and stats.R scripts
# Please do not run this script

# Author: Mustafa S. Pir



pkgList<-c("data.table","dplyr","tidyr","ggplot2","xlsx","readxl","RColorBrewer","forcats")
new.packages <- pkgList[!(pkgList %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages)

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xlsx)
library(readxl)
library(RColorBrewer)
library(forcats)

options(warn=-1)

# data for stats
data_fhead<-fread("./files/dye_assay1.csv") %>%
  select(1,2,4,6)
colnames(data_fhead)[2:4]<-c("Normal","Partial","Abnormal")

data_ftail<-fread("./files/dye_assay1.csv") %>%
  select(1,3,5,7)
colnames(data_ftail)[2:4]<-c("Normal","Partial","Abnormal")

wdr31_dyf<-fread("./files/dye_assay2.csv")
wdr31_dyf_head<-fread("./files/dye_assay2.csv") %>%
  select(1,2,4,6)
colnames(wdr31_dyf_head)[2:4]<-c("Normal","Partial","Abnormal")

wdr31_dyf_tail<-fread("./files/dye_assay2.csv") %>%
  select(1,3,5,7)
colnames(wdr31_dyf_tail)[2:4]<-c("Normal","Partial","Abnormal")


morphology_stat<-read_xlsx("./files/AWB_Cilia_Morphology_Analysis.xlsx")
morphology1_stat<-morphology_stat[1:11,]
morphology2_stat<-morphology_stat[12:16,]
morphology3_stat<-morphology_stat[17:19,]
morphology4_stat<-morphology_stat[20:23,]


ift<-read.xlsx("./files/ift1.xlsx", sheetIndex = 1)
ift$Genotype[ift$Genotype == "wdr-31(tm10423);eldm-1"]<-"wdr-31(tm10423);elmd-1"
ift$Genotype[ift$Genotype == "wdr-31(tm10423);eldm-1;rpi-2 "]<-"wdr-31(tm10423);elmd-1;rpi-2"

ift$Genotype<-factor(ift$Genotype, levels = unique(ift$Genotype))
ift$Marker<-factor(ift$Marker, levels = unique(ift$Marker))


# Data for plots

data_head_plot<-fread("./files/dye_assay1.csv") %>%
  select(1,2,4,6) %>%
  mutate(Normal = 100*(`Head(Normal)` / (`Head(Normal)` + `Head(Partial)` + `Head(Abnormal)`))) %>%
  mutate(Partial = 100*(`Head(Partial)` / (`Head(Normal)` + `Head(Partial)` + `Head(Abnormal)`))) %>%
  mutate(Abnormal = 100*(`Head(Abnormal)` / (`Head(Normal)` + `Head(Partial)` + `Head(Abnormal)`))) %>%
  pivot_longer(cols = 5:7,
               names_to = "Phenotype",
               values_to = "Count")

data_tail_plot<-fread("./files/dye_assay1.csv") %>%
  select(1,3,5,7) %>%
  mutate(Normal = 100*(`Tail(Normal)` / (`Tail(Normal)` + `Tail(Partial)` + `Tail(Abnormal)`))) %>%
  mutate(Partial = 100*(`Tail(Partial)` / (`Tail(Normal)` + `Tail(Partial)` + `Tail(Abnormal)`))) %>%
  mutate(Abnormal = 100*(`Tail(Abnormal)` / (`Tail(Normal)` + `Tail(Partial)` + `Tail(Abnormal)`))) %>%
  pivot_longer(cols = 5:7,
               names_to = "Phenotype",
               values_to = "Count")

prpls<-brewer.pal(9, "Purples")


data_head_plot$Phenotype<-factor(data_head_plot$Phenotype, levels = c("Normal","Partial","Abnormal"))
data_head_plot$Genotype<-factor(data_head_plot$Genotype, levels = rev(unique(data_head_plot$Genotype)))

data_tail_plot$Phenotype<-factor(data_tail_plot$Phenotype, levels = c("Normal","Partial","Abnormal"))
data_tail_plot$Genotype<-factor(data_tail_plot$Genotype, levels = rev(unique(data_tail_plot$Genotype)))


#Table 2
data2_head_plot<-fread("./files/dye_assay2.csv") %>%
  select(1,2,4,6) %>%
  mutate(Normal = 100*(`Head(Normal)` / (`Head(Normal)` + `Head(Partial)` + `Head(Abnormal)`))) %>%
  mutate(Partial = 100*(`Head(Partial)` / (`Head(Normal)` + `Head(Partial)` + `Head(Abnormal)`))) %>%
  mutate(Abnormal = 100*(`Head(Abnormal)` / (`Head(Normal)` + `Head(Partial)` + `Head(Abnormal)`))) %>%
  pivot_longer(cols = 5:7,
               names_to = "Phenotype",
               values_to = "Count")

data2_tail_plot<-fread("./files/dye_assay2.csv") %>%
  select(1,3,5,7) %>%
  mutate(Normal = 100*(`Tail(Normal)` / (`Tail(Normal)` + `Tail(Partial)` + `Tail(Abnormal)`))) %>%
  mutate(Partial = 100*(`Tail(Partial)` / (`Tail(Normal)` + `Tail(Partial)` + `Tail(Abnormal)`))) %>%
  mutate(Abnormal = 100*(`Tail(Abnormal)` / (`Tail(Normal)` + `Tail(Partial)` + `Tail(Abnormal)`))) %>%
  pivot_longer(cols = 5:7,
               names_to = "Phenotype",
               values_to = "Count")

data2_head_plot$Phenotype<-factor(data2_head_plot$Phenotype, levels = c("Normal","Partial","Abnormal"))
data2_head_plot$Genotype<-factor(data2_head_plot$Genotype, levels = rev(unique(data2_head_plot$Genotype)))

data2_tail_plot$Phenotype<-factor(data2_tail_plot$Phenotype, levels = c("Normal","Partial","Abnormal"))
data2_tail_plot$Genotype<-factor(data2_tail_plot$Genotype, levels = rev(unique(data2_tail_plot$Genotype)))


# Morphology
morphology<-read_xlsx("./files/AWB_Cilia_Morphology_Analysis.xlsx")
morphology1<-morphology[1:11,]
morphology2<-morphology[12:14,]
morphology3<-morphology[18:20,]
morphology4<-morphology[21:24,]

morphology1<-morphology1 %>%
  mutate(Normal = 100*(Normal1 / (Normal1 + Fan1 + `Extra Branch1` + `Backward projection1`))) %>%
  mutate(Fan = 100*(Fan1 / (Normal1 + Fan1 + `Extra Branch1` + `Backward projection1`))) %>%
  mutate(`Extra Branch` = 100*(`Extra Branch1` / (Normal1 + Fan1 + `Extra Branch1` + `Backward projection1`))) %>%
  mutate(`Backward Projection` = 100*(`Backward projection1` / (Normal1 + Fan1 + `Extra Branch1` + `Backward projection1`))) %>%
  pivot_longer(cols = 6:9,
               names_to = "Phenotype",
               values_to = "Count")
colnames(morphology1)[1]<-"Genotype"

morphology1$Phenotype<-factor(morphology1$Phenotype, levels = c("Normal","Fan","Extra Branch","Backward Projection"))
morphology1$Genotype<-factor(morphology1$Genotype, levels = rev(unique(morphology1$Genotype)))


morphology2<-morphology2 %>%
  mutate(Normal = 100*(Normal1 / (Normal1 + Fan1 + `Extra Branch1` + `Backward projection1`))) %>%
  mutate(Fan = 100*(Fan1 / (Normal1 + Fan1 + `Extra Branch1` + `Backward projection1`))) %>%
  mutate(`Extra Branch` = 100*(`Extra Branch1` / (Normal1 + Fan1 + `Extra Branch1` + `Backward projection1`))) %>%
  mutate(`Backward Projection` = 100*(`Backward projection1` / (Normal1 + Fan1 + `Extra Branch1` + `Backward projection1`))) %>%
  pivot_longer(cols = 6:9,
               names_to = "Phenotype",
               values_to = "Count")
colnames(morphology2)[1]<-"Genotype"

morphology2$Phenotype<-factor(morphology2$Phenotype, levels = c("Normal","Fan","Extra Branch","Backward Projection"))
morphology2$Genotype<-factor(morphology2$Genotype, levels = rev(unique(morphology2$Genotype)))


# AWB length
length<-read.xlsx("./files/AWB_Cilia_Lenght_new.xlsx", sheetIndex = 1)
length<-length[1:57,1:23]
colnames(length)<-length[1,]
for (i in seq(2,22,2)){
  colnames(length)[i+1]<-colnames(length)[i]
}
length<-length[-1,]

length_short<-select(length, seq(3,23,2))
length_short<-length_short[-1,]
length_long<-select(length, seq(2,22,2))
length_long<-length_long[-1,]
length_short<-sapply(length_short, as.numeric)
length_long<-sapply(length_long, as.numeric)

length_short1<-pivot_longer(as_tibble(length_short), cols = 1:11, names_to = "Genotype", values_to = "Length")
length_long1<-pivot_longer(as_tibble(length_long), cols = 1:11, names_to = "Genotype", values_to = "Length")
length_short1<-length_short1[!is.na(length_short1$Length),]
length_long1<-length_long1[!is.na(length_long1$Length),]
length_short1$Genotype<-factor(length_short1$Genotype, levels = rev(unique(length_short1$Genotype)))
length_long1$Genotype<-factor(length_long1$Genotype, levels = rev(unique(length_long1$Genotype)))


lengthx<-read.xlsx("./files/AWB_Cilia_Lenght_new.xlsx", sheetIndex = 2)
lengthx<-lengthx[1:52,1:7]
colnames(lengthx)<-lengthx[1,]
for (i in seq(2,6,2)){
  colnames(lengthx)[i+1]<-colnames(lengthx)[i]
}
lengthx<-lengthx[-1,]

lengthx_short<-select(lengthx, seq(3,7,2))
lengthx_short<-lengthx_short[-1,]
lengthx_long<-select(lengthx, seq(2,6,2))
lengthx_long<-lengthx_long[-1,]
lengthx_short<-sapply(lengthx_short, as.numeric)
lengthx_long<-sapply(lengthx_long, as.numeric)

lengthx_short1<-pivot_longer(as_tibble(lengthx_short), cols = 1:3, names_to = "Genotype", values_to = "Length")
lengthx_long1<-pivot_longer(as_tibble(lengthx_long), cols = 1:3, names_to = "Genotype", values_to = "Length")
lengthx_short1<-lengthx_short1[!is.na(lengthx_short1$Length),]
lengthx_long1<-lengthx_long1[!is.na(lengthx_long1$Length),]

lengthx_short1$Genotype<-factor(lengthx_short1$Genotype, levels = unique(lengthx_short1$Genotype))
lengthx_long1$Genotype<-factor(lengthx_long1$Genotype, levels = unique(lengthx_long1$Genotype))

# ASER length
gcy_length<-read.xlsx("./files/gcy-5_and_srb-6p.xlsx", sheetIndex = 1, check.names = FALSE)
gcy_length<-gcy_length[1:86,2:9]

gcy_length<-sapply(gcy_length, as.numeric)
gcy_length1<-pivot_longer(as_tibble(gcy_length), cols = 1:8, names_to = "Genotype", values_to = "Length")
gcy_length1<-gcy_length1[!is.na(gcy_length1$Length),]
gcy_length1$Genotype<-factor(gcy_length1$Genotype, levels = unique(gcy_length1$Genotype))
gcy_length1$Genotype<-fct_rev(gcy_length1$Genotype)

# PHA/PHB length
srb6_length<-read.xlsx("./files/gcy-5_and_srb-6p.xlsx", sheetIndex = 2, check.names = FALSE)
srb6_length<-srb6_length[,2:9]

srb6_length<-sapply(srb6_length, as.numeric)
srb6_length1<-pivot_longer(as_tibble(srb6_length), cols = 1:8, names_to = "Genotype", values_to = "Length")
srb6_length1<-srb6_length1[!is.na(srb6_length1$Length),]
srb6_length1$Genotype<-factor(srb6_length1$Genotype, levels = unique(srb6_length1$Genotype))
srb6_length1$Genotype<-fct_rev(srb6_length1$Genotype)

# AWB length 2
awb_length<-read.xlsx("./files/awb_length.xlsx", sheetIndex = 1)
awb_length<-awb_length[1:57,1:23]
colnames(awb_length)<-awb_length[1,]
for (i in seq(2,22,2)){
  colnames(awb_length)[i+1]<-colnames(awb_length)[i]
}
awb_length<-awb_length[-1,]

awb_length<-select(awb_length, seq(3,23,2))
awb_length<-awb_length[-1,]
awb_length<-sapply(awb_length, as.numeric)
awb_length<-awb_length[,c(-6,-10,-11)]

awb_length1<-pivot_longer(as_tibble(awb_length), cols = 1:8, names_to = "Genotype", values_to = "length")
awb_length1<-awb_length1[!is.na(awb_length1$length),]
awb_length1$Genotype<-factor(awb_length1$Genotype, levels = rev(unique(awb_length1$Genotype)))



