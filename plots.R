
pkgList<-c("data.table","dplyr","tidyr","ggplot2","xlsx","readxl","RColorBrewer")
new.packages <- pkgList[!(pkgList %in% installed.packages()[,"Package"])]
if(length(new.packages)>0) install.packages(new.packages)

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xlsx)
library(readxl)
library(RColorBrewer)

options(warn=-1)

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

#windowsFonts("Open Sans SemiBold" = windowsFont("Open Sans SemiBold"))

# Dyf assay (head)
ggplot(data_head_plot, aes(x = Genotype, y = Count)) +
  geom_bar(aes(color = Phenotype, fill = Phenotype),
           stat = "identity", position = position_stack(), width = 0.8) +
  scale_fill_manual(values = prpls[c(3,5,7)]) +
  scale_color_brewer() +
  theme_classic() +
  coord_flip() +
  ylab("Count(%)") +
  ggtitle("Dyf Assay (Head)") +
  theme(text = element_text(family = "Open Sans SemiBold"))


data_tail_plot$Phenotype<-factor(data_tail_plot$Phenotype, levels = c("Normal","Partial","Abnormal"))
data_tail_plot$Genotype<-factor(data_tail_plot$Genotype, levels = rev(unique(data_tail_plot$Genotype)))

# Dyf assay (tail)
ggplot(data_tail_plot, aes(x = Genotype, y = Count)) +
  geom_bar(aes(color = Phenotype, fill = Phenotype),
           stat = "identity", position = position_stack(), width = 0.8) +
  scale_fill_manual(values = prpls[c(3,5,7)]) +
  scale_color_brewer() +
  theme_classic() +
  coord_flip() +
  ylab("Count(%)") +
  ggtitle("Dyf Assay (Tail)") +
  theme(text = element_text(family = "Open Sans SemiBold"))


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

 # Dyf assay head 2
ggplot(data2_head_plot, aes(x = Genotype, y = Count)) +
  geom_bar(aes(color = Phenotype, fill = Phenotype),
           stat = "identity", position = position_stack(), width = 0.8) +
  scale_fill_manual(values = prpls[c(3,5,7)]) +
  scale_color_brewer() +
  theme_classic() +
  coord_flip() +
  ylab("Count(%)") +
  ggtitle("Dyf Assay (Head)") +
  theme(text = element_text(family = "Open Sans SemiBold"),
        plot.title = element_text(hjust = 0.4))

data2_tail_plot$Phenotype<-factor(data2_tail_plot$Phenotype, levels = c("Normal","Partial","Abnormal"))
data2_tail_plot$Genotype<-factor(data2_tail_plot$Genotype, levels = rev(unique(data2_tail_plot$Genotype)))

# Dyf assay tail 2
ggplot(data2_tail_plot, aes(x = Genotype, y = Count)) +
  geom_bar(aes(color = Phenotype, fill = Phenotype),
           stat = "identity", position = position_stack(), width = 0.8) +
  scale_fill_manual(values = prpls[c(3,5,7)]) +
  scale_color_brewer() +
  theme_classic() +
  coord_flip() +
  ylab("Count(%)") +
  ggtitle("Dyf Assay (Tail)") +
  theme(text = element_text(family = "Open Sans SemiBold"),
        plot.title = element_text(hjust = 0.4))


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

# AWB cilia morphology
ggplot(morphology1, aes(x = Genotype, y = Count)) +
  geom_bar(aes(color = Phenotype, fill = Phenotype),
           stat = "identity", position = position_stack(), width = 0.8) +
  scale_fill_manual(values = prpls[c(3,5,7,9)]) +
  scale_color_brewer() +
  theme_classic() +
  coord_flip() +
  ylab("Count(%)") +
  ggtitle("AWB Cilia Morphology") +
  theme(text = element_text(family = "Open Sans SemiBold"),
        plot.title = element_text(hjust = 0.4))


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

# AWB cilia morphology 2
ggplot(morphology2, aes(x = Genotype, y = Count)) +
  geom_bar(aes(color = Phenotype, fill = Phenotype),
           stat = "identity", position = position_stack(), width = 0.8) +
  scale_fill_manual(values = prpls[c(3,5,7,9)]) +
  scale_color_brewer() +
  theme_classic() +
  coord_flip() +
  ylab("Count(%)") +
  ggtitle("AWB Cilia Morphology") +
  theme(text = element_text(family = "Open Sans SemiBold"),
        plot.title = element_text(hjust = 0.4))


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

# Short cilia length
ggplot(length_short1, aes(x = Genotype, y = Length, fill = Genotype)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, position=position_jitter(0.2), size = 1) +
  scale_fill_manual(values = rep(prpls[5],11)) +
  theme_minimal() +
  coord_flip() +
  ylab("Length(um)") +
  ggtitle("Short Cilia Length") +
  theme(legend.position = "none", text = element_text(family = "Open Sans SemiBold"),
        plot.title = element_text(hjust = 0.25))

length_long1$Genotype<-factor(length_long1$Genotype, levels = rev(unique(length_long1$Genotype)))

# Long cilia length
ggplot(length_long1, aes(x = Genotype, y = Length, fill = Genotype)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, position=position_jitter(0.2), size = 1) +
  scale_fill_manual(values = rep(prpls[5],11)) +
  theme_minimal() +
  coord_flip() +
  ylab("Length(um)") +
  ggtitle("Long Cilia Length") +
  theme(legend.position = "none", text = element_text(family = "Open Sans SemiBold"),
        plot.title = element_text(hjust = 0.25))


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

# Short cilia length 2
ggplot(lengthx_short1, aes(x = Genotype, y = Length, fill = Genotype)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, position=position_jitter(0.2), size = 1) +
  scale_fill_manual(values = rep(prpls[5],11)) +
  theme_minimal() +
  ylab("Length(um)") +
  ggtitle("Short Cilia Length") +
  theme(legend.position = "none", text = element_text(family = "Open Sans SemiBold"),
        plot.title = element_text(hjust = 0.5))

lengthx_long1$Genotype<-factor(lengthx_long1$Genotype, levels = unique(lengthx_long1$Genotype))

# Long cilia length 2
ggplot(lengthx_long1, aes(x = Genotype, y = Length, fill = Genotype)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, position=position_jitter(0.2), size = 1) +
  scale_fill_manual(values = rep(prpls[5],11)) +
  theme_minimal() +
  ylab("Length(um)") +
  ggtitle("Long Cilia Length") +
  theme(legend.position = "none", text = element_text(family = "Open Sans SemiBold"),
        plot.title = element_text(hjust = 0.5))


