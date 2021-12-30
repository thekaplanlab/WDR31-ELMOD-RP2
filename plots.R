
# This script generates plots from the paper
# Author: Mustafa S. Pir



source("data_wrangling.R")

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



# Short cilia length 2
ggplot(gcy_length1, aes(x = Genotype, y = Length, fill = Genotype)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, position=position_jitter(0.2), size = 1) +
  scale_fill_manual(values = rep(prpls[5],11)) +
  theme_minimal() +
  coord_flip() +
  ylab("Length(um)") +
  ggtitle("ASER Cilia Length") +
  theme(legend.position = "none", text = element_text(family = "Open Sans SemiBold"),
        plot.title = element_text(hjust = 0.35))



# Short cilia length 2
ggplot(srb6_length1, aes(x = Genotype, y = Length, fill = Genotype)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, position=position_jitter(0.2), size = 1) +
  scale_fill_manual(values = rep(prpls[5],11)) +
  theme_minimal() +
  coord_flip() +
  ylab("Length(um)") +
  ggtitle("PHA/PHB Cilia Length") +
  theme(legend.position = "none", text = element_text(family = "Open Sans SemiBold"),
        plot.title = element_text(hjust = 0.35))




# Short cilia awb_length

ggplot(awb_length1, aes(x = Genotype, y = length, fill = Genotype)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(shape=16, position=position_jitter(0.2), size = 1) +
  scale_fill_manual(values = rep(prpls[5],11)) +
  theme_minimal() +
  coord_flip() +
  ylab("Length(um)") +
  ggtitle("AWB Cilia Length (short)") +
  theme(legend.position = "none", text = element_text(family = "Open Sans SemiBold"),
        plot.title = element_text(hjust = 0.35))




# IFT figures
ggplot(ift[ift$IFT == "Anterograde" & ift$Marker != "BBS-7::GFP",], aes(x = Marker, y = Number.of.Particles, fill = Genotype)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_jitter(shape=16, position=position_jitter(0.2), size = 1) +
  #geom_boxplot(outlier.shape = NA) +
  #scale_color_brewer(palette = "Paired") +
  #scale_fill_manual(values = rep(prpls[5],11)) +
  theme_minimal() +
  #coord_flip() +
  ylab("Number of Particles") +
  ggtitle("Anterograde") +
  theme(legend.position = "right", text = element_text(family = "Open Sans SemiBold", size = 12),
        plot.title = element_text(hjust = 0.6))



ggplot(ift[ift$IFT == "Retrograde" & ift$Marker != "BBS-7::GFP",], aes(x = Marker, y = Number.of.Particles, fill = Genotype)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_jitter(shape=16, position=position_jitter(0.2), size = 1) +
  #geom_boxplot(outlier.shape = NA) +
  #scale_color_brewer(palette = "Paired") +
  #scale_fill_manual(values = rep(prpls[5],11)) +
  theme_minimal() +
  #coord_flip() +
  ylab("Number of Particles") +
  ggtitle("Retrograde") +
  theme(legend.position = "right", text = element_text(family = "Open Sans SemiBold", size = 12),
        plot.title = element_text(hjust = 0.6))



ggplot(ift[ift$IFT == "Anterograde" & ift$Marker == "BBS-7::GFP",], aes(x = Marker, y = Number.of.Particles, fill = Genotype)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_jitter(shape=16, position=position_jitter(0.2), size = 1) +
  #geom_boxplot(outlier.shape = NA) +
  #scale_color_brewer(palette = "Paired") +
  #scale_fill_manual(values = rep(prpls[5],11)) +
  theme_minimal() +
  #coord_flip() +
  ylab("Number of Particles") +
  ggtitle("Anterograde") +
  theme(legend.position = "right", text = element_text(family = "Open Sans SemiBold", size = 12),
        plot.title = element_text(hjust = 0.6))



ggplot(ift[ift$IFT == "Retrograde" & ift$Marker == "BBS-7::GFP",], aes(x = Marker, y = Number.of.Particles, fill = Genotype)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_jitter(shape=16, position=position_jitter(0.2), size = 1) +
  #geom_boxplot(outlier.shape = NA) +
  #scale_color_brewer(palette = "Paired") +
  #scale_fill_manual(values = rep(prpls[5],11)) +
  theme_minimal() +
  #coord_flip() +
  ylab("Number of Particles") +
  ggtitle("Retrograde") +
  theme(legend.position = "right", text = element_text(family = "Open Sans SemiBold", size = 12),
        plot.title = element_text(hjust = 0.6))




