library(readxl)
library(tidyr)
library(dplyr)
library(patchwork)
library(cowplot)
library(ggplot2)

options(warn=-1)

# Prepare data for first plot ----

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


# First plot ----

plot1<-list()

for (i in 1:4){
  iftplot<-ifts[[i]]
  typelist<-unique(iftplot$Part)
  genelist<-c("IFT-74::GFP","OSM-6::GFP","OSM-3::GFP","CHE-11::GFP")
  iftplot$Part<-as.factor(iftplot$Part)
  iftplot$Genotype<-factor(iftplot$Genotype, levels = unique(iftplot$Genotype))
  do.call(patchwork::wrap_plots, c(lapply(typelist, function(a) {
    ggplot(iftplot[iftplot$Part == a,], aes(x = Genotype, y = Speed, fill = Genotype)) +
      geom_boxplot(outlier.shape = NA) +
      scale_fill_manual(values=c("antiquewhite", "lightblue", "antiquewhite3", "antiquewhite4")) +
      #scale_fill_brewer(palette="Accent") +
      theme_classic() +
      ylab("IFT Velocity") +
      ggtitle(a) +
      theme(legend.position = "right", text = element_text(family = "Open Sans SemiBold", size = 12),
            plot.title = element_text(hjust = 0.6), axis.text.x=element_blank(), axis.title.x=element_blank())
    
  }), nrow = 1, ncol = 3, guides = "collect")) -> plot
  plot<-plot + plot_annotation(title = genelist[i], theme = theme(plot.title = element_text(hjust = 0.5)))
  plot1[[i]]<-plot
}

plot_grid(plotlist = plot1, labels = "AUTO")
ggsave("ift_frequency.jpg", width = 30, height = 10, dpi = 300) 


  # Prepare data for 2nd plot ----

iftz<-suppressMessages(read_xlsx("IFT_Velocity_IFT_frequency_edited.xlsx", sheet = 5))
iftz<-iftz %>% select(!grep("number", as.character(as.vector(iftz[2,]))))
iftz<-iftz[c(-1,-2),]
iftz_long<-pivot_longer(iftz, everything(), names_to = "Genotype2", values_to = "Speed1", values_drop_na = TRUE)

iftz2<-suppressMessages(read_xlsx("IFT_Velocity_IFT_frequency_edited.xlsx", sheet = 5, skip = 1))
iftz2<-iftz2 %>% select(!grep("number", as.character(as.vector(iftz2[1,]))))
iftz2<-iftz2[-1,]
iftz_long_2<-pivot_longer(iftz2, everything(), names_to = "Genotype", values_to = "Speed2", values_drop_na = TRUE)

iftz3<-suppressMessages(read_xlsx("IFT_Velocity_IFT_frequency_edited.xlsx", sheet = 5, skip = 2))
iftz3<-iftz3 %>% select(!grep("number", colnames(iftz3)))
iftz_long_3<-pivot_longer(iftz3, everything(), names_to = "Part", values_to = "Speed", values_drop_na = TRUE)


iftz_all<-cbind(iftz_long, iftz_long_2, iftz_long_3)
iftz_all<-iftz_all %>% select(-Speed1, -Speed2)
iftz_all[,1:3]<-as.data.frame(lapply(iftz_all[,1:3], function(x) gsub("\\.\\.\\.[0-9]+", "", x)))


# 2nd plot ----

plot2<-list()
typelist2<-unique(iftz_all$Part)
genelist2<-unique(iftz_all$Genotype2)

for (i in 1:length(genelist2)){
  iftplot2<-iftz_all[iftz_all$Genotype2 == genelist2[i],]
  iftplot2$Genotype<-factor(iftplot2$Genotype, levels = unique(iftplot2$Genotype))
  plot<-do.call(patchwork::wrap_plots, c(lapply(typelist2, function(a) {
    ggplot(iftplot2[iftplot2$Part == a,], aes(x = Genotype, y = Speed, fill = Genotype)) +
      geom_boxplot(outlier.shape = NA) +
      scale_fill_manual(values=c("antiquewhite", "lightblue", "antiquewhite3", "antiquewhite4", "lightyellow",  "yellow3")) +
      #scale_fill_brewer(palette = "Accent") +
      theme_classic()+
      ylab("IFT Frequency") +
      ggtitle(a) +
      theme(legend.position = "right", text = element_text(family = "Open Sans SemiBold", size = 12),
            plot.title = element_text(hjust = 0.6), axis.text.x=element_blank(), axis.title.x=element_blank())
  }), nrow = 1, ncol = 3, guides = "collect")) +
    plot_annotation(title = genelist2[i], theme = theme(plot.title = element_text(hjust = 0.5)))
  
  plot2[[i]]<-plot
}

plot_grid(plotlist = plot2, labels = "AUTO")
ggsave("ift.jpg", width = 30, height = 10, dpi = 300) 
          

ift2 <- ifts[[1]]
ift2$Marker <- "IFT-74::GFP"

for (i in 2:4) {
  ift22 <- ifts[[i]]
  genelist <- c("IFT-74::GFP", "OSM-6::GFP", "OSM-3::GFP", "CHE-11::GFP")
  ift22$Marker <- genelist[i]
  ift2 <- rbind(ift2, ift22)
}
colnames(ift2)[2:3] <- c("IFT", "Number.of.Particles")

# IFT

df <- ift2
df$IFT <- as.character(df$IFT)
df$Genotype <- as.character(df$Genotype)
df$Marker <- as.character(df$Marker)
Length <- "Number.of.Particles"
genotype <- "Genotype"

markers <- unique(df$Marker)
markers <- gsub(":", "", markers)

# Retrograde
types <- unique(df$IFT)
for (k in 1:length(types)) {
  for (j in 1:length(markers)) {
    dfx <- df %>%
      filter(Marker == unique(df$Marker)[j] & IFT == types[k]) %>%
      dplyr::select(Number.of.Particles, Genotype)
    
    genelist <- unique(dfx[[genotype]])
    x <- combn(genelist, 2)
    y <- length(x) / 2
    x1 <- parse_expr(genotype)
    x2 <- parse_expr(Length)
    m <- matrix("-", nrow = length(genelist), ncol = length(genelist))
    ms <- matrix("-", nrow = length(genelist), ncol = length(genelist))
    
    m <- matrix("-", nrow = length(genelist), ncol = length(genelist))
    ms <- matrix("-", nrow = length(genelist), ncol = length(genelist))
    for (i in 1:y) {
      # print(c(x[1,i],x[2,i]))
      # print(i)
      s <- shapiro.test(dfx$Number.of.Particles[dfx[[genotype]] %in% c(x[1, i], x[2, i])])
      
      if (s$p.value < 0.05) {
        t <- wilcox.test(eval(x2) ~ eval(x1), data = dfx[dfx[[genotype]] %in% c(x[1, i], x[2, i]), ], exact = FALSE)
      } else {
        t <- t.test(eval(x2) ~ eval(x1), data = dfx[dfx[[genotype]] %in% c(x[1, i], x[2, i]), ], alternative = "two.sided", var.equal = FALSE)
      }
      
      m[which(genelist == x[1, i]), which(genelist == x[2, i])] <- as.numeric(t$p.value)
      if (t$p.value < 0.05 && t$p.value >= 0.01) {
        ms[which(genelist == x[1, i]), which(genelist == x[2, i])] <- "*"
      } else if (t$p.value < 0.01 && t$p.value >= 0.001) {
        ms[which(genelist == x[1, i]), which(genelist == x[2, i])] <- "**"
      } else if (t$p.value < 0.001 && t$p.value >= 0.0001) {
        ms[which(genelist == x[1, i]), which(genelist == x[2, i])] <- "***"
      } else if (t$p.value < 0.0001) {
        ms[which(genelist == x[1, i]), which(genelist == x[2, i])] <- "****"
      } else {
        ms[which(genelist == x[1, i]), which(genelist == x[2, i])] <- "ns"
      }
    }
    rownames(m) <- genelist
    colnames(m) <- genelist
    rownames(ms) <- genelist
    colnames(ms) <- genelist
    m <- t(m)
    ms <- t(ms)
    m <- as_tibble(m, rownames = NA)
    ms <- as_tibble(ms, rownames = NA)
    write.xlsx(m, paste0(types[k], "_velocity.xlsx"), sheetName = paste(markers[j]), append = TRUE)
    write.xlsx(ms, paste0(types[k], "_velocity_sig.xlsx"), sheetName = paste(markers[j]), append = TRUE)
  }
}

df2 <- iftz_all
colnames(df2) <- c("Marker", "Genotype", "IFT", "Number.of.Particles")

df2$IFT <- as.character(df2$IFT)
df2$Genotype <- as.character(df2$Genotype)
df2$Marker <- as.character(df2$Marker)
Length <- "Number.of.Particles"
genotype <- "Genotype"

markers <- unique(df2$Marker)
markers <- gsub(":", "", markers)

# Retrograde
types <- unique(df2$IFT)
for (k in 1:length(types)) {
  for (j in 1:length(markers)) {
    dfx <- df2 %>%
      filter(Marker == unique(df2$Marker)[j] & IFT == types[k]) %>%
      dplyr::select(Number.of.Particles, Genotype)
    
    genelist <- unique(dfx[[genotype]])
    x <- combn(genelist, 2)
    y <- length(x) / 2
    x1 <- parse_expr(genotype)
    x2 <- parse_expr(Length)
    m <- matrix("-", nrow = length(genelist), ncol = length(genelist))
    ms <- matrix("-", nrow = length(genelist), ncol = length(genelist))
    
    m <- matrix("-", nrow = length(genelist), ncol = length(genelist))
    ms <- matrix("-", nrow = length(genelist), ncol = length(genelist))
    for (i in 1:y) {
      # print(c(x[1,i],x[2,i]))
      # print(i)
      s <- shapiro.test(dfx$Number.of.Particles[dfx[[genotype]] %in% c(x[1, i], x[2, i])])
      
      if (s$p.value < 0.05) {
        t <- wilcox.test(eval(x2) ~ eval(x1), data = dfx[dfx[[genotype]] %in% c(x[1, i], x[2, i]), ], exact = FALSE)
      } else {
        t <- t.test(eval(x2) ~ eval(x1), data = dfx[dfx[[genotype]] %in% c(x[1, i], x[2, i]), ], alternative = "two.sided", var.equal = FALSE)
      }
      
      m[which(genelist == x[1, i]), which(genelist == x[2, i])] <- as.numeric(t$p.value)
      if (t$p.value < 0.05 && t$p.value >= 0.01) {
        ms[which(genelist == x[1, i]), which(genelist == x[2, i])] <- "*"
      } else if (t$p.value < 0.01 && t$p.value >= 0.001) {
        ms[which(genelist == x[1, i]), which(genelist == x[2, i])] <- "**"
      } else if (t$p.value < 0.001 && t$p.value >= 0.0001) {
        ms[which(genelist == x[1, i]), which(genelist == x[2, i])] <- "***"
      } else if (t$p.value < 0.0001) {
        ms[which(genelist == x[1, i]), which(genelist == x[2, i])] <- "****"
      } else {
        ms[which(genelist == x[1, i]), which(genelist == x[2, i])] <- "ns"
      }
    }
    rownames(m) <- genelist
    colnames(m) <- genelist
    rownames(ms) <- genelist
    colnames(ms) <- genelist
    m <- t(m)
    ms <- t(ms)
    m <- as_tibble(m, rownames = NA)
    ms <- as_tibble(ms, rownames = NA)
    write.xlsx(m, paste0(types[k], "_freq.xlsx"), sheetName = paste(markers[j]), append = TRUE)
    write.xlsx(ms, paste0(types[k], "_freq_sig.xlsx"), sheetName = paste(markers[j]), append = TRUE)
  }
}


# 3rd plot ----

plot3<-list()
typelist3<-unique(iftzn_all$Part)
genelist3<-unique(iftzn_all$Genotype2)

for (i in 1:length(genelist3)){
  iftplot3<-iftzn_all[iftzn_all$Genotype2 == genelist3[i],]
  iftplot3$Genotype<-factor(iftplot3$Genotype, levels = unique(iftplot3$Genotype))
  plot<-do.call(patchwork::wrap_plots, c(lapply(typelist3, function(a) {
    ggplot(iftplot3[iftplot3$Part == a,], aes(x = Genotype, y = Speed, fill = Genotype)) +
      geom_boxplot(outlier.shape = NA) +
      scale_fill_manual(values=c("antiquewhite", "lightblue", "antiquewhite3", "antiquewhite4", "lightyellow",  "yellow3")) +
      #scale_fill_brewer(palette = "Accent") +
      theme_classic()+
      ylab("IFT Frequency") +
      ggtitle(a) +
      theme(legend.position = "right", text = element_text(family = "Open Sans SemiBold", size = 12),
            plot.title = element_text(hjust = 0.6), axis.text.x=element_blank(), axis.title.x=element_blank())
  }), nrow = 1, ncol = 3, guides = "collect")) +
    plot_annotation(title = genelist3[i], theme = theme(plot.title = element_text(hjust = 0.3)))
  
  plot3[[i]]<-plot
}

plot_grid(plotlist = plot3, labels = "AUTO", nrow = 3)
ggsave("ift3.jpg", width = 30, height = 20, dpi = 300) 






