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
          
