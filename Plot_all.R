###################################################################################################

###install.packages
## install.packages("tidyverse")
library(tidyverse)
library(RColorBrewer)
library(ggsignif)
library(dplyr)
library(usethis)
library(devtools)
library(tidyr)
library(ggplot2)
library(wesanderson)
library(scales)
library(reshape2)
library(plyr) 
library(grid)
library(gridExtra)
library(lattice)
library(patchwork)
library(devtools)
library(ggbiplot)
library(RColorBrewer)
library(vioplot)
library(ggridges)
library("stringr")


##########################################################################################################################################################################
##ggplot
library(tidyverse)
library(RColorBrewer)
library(ggsignif)
library(dplyr)

getwd()
###core
pdf(file = "K2_core_boxplot_new22.pdf", width = 10, height= 5)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)
data = read.csv("K2_core22.csv",header=T)
data <- data %>% 
  gather(key = 'variable',value = 'counts')

ggplot(data,aes(variable,counts))+
  stat_boxplot(geom = 'errorbar',width=0.2,cex=0.5)+
  geom_boxplot(width=0.5,cex=0.5,fill='darkred',notch = T,notchwidth = 0.4,outlier.shape = NA)+ 
  labs(title = 'Boxplot')+
  theme_classic(base_size = 15)+
  theme(legend.position = 'right',
        panel.border = element_rect(fill = 'transparent',size = 1),
        plot.title = element_text(hjust = 0.5))+
  stat_summary(fun = "median", geom = "line", color = "black", aes(group = 1), size = 0.8)

dev.off()


###pan
getwd()
pdf(file = "K2_pan_boxplot_final.pdf", width = 10, height= 5)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)
data = read.csv("K2_pan22.csv",header=T)

data <- data %>% 
  gather(key = 'variable',value = 'counts')

ggplot(data,aes(variable,counts))+
  stat_boxplot(geom = 'errorbar',width=0.2,cex=0.5)+
  geom_boxplot(width=0.5,cex=0.5,fill='darkblue',notch = T,notchwidth = 0.4,outlier.shape = NA)+
  labs(title = 'Boxplot')+
  theme_classic(base_size = 15)+
  theme(legend.position = 'right',
        panel.border = element_rect(fill = 'transparent',size = 1),
        plot.title = element_text(hjust = 0.5))+
  stat_summary(fun = "median", geom = "line", color = "black", aes(group = 1), size = 0.8)
dev.off()


###################################################################################################
library(gcookbook)
library(ggplot2)

pdf(file = "K2_genes_of_accession_number_bar_chart.finally.pdf", width = 10.2, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

data = read.csv("K2_genes_of_accession_number_bar_chart.csv",header=T)

ggplot(data, aes(x = ID, y = Number)) +
  geom_col(fill='darkblue', width = 0.4) +
  
  theme_classic(base_size = 10) +
  theme(panel.grid=element_blank()) +
  scale_x_continuous(name = "Accession number", breaks = seq(1, 22, by = 1), labels = c("1", "2", "3", "4", "5","6", "7", "8", "9", "10","11", "12", "13", "14", "15", "16", "17", "18", "19","20", "21", "22"), limits = c(0.7, 22.3), expand = c(0,0)) +
  scale_y_continuous(name = "Gene number", breaks = seq(0, 20000, by = 5000), labels = c("0", "5K", "10K", "15K", "20K"), limits = c(0, 25000), expand = c(0,0))

dev.off()



###################################################################################################

library(ggplot2)
library(ggpubr)

pdf(file = "K2_gene_types_pie_chart_finally.pdf", width = 10.2, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

data<-data.frame(group=c("Core genes","Variable genes","Private genes"),value=c(22384,34093,11330))
colnames(data) <- c('group', 'value')
percentage <- scales::percent(data$value / sum(data$value)) 
ggpie(data, 'value',
      fill = 'group', palette = 'Dark2',
      label = percentage, lab.pos = 'in', color="white", lab.font = c(6, 'white')) + 
  theme(legend.position = "right",legend.text = element_text(size=15)) +
  labs(fill="Type")

dev.off()



###################################################################################################
library(ggplot2)
getwd()
pdf(file = "SV_number_types_K2.pdf", width = 10.2, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

data = read.csv("SV_number_types.csv",header=T)
data_new = data.frame(data)

ggplot(data_new,mapping = aes(ID,Value,fill=Type))+
  geom_bar(stat='identity',position='stack') +
  labs(x = 'ID',y = 'Frequnency') +
  theme_classic(base_size = 10) +
  theme(panel.grid=element_blank()) +
  theme(axis.title =element_text(size = 12),axis.text =element_text(size = 12, color = 'black'))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(expand = c(0,0))


dev.off()



###################################################################################################
library(ggplot2)
getwd()
pdf(file = "SV_number_types_A2.pdf", width = 10.2, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

data = read.csv("SV_number_types_A2.csv",header=T)
data_new = data.frame(data)

ggplot(data_new,mapping = aes(ID,Value,fill=Type))+
  geom_bar(stat='identity',position='stack') +
  labs(x = 'ID',y = 'Frequnency') +
  theme_classic(base_size = 10) +
  theme(panel.grid=element_blank()) +
  theme(axis.title =element_text(size = 12),axis.text =element_text(size = 12, color = 'black'))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(expand = c(0,0))


dev.off()





###################################################################################################
library(ggplot2)
getwd()
pdf(file = "SV_number_types_D5-502.pdf", width = 10.2, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

data = read.csv("SV_number_types_D5-502.csv",header=T)
data_new = data.frame(data)

ggplot(data_new,mapping = aes(ID,Value,fill=Type))+
  geom_bar(stat='identity',position='stack') +
  labs(x = 'ID',y = 'Frequnency') +
  theme_classic(base_size = 10) +
  theme(panel.grid=element_blank()) +
  theme(axis.title =element_text(size = 12),axis.text =element_text(size = 12, color = 'black'))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(expand = c(0,0))


dev.off()





###################################################################################################
library(ggplot2)

NAM_ID = c("D5-4", "D5-8", "D5-502", 
           "D1-35", "D1-5", "D2-1", "D2-2", "D3", "D4", "D6", "D7", "D8", "D9", "D10", "D11",
           "E1", "B1", "F1", "G2", "K2", "A2", "A1", "Kirkii")

NAM_colors = c("D5-4"="goldenrod1", "D5-8"="goldenrod1",
               "D5-502"="goldenrod1", "D1-35"="limegreen", 
               "D1-5"="limegreen", "D2-1"="limegreen", "D2-2"="limegreen", 
               "D3"="limegreen", "D4"="limegreen", "D6"="limegreen", 
               "D7"="limegreen", "D8"="limegreen", "D9"="limegreen", 
               "D10"="limegreen", "D11"="limegreen", "E1"="royalblue", 
               "B1"="royalblue", "F1"="royalblue", "G2"="royalblue", 
               "K2"="royalblue", 
               "A2"="orangered",  "A1"="orchid", "Kirkii"="gray47")

#reorder NAM_colors based on NAM_ID
NAM_colors = NAM_colors[order(match(names(NAM_colors), NAM_ID))]
TE_colors = c('gray', rev(brewer.pal(11,'RdYlBu')[1:5]), 
              brewer.pal(11,'RdYlBu')[6], 
              rev(brewer.pal(11,'RdBu')[7:10]))


pdf(file = "Te_number_types_new.pdf", width = 10.2, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

repeats = read.table("TE_number_types_new.txt",header=T)

repeats_bp = data.frame(repeats[,-1], Genome=repeats$Genome)
repeats_bp$Genome = factor(repeats_bp$Genome, levels=NAM_ID)

# gather plotting variables and change the variable plotting order
repeats_final = repeats_bp %>% 
  gather(variable, value, hAT, CACTA, PIF_Harbinger, Mutator, Tc1_Mariner,  
         helitron, nonLTR, Copia, Gypsy, LTR_unknown,Other) #remove nonTE
repeats_final$variable = as.factor(repeats_final$variable) #convert to character to factor
repeats_final$variable = factor(repeats_final$variable, 
                                levels = c("hAT", "CACTA", "PIF_Harbinger", "Mutator", 
                                           "Tc1_Mariner", "helitron", "nonLTR", 
                                           "LTR_unknown", "Gypsy", "Copia", "Other"))

# plot the total Mb of TEs
repeats_Mb_plot = repeats_final %>%
  ggplot(aes(x = Genome, y = value, fill = variable)) + 
  geom_bar(stat = "identity",colour="black") +
  scale_fill_manual(values=TE_colors) + #remove nonTE
  # Making two Y axes
  plt+scale_y_continuous("feet",sec.axis=sec_axis(~.*30.48,name="centi meter"))+

  labs(x =" ", y = "Repeat contents number",size=14, face="bold")+
  theme(axis.title.y = element_text(size=16, face="bold"),
        axis.text.y = element_text(size=11), 
        axis.text.x = element_text(color = NAM_colors, face="bold", size=11, angle=35, vjust=1, hjust=0.95),
        legend.text = element_text(size=14),
        legend.title=element_blank()) +
  theme(legend.position="top")

repeats_Mb_plot

dev.off()



####################################################################################################################################
library(ggplot2)

NAM_ID = c("D5-4", "D5-8", "D5-502", 
           "D1-35", "D1-5", "D2-1", "D2-2", "D3", "D4", "D6", "D7", "D8", "D9", "D10", "D11",
           "E1", "B1", "F1", "G2", "K2", "A2", "A1", "Kirkii")

NAM_colors = c("D5-4"="goldenrod1", "D5-8"="goldenrod1",
               "D5-502"="goldenrod1", "D1-35"="limegreen", 
               "D1-5"="limegreen", "D2-1"="limegreen", "D2-2"="limegreen", 
               "D3"="limegreen", "D4"="limegreen", "D6"="limegreen", 
               "D7"="limegreen", "D8"="limegreen", "D9"="limegreen", 
               "D10"="limegreen", "D11"="limegreen", "E1"="royalblue", 
               "B1"="royalblue", "F1"="royalblue", "G2"="royalblue", 
               "K2"="royalblue", 
               "A2"="orangered",  "A1"="orchid", "Kirkii"="gray47")

#reorder NAM_colors based on NAM_ID
NAM_colors = NAM_colors[order(match(names(NAM_colors), NAM_ID))]
TE_colors = c('gray', rev(brewer.pal(11,'RdYlBu')[1:5]), 
              brewer.pal(11,'RdYlBu')[6], 
              rev(brewer.pal(11,'RdBu')[7:10]))


pdf(file = "TE_types_size.pdf", width = 10.2, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

repeats = read.table("TE_types_size.txt",header=T)

repeats_bp = data.frame(repeats[,-1], Genome=repeats$Genome)
repeats_bp$Genome = factor(repeats_bp$Genome, levels=NAM_ID)

# gather plotting variables and change the variable plotting order
repeats_final = repeats_bp %>% 
  gather(variable, value, hAT, CACTA, PIF_Harbinger, Mutator, Tc1_Mariner,  
         helitron, nonLTR, Copia, Gypsy, LTR_unknown,Other) #remove nonTE
repeats_final$variable = as.factor(repeats_final$variable) #convert to character to factor
repeats_final$variable = factor(repeats_final$variable, 
                                levels = c("hAT", "CACTA", "PIF_Harbinger", "Mutator", 
                                           "Tc1_Mariner", "helitron", "nonLTR", 
                                           "LTR_unknown", "Gypsy", "Copia", "Other"))

# plot the total Mb of TEs
repeats_Mb_plot = repeats_final %>%
  ggplot(aes(x = Genome, y = value, fill = variable)) + 
  geom_bar(stat = "identity",colour="black") +
  scale_fill_manual(values=TE_colors) + #remove nonTE
  labs(x =" ", y = "Repeat contents number",size=14, face="bold")+
  theme(axis.title.y = element_text(size=16, face="bold"),
        axis.text.y = element_text(size=11), 
        axis.text.x = element_text(color = NAM_colors, face="bold", size=11, angle=35, vjust=1, hjust=0.95),
        legend.text = element_text(size=14),
        legend.title=element_blank()) +
  theme(legend.position="top")

size = 24.44
repeats_Mb_plot + scale_y_continuous("Repeat contents number (Mb)",sec.axis=sec_axis(~./24.44,name="Size to K2 (%)"))
dev.off()




####################################################################################################################################

library(ggplot2)

NAM_ID = c("D5-4", "D5-8", "D5-502", 
           "D1-35", "D1-5", "D2-1", "D2-2", "D3", "D4", "D6", "D7", "D8", "D9", "D10", "D11",
           "E1", "B1", "F1", "G2", "K2", "A2", "A1", "Kirkii")

NAM_colors = c("D5-4"="goldenrod1", "D5-8"="goldenrod1",
               "D5-502"="goldenrod1", "D1-35"="limegreen", 
               "D1-5"="limegreen", "D2-1"="limegreen", "D2-2"="limegreen", 
               "D3"="limegreen", "D4"="limegreen", "D6"="limegreen", 
               "D7"="limegreen", "D8"="limegreen", "D9"="limegreen", 
               "D10"="limegreen", "D11"="limegreen", "E1"="royalblue", 
               "B1"="royalblue", "F1"="royalblue", "G2"="royalblue", 
               "K2"="royalblue", 
               "A2"="orangered",  "A1"="orchid", "Kirkii"="gray47")

#reorder NAM_colors based on NAM_ID
NAM_colors = NAM_colors[order(match(names(NAM_colors), NAM_ID))]
TE_colors = c('gray', rev(brewer.pal(11,'RdYlBu')[1:5]), 
              brewer.pal(11,'RdYlBu')[6], 
              rev(brewer.pal(11,'RdBu')[7:10]))

pdf(file = "TE_types_size_new.pdf", width = 10.2, height= 4.8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

repeats = read.table("TE_types_size.txt",header=T)

repeats_bp = data.frame(repeats[,-1], Genome=repeats$Genome)
repeats_bp$Genome = factor(repeats_bp$Genome, levels=NAM_ID)

# gather plotting variables and change the variable plotting order
repeats_final = repeats_bp %>% 
  gather(variable, value, hAT, CACTA, PIF_Harbinger, Mutator, Tc1_Mariner,  
         helitron, nonLTR, Copia, Gypsy, LTR_unknown,Other) #remove nonTE
repeats_final$variable = as.factor(repeats_final$variable) #convert to character to factor
repeats_final$variable = factor(repeats_final$variable, 
                                levels = c("hAT", "CACTA", "PIF_Harbinger", "Mutator", 
                                           "Tc1_Mariner", "helitron", "nonLTR", 
                                           "LTR_unknown", "Gypsy", "Copia", "Other"))

# plot the total Mb of TEs
repeats_Mb_plot = repeats_final %>%
  ggplot(aes(x = Genome, y = value, fill = variable)) + 
  geom_bar(stat = "identity",colour="black") +
  scale_fill_manual(values=TE_colors) + #remove nonTE
  labs(x =" ", y = "Repeat contents number",size=14, face="bold")+
  theme(axis.title.y = element_text(size=16, face="bold"),
        axis.text.y = element_text(size=11), 
        axis.text.x = element_text(color = NAM_colors, face="bold", size=11, angle=35, vjust=1, hjust=0.95),
        legend.text = element_text(size=14),
        legend.title=element_blank()) +
  theme(legend.position="top")

size = 24.44
repeats_Mb_plot + scale_y_continuous("Repeat contents number (Mb)",sec.axis=sec_axis(~./24.44,name="Size to K2 (%)"))
dev.off()


###################################################################################################

library(dplyr)
library(reshape2)
library(ggplot2)
getwd()
pdf(file = "K12_PAV.heatmap5.pdf", width = 10.2, height= 8, family = "serif")
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("pan_genome_accession_PAV_column5.csv",
             row.names = 1)
dim(df)

df %>% 
  mutate(x=1:nrow(.)) %>% 
  select(1:22,x) %>% 
  reshape2::melt(,id.vars="x") %>% 
  mutate(pav=case_when(
    value == 0 ~ "Absence",
    TRUE ~ "Presence"
    
  )) -> dfa

dfa %>% count(pav)


ggplot(data=dfa,aes(x=x,y=variable))+
  geom_tile(aes(fill=pav))+
  scale_fill_manual(name="",
                    values=c("#316879","#ff9a8d")) +
  theme_classic() + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_text(angle = 0, 
                                 hjust = 1),
        axis.ticks.y=element_blank())  +
  theme(legend.position="bottom") +
  theme(text = element_text(size = 12)) 

dev.off()


###################################################################################################
#if (!requireNamespace("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
#BiocManager::install("clusterProfiler")
#BiocManager::install("org.Hs.eg.db")

library(dplyr)
library(reshape2)
library(ggplot2)
library(clusterProfiler)
library(tidyverse)
library(enrichplot)
library(Hmisc)


#####G2_GO
getwd()
pdf(file = "G2.GO.pdf", width = 10.2, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("G2.GO.csv", header = T)

#data<-as.matrix(df)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))
dev.off()


#####K2_Pathway
getwd()
pdf(file = "K2.Pathway.pdf", width = 10.2, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)
df<-read.csv("K2.pathway.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))
dev.off()




#####K2_GO
getwd()
pdf(file = "K2.GO.CC.pdf", width = 10.2, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("K2.GO.CC.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))
dev.off()


#####K2_GO
getwd()
pdf(file = "K2.GO.MF.pdf", width = 10.2, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("K2.GO.MF.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))
dev.off()



#####K2_GO
getwd()
pdf(file = "K2.GO.BP.pdf", width = 10.2, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("K2.GO.BP.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()



#####K2_GO
getwd()
pdf(file = "K2.GO.all.pdf", width = 20, height= 16)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("K2.GO.all.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()




#####Kirkii.GO
getwd()
pdf(file = "Kirkii.GO.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("Kirkii.GO.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()



#####F1.GO
getwd()
pdf(file = "F1.GO.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("F1.GO.csv", header = T)

#data<-as.matrix(df)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####E1.GO
getwd()
pdf(file = "E1.GO.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("E1.GO.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()




#####E1.pathway
getwd()
pdf(file = "E1.pathway.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("E1.pathway.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()



#####D11.pathway
getwd()
pdf(file = "D11.pathway.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D11.pathway.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####D10.pathway
getwd()
pdf(file = "D10.pathway.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D10.pathway.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####D10.GO
getwd()
pdf(file = "D10.GO.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D10.GO.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()





#####D8.GO.pathway
getwd()
pdf(file = "D8.GO.pathway.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D8.GO.pathway.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####D7.GO
getwd()
pdf(file = "D7.GO.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D7.GO.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()



#####D6.GO
getwd()
pdf(file = "D6.GO.pdf", width = 10, height= 8) 
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D6.GO.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()

#####D5-8.GO
getwd()
pdf(file = "D5-8.GO.pdf", width = 10, height= 8)字
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D5-8.GO.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()




#####D5-8.pathway
getwd()
pdf(file = "D5-8.pathway.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D5-8.pathway.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####D5-4.GO
getwd()
pdf(file = "D5-4.GO.pdf", width = 20, height= 16)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D5-4.GO.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####D5-4.pathway
getwd()
pdf(file = "D5-4.pathway.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D5-4.pathway.csv", header = T)

#data<-as.matrix(df)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####D5-502.GO.pathway
getwd()
pdf(file = "D5-502.GO.pathway.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D5-502.GO.pathway.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####D4.GO
getwd()
pdf(file = "D4.GO.pdf", width = 10, height= 8) 
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D4.GO.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####D3.GO
getwd()
pdf(file = "D3.GO.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D3.GO.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####D3.pathway
getwd()
pdf(file = "D3.pathway.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D3.pathway.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()



#####D2-2.GO.pathway
getwd()
pdf(file = "D2-2.GO.pathway.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D2-2.GO.pathway.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####D1-5.GO.pathway
getwd()
pdf(file = "D1-5.GO.pathway.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("D1-5.GO.pathway.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####B1.GO
getwd()
pdf(file = "B1.GO.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("B1.GO.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####B1.pathway
getwd()
pdf(file = "B1.pathway.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("B1.pathway.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####A2.GO
getwd()
pdf(file = "A2.GO.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("A2.GO.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####A2.pathway
getwd()
pdf(file = "A2.pathway.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("A2.pathway.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####A1.GO
getwd()
pdf(file = "A1.GO.pdf", width = 11, height= 8.8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("A1.GO.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


#####A1.pathway
getwd()
pdf(file = "A1.pathway.pdf", width = 10, height= 8)
par(mar = c(4.3,4.3,2.1,0.3),cex.lab=1.2,cex.sub=1.5)

df<-read.csv("A1.pathway.csv", header = T)
data <- data.frame(df)

ggplot(data,aes(Enrichratio,Description))+
  geom_point(aes(size=Count,color=p.adjust))+
  scale_colour_gradient(low="red",high="blue",
                        guide = guide_colorbar(reverse = TRUE))+
  labs(x="Enrich ratio")+
  #theme_classic() +
  theme_bw()+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 14))

dev.off()


