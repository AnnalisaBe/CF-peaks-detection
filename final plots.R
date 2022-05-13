
library(ggsignif)

df_valleys <- read.table('clipboard', sep = '\t', header=TRUE)

head(df_valleys)
tail(df_valleys)

#if you added a column with relative leaf number in excel
df_relative$relative_leaf <- factor(df_relative$relative_leaf)


df_valleys$relative.leaf <- factor(df_valleys$relative.leaf)

ggplot(df_valleys, aes(x=trt, y=abs_valleysint, color=trt)) + 
  geom_boxplot(lwd=0.8)+
  stat_summary(color=my_pal_gray[6], alpha=0.6, fun=mean, geom="point", shape=4, size=4, stroke=2)+
  geom_signif(comparisons =list(c('est', 'mock')),
              step_increase = 0.1, map_signif_level=c("***"=0.001, "**"=0.01, "*"=0.05), test = 'wilcox.test', color='black',size = 0.8, y_position = 30000, textsize = 5) #default is to wilcoxon test


#divided by relative leaf number

ggplot(df_valleys, aes(x=relative.leaf, y=abs_valleysint, fill=trt)) + 
  geom_boxplot(lwd=0.8, outlier.shape = NA)+
  coord_cartesian(ylim = c(0,5000))+
  #stat_summary(color=my_pal_gray[6], alpha=0.6, fun=mean, geom="point", shape=4, size=4, stroke=2)+
  geom_signif(comparisons =list(c('est', 'mock')),
              step_increase = 0.1, map_signif_level=c("***"=0.001, "**"=0.01, "*"=0.05), test = 'wilcox.test', color='black',size = 0.8, y_position = 30000, textsize = 5) #default is to wilcoxon test


stat_valleys<-compare_means(Relative.Normalised.quantity..NRQ. ~ genotrt, 
                           data=df, 
                           method = 't.test', 
                           p.adjust.method = "bonferroni")
stat_valleys


df_peaks <- read.table('clipboard', sep = '\t', header=TRUE)

head(df_peaks)
tail(df_peaks)


df_peaks$relative_leaf <- factor(df_peaks$relative_leaf)


ggplot(df_peaks, aes(x=trt, y=rel_peaksint, color=trt)) + 
  geom_boxplot(lwd=0.8)+
  geom_violin()+
  stat_summary(color=my_pal_gray[6], alpha=0.6, fun=mean, geom="point", shape=4, size=4, stroke=2)+
  geom_signif(comparisons =list(c('est', 'mock')),
              step_increase = 0.1, map_signif_level=c("***"=0.001, "**"=0.01, "*"=0.05), test = 'wilcox.test', color='black',size = 0.8, y_position = 35000, textsize = 5) #default is to wilcoxon test



ggplot(df_peaks, aes(x=trt, y=abs_peaksint, color=trt)) + 
  geom_boxplot(lwd=0.8, outlier.shape = NA)+
  #geom_violin()+
  stat_summary(color=my_pal_gray[6], alpha=0.6, fun=mean, geom="point", shape=4, size=4, stroke=2)+
  geom_signif(comparisons =list(c('est', 'mock')),
              step_increase = 0.1, map_signif_level=c("***"=0.001, "**"=0.01, "*"=0.05), test = 'wilcox.test', color='black',size = 0.8, y_position = 8000, textsize = 5) #default is to wilcoxon test


ggplot(df_peaks, aes(x=trt, y=peak_W_v, fill=trt)) +
  geom_boxplot(lwd=0.8)+
  geom_signif(comparisons =list(c('est', 'mock')),
              step_increase = 0.1, map_signif_level=c("***"=0.001, "**"=0.01, "*"=0.05), test = 'wilcox.test', color='black',size = 0.8, y_position = 1500, textsize = 5) #default is to wilcoxon test





#divided by realtive leaf number

ggplot(df_peaks, aes(x=relative.leaf, y=peak_W_v, fill=trt)) +
  geom_boxplot(lwd=0.8, outlier.shape = NA)+
  coord_cartesian(ylim = c(0,500))+
  geom_signif(comparisons =list(c('est', 'mock')),
              step_increase = 0.1, map_signif_level=c("***"=0.001, "**"=0.01, "*"=0.05), test = 'wilcox.test', color='black',size = 0.8, y_position = 35000, textsize = 5) #default is to wilcoxon test

ggplot(df_peaks, aes(x=relative.leaf, y=abs_peaksint, fill=trt)) +
  geom_boxplot(lwd=0.8, outlier.shape = NA)+
  coord_cartesian(ylim = c(0,10000))+
  geom_signif(comparisons =list(c('est', 'mock')),
              step_increase = 0.1, map_signif_level=c("***"=0.001, "**"=0.01, "*"=0.05), test = 'wilcox.test', color='black',size = 0.8, y_position = 35000, textsize = 5) #default is to wilcoxon test

#width of the peaks
ggplot(df_peaks, aes(x=relative.leaf, y=peak_W_v, fill=trt)) +
  geom_boxplot(lwd=0.8)+
  coord_cartesian(ylim = c(0,50))+
  geom_signif(comparisons =list(c('est', 'mock')),
              step_increase = 0.1, map_signif_level=c("***"=0.001, "**"=0.01, "*"=0.05), test = 'wilcox.test', color='black',size = 0.8, y_position = 35000, textsize = 5) #default is to wilcoxon test



#summarize valleys vs peaks per sample

valley_sample <- data.frame(df_valleys %>% group_by(trt, sample, relative_leaf) %>% summarise(mean_valley=mean(abs_valleysint)))
head(valley_sample)
tail(valley_sample)

peaks_sample <- data.frame(df_peaks %>% group_by(trt, sample, relative_leaf) %>% 
                             summarise(mean_rel=mean(rel_peaksint), 
                                       mean_abs=mean(abs_peaksint), 
                                       mean_width=mean(peak_W_v), 
                                       mean_slope=mean(slope_v)))
head(peaks_sample)
tail(peaks_sample)

mean_valley <- valley_sample$mean_valley

mean_sample <- cbind(peaks_sample, mean_valley)
head(mean_sample)
tail(mean_sample)


plot <- ggplot(mean_sample, mapping=aes(x=mean_abs, y=mean_valley, fill=trt))+
  geom_point(aes(fill=trt, color=trt, shape=relative_leaf), size=4, alpha=0.7)+
  geom_smooth(method=lm, fullrange=TRUE, alpha=0.2, aes(color=trt))


plot0 <- ggplot(mean_sample, mapping=aes(x=mean_abs, y=mean_valley, fill=trt))+
  geom_point(aes(fill=trt, color=trt, shape=relative_leaf), size=4, alpha=0.7)+
  xlim(0,2000)+
  ylim(0,2000)+
  geom_smooth(method=lm, fullrange=TRUE, alpha=0.2, aes(color=trt))

plot1 <- ggplot(mean_sample, mapping=aes(x=mean_rel, y=mean_width, fill=trt))+
  geom_point(aes(fill=trt, color=trt, shape=relative_leaf), size=4, alpha=0.7)

plot2 <- ggplot(mean_sample, mapping=aes(x=mean_abs, y=mean_slope, fill=trt))+
  geom_point(aes(fill=trt, color=trt, shape=relative_leaf), size=4, alpha=0.7)+
  geom_smooth(method=lm, fullrange=TRUE, alpha=0.2, aes(color=trt))

plot3 <- ggplot(mean_sample, mapping=aes(x=mean_slope, y=mean_width, fill=trt))+
  geom_point(aes(fill=trt, color=trt, shape=relative_leaf), size=4, alpha=0.7)

ggarrange(plot,
          #plot0, 
          plot1,
          plot2,
          plot3,
          labels = c("A", "B", "C", 'D', 'E'),
          ncol = 2, nrow = 3)





#boxplots per sample mean width
plot_mean_width <- ggplot(mean_sample, aes(x=relative_leaf, y=mean_width, color=trt)) +
  geom_boxplot(lwd=0.8)+
  theme_bw(base_size=12)+
  scale_fill_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  scale_color_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  geom_point(position = position_dodge(0.75), aes(fill=trt, color=trt), size=4, alpha=0.5)

#boxplots per sample abs height
plot_abs_int <- ggplot(mean_sample, aes(x=relative_leaf, y=mean_abs, color=trt)) +
  geom_boxplot(lwd=0.8)+
  theme_bw(base_size=12)+
  scale_fill_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  scale_color_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  geom_point(position = position_dodge(0.75), aes(fill=trt, color=trt), size=4, alpha=0.5)

#boxplots per sample relative height
plot_rel_int <- ggplot(mean_sample, aes(x=relative_leaf, y=mean_rel, color=trt)) +
  geom_boxplot(lwd=0.8)+
  theme_bw(base_size=12)+
  scale_fill_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  scale_color_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  geom_point(position = position_dodge(0.75), aes(fill=trt, color=trt), size=4, alpha=0.5)

#boxplots per sample relative height/mean width
plot_HrelW_ratio <- ggplot(mean_sample, aes(x=relative_leaf, y=mean_rel/mean_width, color=trt)) +
  geom_boxplot(lwd=0.8)+
  theme_bw(base_size=12)+
  scale_fill_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  scale_color_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  geom_point(position = position_dodge(0.75), aes(fill=trt, color=trt), size=4, alpha=0.5)

plot_HabsW_ratio <- ggplot(mean_sample, aes(x=relative_leaf, y=mean_abs/mean_width, color=trt)) +
  geom_boxplot(lwd=0.8)+
  theme_bw(base_size=12)+
  scale_fill_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  scale_color_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  geom_point(position = position_dodge(0.75), aes(fill=trt, color=trt), size=4, alpha=0.5)


plot_slope <- ggplot(mean_sample, aes(x=relative_leaf, y=mean_slope, color=trt)) +
  geom_boxplot(lwd=0.8)+
  theme_bw(base_size=12)+
  scale_fill_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  scale_color_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  geom_point(position = position_dodge(0.75), aes(fill=trt, color=trt), size=4, alpha=0.5)


plot_valleys <- ggplot(mean_sample, aes(x=relative_leaf, y=mean_valley, color=trt)) +
  geom_boxplot(lwd=0.8)+
  theme_bw(base_size=12)+
  scale_fill_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  scale_color_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  geom_point(position = position_dodge(0.75), aes(fill=trt, color=trt), size=4, alpha=0.5)


ggarrange(plot_mean_width,
          plot_abs_int, 
          plot_rel_int,
          plot_HrelW_ratio,
          plot_HabsW_ratio,
          plot_slope,
          plot_valleys,
          labels = c("A", "B", "C", 'D', 'E'),
          ncol = 4, nrow = 2)




#subset df for relative leaf 3 only
df_subset<- mean_sample[mean_sample$relative.leaf==2,]



#boxplots per sample mean width
plot_mean_width <- ggplot(df_subset, aes(x=relative.leaf, y=mean_width, color=trt)) +
  geom_boxplot(lwd=0.8)+
  theme_bw(base_size=20)+
  scale_fill_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  scale_color_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  geom_point(position = position_dodge(0.75), aes(fill=trt, color=trt), size=4, alpha=0.5)

#boxplots per sample abs height
plot_abs_int <- ggplot(df_subset, aes(x=relative.leaf, y=mean_abs, color=trt)) +
  geom_boxplot(lwd=0.8)+
  theme_bw(base_size=20)+
  scale_fill_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  scale_color_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  geom_point(position = position_dodge(0.75), aes(fill=trt, color=trt), size=4, alpha=0.5)

#boxplots per sample relative height
plot_rel_int <- ggplot(df_subset, aes(x=relative.leaf, y=mean_rel, color=trt)) +
  geom_boxplot(lwd=0.8)+
  theme_bw(base_size=20)+
  scale_fill_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  scale_color_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  geom_point(position = position_dodge(0.75), aes(fill=trt, color=trt), size=4, alpha=0.5)

#boxplots per sample relative height/mean width
plot_HrelW_ratio <- ggplot(df_subset, aes(x=relative.leaf, y=mean_rel/mean_width, color=trt)) +
  geom_boxplot(lwd=0.8)+
  theme_bw(base_size=20)+
  scale_fill_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  scale_color_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  geom_point(position = position_dodge(0.75), aes(fill=trt, color=trt), size=4, alpha=0.5)

plot_HabsW_ratio <- ggplot(df_subset, aes(x=relative.leaf, y=mean_abs/mean_width, color=trt)) +
  geom_boxplot(lwd=0.8)+
  theme_bw(base_size=20)+
  scale_fill_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  scale_color_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  geom_point(position = position_dodge(0.75), aes(fill=trt, color=trt), size=4, alpha=0.5)

ggarrange(plot_mean_width,
          plot_abs_int, 
          plot_rel_int,
          plot_HrelW_ratio,
          plot_HabsW_ratio,
          labels = c("A", "B", "C", 'D', 'E'),
          ncol = 2, nrow = 3)





ggplot(df_subset, mapping=aes(x=mean_abs, y=mean_valley, fill=trt))+
  geom_point(aes(fill=trt, color=trt, shape=relative.leaf), size=4, alpha=0.7)+
  geom_smooth(method=lm, fullrange=TRUE, alpha=0.2, aes(color=trt))


ggplot(df_subset, mapping=aes(x=mean_rel, y=mean_valley, fill=trt))+
  geom_point(aes(fill=trt, color=trt, shape=relative.leaf), size=4, alpha=0.7)+
  geom_smooth(method=lm, fullrange=TRUE, alpha=0.2, aes(color=trt))

ggplot(df_subset, mapping=aes(x=mean_rel, y=mean_width, fill=trt))+
  geom_point(aes(fill=trt, color=trt, shape=relative.leaf), size=4, alpha=0.7)


ggplot(df_subset, mapping=aes(x=mean_abs, y=mean_slope, fill=trt))+
  geom_point(aes(fill=trt, color=trt, shape=relative.leaf), size=4, alpha=0.7)+
  geom_smooth(method=lm, fullrange=TRUE, alpha=0.2, aes(color=trt))





#summarize valleys vs peaks per leaf
#sheet4 - detail of valleys vs peaks per leaf
valley_sample <- data.frame(df_valleys %>% group_by(trt, sample, relative_leaf) %>% summarise(mean_valley=mean(abs_valleysint)))
head(valley_sample)
tail(valley_sample)

peaks_sample <- data.frame(df_peaks %>% group_by(trt, leaf) %>% summarise(mean_rel=mean(rel_peaksint), mean_abs=mean(abs_peaksint), mean_width=mean(peak_W_v)))
head(peaks_sample)
tail(peaks_sample)

mean_valley <- valley_sample$mean_valley

mean_sample <- cbind(peaks_sample, mean_valley)
head(mean_sample)
tail(mean_sample)





#workbook for the summarized data
workbook_plots <- createWorkbook(paste(EXP, "summarised data.xlsx"))

#sheet4 - detail of valleys vs peaks per sample
addWorksheet(workbook_plots, 'valleys vs peaks per sample')
writeData(workbook_plots, 'valleys vs peaks per sample', mean_sample)
saveWorkbook(workbook_plots, file=paste(EXP, "summarised data.xlsx"), overwrite = TRUE)

addWorksheet(workbook_plots, 'peaks')
writeData(workbook_plots, 'peaks', df_peaks)
saveWorkbook(workbook_plots, file=paste(EXP, "summarised data.xlsx"), overwrite = TRUE)

addWorksheet(workbook_plots, 'valleys')
writeData(workbook_plots, 'valleys', df_valleys)
saveWorkbook(workbook_plots, file=paste(EXP, "summarised data.xlsx"), overwrite = TRUE)



#pulling experiments
setwd("//nbi-cfs2/shared/Research-Groups/Christine-Faulkner/Annalisa/dye loading/dye loading emma style/exp2 and 3 pulling")

EXP<- 'pulling 2_3'

df_peaks <- read.table('clipboard', sep = '\t', header=TRUE)

head(df_peaks)
tail(df_peaks)


#if you added a column with relative leaf number in excel
df_peaks$exp <- factor(df_peaks$exp)


df_peaks$relative_leaf <- factor(df_peaks$relative_leaf)



plot_mean_width_all <- ggplot(df_peaks, aes(x=relative_leaf, y=mean_width, color=trt)) +
  geom_boxplot(lwd=0.8)+
  theme_bw(base_size=12)+
  scale_fill_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  scale_color_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  geom_point(position = position_dodge(0.75), aes(fill=trt, color=trt, shape=exp), size=4, alpha=0.5)


#saves PNG version for easy acces
ggsave(paste(EXP, "boxplot_width_all", ".png"), plot_mean_width_all, width = 8, height = 8, dpi = 450, device = 'png')

#saves svg version for easy editing, this is correctly visualized only in illustrator while in inkscape looses the 
#strokes and looses size of the legend. To insert in thesis, open in illustrator, save and save as pdf
svglite(file=paste(EXP, "boxplot_width_all", ".svg"),width=8,height=8)
figure <- plot_mean_width_all
print(figure)
dev.off()

#saves pdf version for easy insertion in thesis. Problem with this pdf is that the data point are rendered as squares in 
#illustrator, however this renders correctly in inkscape
ggsave(paste(EXP,"boxplot_width_all", ".pdf"), plot_mean_width_all, width = 8, height = 8, dpi = 450)







#subset df for relative leaf 3 only
df_subset<- df_peaks[df_peaks$relative_leaf==2,]


df_peaks$trt <- factor(df_peaks$trt)

#boxplots per sample mean width
plot_mean_width_leaf2 <- ggplot(df_subset, aes(x=trt, y=mean_width, color=trt)) +
  geom_boxplot(lwd=2, fatten=2)+
  theme_bw(base_size=28)+
  scale_fill_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  scale_color_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  geom_point(position = position_dodge(0.75), aes(fill=trt, shape=exp), size=7, alpha=0.5)+
  ylim(190,320)+
  stat_summary(color=my_pal_gray[6], alpha=0.6, fun=mean, geom="point", shape=4, size=4, stroke=3)



compare_means(mean_width ~ trt, 
              data=df_subset, 
              method = 'wilcox')



#saves PNG version for easy acces
ggsave(paste(EXP, "plot_mean_width_leaf2", ".png"), plot_mean_width_leaf2, width = 8, height = 8, dpi = 450, device = 'png')

#saves svg version for easy editing, this is correctly visualized only in illustrator while in inkscape looses the 
#strokes and looses size of the legend. To insert in thesis, open in illustrator, save and save as pdf
svglite(file=paste(EXP, "plot_mean_width_leaf2", ".svg"),width=8,height=8)
figure <- plot_mean_width_leaf2
print(figure)
dev.off()

#saves pdf version for easy insertion in thesis. Problem with this pdf is that the data point are rendered as squares in 
#illustrator, however this renders correctly in inkscape
ggsave(paste(EXP,"plot_mean_width_leaf2", ".pdf"), plot_mean_width_leaf2, width = 8, height = 8, dpi = 450)





# plot profile for supp figure

pdf("profile line 32 2R leaf 10 left.pdf",         # File name
    width = 8, height = 7, # Width and height in inches
    bg = "white",          # Background color
    paper = "A4")          # Paper size

# Creating a plot
plot(x,y, col=my_pal_gray[5])
lines(x, y_smooth, col='blue')
abline(v=x[line_peaks_recap$peak_ind], col=okabe_pal[6])
abline(v=line_peaks_recap$left_foot, col=my_pal_div[8], lty=2)
abline(v=line_peaks_recap$right_foot, col=my_pal_div[8], lty=2)

# Closing the graphical device
dev.off()
