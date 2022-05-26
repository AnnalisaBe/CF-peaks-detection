#packages needed
library(ggsignif)


#From the output of the R script grid_peak_valleys_analysis 
#copy valleys df from excel file
df_valleys <- read.table('clipboard', sep = '\t', header=TRUE)

head(df_valleys)
tail(df_valleys)

df_valleys$relative_leaf <- factor(df_valleys$relative_leaf)


#copy peaks df from excel file
df_peaks <- read.table('clipboard', sep = '\t', header=TRUE)

head(df_peaks)
tail(df_peaks)

df_peaks$relative_leaf <- factor(df_peaks$relative_leaf)


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


#subset df for relative leaf 2 only
df_subset<- mean_sample[mean_sample$relative.leaf==2,]

#boxplots per sample mean width
plot_mean_width <- ggplot(df_subset, aes(x=relative.leaf, y=mean_width, color=trt)) +
  geom_boxplot(lwd=0.8)+
  theme_bw(base_size=20)+
  scale_fill_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  scale_color_manual(values=c(okabe_pal[2], okabe_pal[1])) +
  geom_point(position = position_dodge(0.75), aes(fill=trt, color=trt), size=4, alpha=0.5)




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
#make excel file with all experiments data, then copy the data
setwd(".....")

EXP<- '...'

df_peaks <- read.table('clipboard', sep = '\t', header=TRUE)

head(df_peaks)
tail(df_peaks)

df_peaks$exp <- factor(df_peaks$exp)

df_peaks$relative_leaf <- factor(df_peaks$relative_leaf)

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

