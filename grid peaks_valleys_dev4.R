#------------------------------------------------------------------------------------------------------------------------
#--------------------------------------- Detection of peaks and valleys along intensity profiles  -----------------------
#------------------------------------------------------------------------------------------------------------------------

#detection of peaks and valleys along profiles

#Date: 29/04/2022
#Author: Annalisa Bellandi, Faulkner group

#----------------------------------------------------------------------------------------------------------------------

#packages needed
library('rlang')
library("purrr")
library("tidyr")
library("dplyr")
library("openxlsx")
library("ggplot2")
library("reshape2")
library("zoo")
library("RColorBrewer")
library("svglite")

#---------------------------------------------------------------------------------------------------------------------
# set colour palettes
my_pal_div <- RColorBrewer::brewer.pal(11, "BrBG")[2:11]
my_pal_quant_1 <- RColorBrewer::brewer.pal(9, "Oranges")
my_pal_quant_2 <- RColorBrewer::brewer.pal(9, "Blues")
my_pal_gray <- RColorBrewer::brewer.pal(9, "Greys")
okabe_pal <- c("#E69F00","#56B4E9","#009E73", "#F0E442", "#0072B2", "#D55E00","#CC79A7")

n <- max(length(my_pal_div), length(my_pal_quant_1), length(my_pal_quant_2), length(my_pal_gray), length(okabe_pal))

length(my_pal_div) <- n
length(my_pal_quant_1) <- n
length(my_pal_quant_2) <- n
length(my_pal_gray) <- n
length(okabe_pal) <- n

my_pal_gray_d <- data.frame(my_pal_gray)
my_pal_quant_1_d <- data.frame(my_pal_quant_1)
my_pal_quant_2_d <- data.frame(my_pal_quant_2)
my_pal_div_d <- data.frame(my_pal_div)
okabe_pal_d <- data.frame(okabe_pal)

my_col_set <- (0)
my_col_set <- cbind(my_pal_gray_d, my_pal_quant_1_d)
my_col_set <- cbind(my_col_set, my_pal_quant_2_d)
my_col_set <- cbind(my_col_set, okabe_pal_d)
my_col_set <- cbind(my_col_set, my_pal_div_d)

my_col_set_df <- data.frame(my_col_set)

order <- c(1:10)
my_col_set_df1 <- cbind(my_col_set_df, order)
my_col_set_df1

long_color <- melt(my_col_set_df1,
                   id.vars = "order",
                   variable.name = "palette",
                   value.name = "color")

my_colors_plot <- ggplot(long_color, aes(x = palette, y = order, fill = color)) +
  geom_tile(aes(width=0.93, height=0.95)) +
  scale_fill_identity() +
  scale_y_continuous(breaks=c(1:n)) +
  theme_light()+
  geom_label(aes(label=color),colour = "black", fill= "white", fontface = "bold", size = 4)

my_colors_plot


##-------------------------------------------------------------------------------------------------------------------------
##------------------------------------------ defining FUNCTIONS --------------------------------------------------------------
##------------------------------------------------------------------------------------------------------------------------------

#Function 1

find_peaks_and_Valleys <- function(x, y, w, span, treshold) {
  
  require(zoo)
  n <- length(y)
  
  #fits a loess curve on the data using the given span
  y_smooth <- loess(y ~ x, span=span)$fitted
  
  #finds all local maxima and minima along the loess curve, this uses a rolling window approach. In practice data are divided in windows of width w, within each window 
  #the max and min are calculated. Then the max and min values are allocated to the position corresponding to the centre of the window.
  y_max <- rollapply(zoo(y_smooth), 2*w+1, max, align="center")
  y_min <- rollapply(zoo(y_smooth), 2*w+1, min, align="center")
  
  #now we want to compare the y_max values (the max of each window) and the y_smooth values so that to determine the exact position of the
  #maxima, but to do so we need to remember that y_max indexes starts 
  #form the centre of the first window, so first we need to centre y_max on y_smooth and trim y_smooth to exclude all the 
  #values before the centre of the first window and after the centre of the last window.
  y_smooth_trimmed <- y_smooth[-c(1:w, n+1-1:w)]
  
  #y_smooth_trimmed now starts with the value in position w+1, which is the centre of the first window used in the rollapply command. 
  #now we can compare the two dataset - a max listed in the y_max it is a real max only if in that pont the loess function is actually equal
  #or bigger than the value of the max.This comparison allow us to get out 
  #only the real local maxima (see graphic rapresentation to understand it)
  delta <- y_max - y_smooth_trimmed
  
  #the indexes of our max values now refer to the indexes of the y_max, but we want to bring them back to the indexes of our original 
  #dataset (y_smooth) so that we can fish out the correct distance from the centre that corresponds to each max, so we add w. 
  index_max_all <- which(delta <= 0) + w
  index_max_all
  
  #and we do the same with the minima
  delta_min <- y_min - y_smooth_trimmed
  index_min_all <- which(delta_min >= 0) + w
  index_min_all
  
  #creates a df with the values of the fitted loess in one column and the indexing in the other
  intensity <- y_smooth_trimmed
  index_num <- index(y_max)
  df_y_smooth_trimmed <- data.frame(cbind(index_num, intensity))
  
  #subsets the df to extract the rows containing the maxima and the ones containing the minima
  max_values_all <- subset(df_y_smooth_trimmed, index_num %in% index_max_all)
  min_values_all <- subset(df_y_smooth_trimmed, index_num %in% index_min_all)
  max_values_all
  min_values_all
  
  #now some peaks are detected even if they could be considered just noise
  #peaks that are actually noise could be having an intensity that is not high enough compared to the valleys. How high is high enouhg? Maybe the average SD of the real dataset from the ysmooth
  avgres <- sum(abs(y-y_smooth))/length(y) 
  SDsample <- sqrt(sum((abs(y-y_smooth))^2)/length(y)) #Root mean square error (RMSE, a measure of average residuals between data and fitted loess, so a measure of how spread data are)
  
  p <- (max_values_all$index_num)
  
  line_peaks_recap <- data.frame(peak_ind=numeric(), 
                                 abs_int=numeric(), 
                                 rel_int=numeric(), 
                                 left_foot=numeric(), 
                                 right_foot=numeric(),
                                 slope_c=numeric(),
                                 slope_v=numeric(),
                                 width_c=numeric(),
                                 width_v=numeric()
  )
  
  line_valleys_recap <- data.frame(index_num=numeric(),
                                   intensity=numeric())
  
  #now for every peak in the grid line, work to find 
  #the relative intensity
  #the slope and width
  #if relative intensity is less than 1.96*SDsample then it is not a peak
  
  for (i in p) { #for each peak, based on the index, find the nearest valleys
    
    peak <- max_values_all[max_values_all$index_num==i,] #select the peak
    
    
    #focus on the right valley
    right_valley <- min_values_all[min(which(min_values_all$index_num > i)),]
    right_valley
    
    
    if (is.na(right_valley$index_num)==FALSE) {#if there is a valley at the right of the peak, so the peak is not at the very end of the plot
      
      #then we cna proceed to find:
      #-width
      #-slope
      #on the right side of the peak
      
      peak_wc_right <- x[right_valley$index_num] - x[i]
      peak_wc_right #peak width to the centre of the valley
      
      #now find peak slope to the centre of the valley
      delta_y <- peak$intensity-right_valley$intensity
      peak_slopec_right <- delta_y/peak_wc_right #slope from centre of peak to centre of valley
      
      #now find valley width to search peak width to the side of the valley
      #points are part of the valley if the values are <= of valley in +avgres
      right_valley_treshold <- right_valley$intensity + SDsample #this is the max value over which you are not part of that valley anymore
      
      #now subset y_smooth to find the point between the peak and the centre of the valley where the values are higher, so find the foot of the peak
      y_smooth_interval <- y_smooth[peak$index_num:right_valley$index_num] 
      peak_right_foot <- x[min(which(y_smooth_interval <= right_valley_treshold)) + peak$index_num]
      
      if ((peak_right_foot < x[peak$index_num]+50)==TRUE) {
        peak_right_foot <- x[right_valley$index_num]
      }
      
      peak_wv_right <- peak_right_foot - x[i]
      
      #then can find slope of peak to the border of valley
      peak_slopev_right <- delta_y/peak_wv_right #slope from the centre of the peak to the start of the valley
      
      
      
    }
    
    
    left_valley <- min_values_all[max(which(min_values_all$index_num < i)),]
    left_valley
    
    if (is.na(left_valley$index_num)==FALSE) {#if there is a valley at the left side of the peak, so the peak is not at the very start of the plot
      
      #then we cna proceed to find:
      #-width
      #-slope
      #on the left side of the peak
      
      peak_wc_left <- x[i]-x[left_valley$index_num] #width of the peak to the centre of the valley
      peak_wc_left
      #add slope left here
      
      #now find peak slope to the centre of the valley
      delta_y <- peak$intensity-left_valley$intensity
      peak_slopec_left <- delta_y/peak_wc_left #slope from centre of peak to centre of valley
      
      #now find valley width to search peak width to the side of the valley
      #points are part of the valley if the values are <= of valley in +avgres
      left_valley_treshold <- left_valley$intensity + SDsample #this is the max value over which you are not part of that valley anymore
      
      #now subset y_smooth to find the point between the peak and the centre of the valley where the values are higher, so find the foot of the peak
      y_smooth_interval <- y_smooth[left_valley$index_num:peak$index_num] 
      peak_left_foot <- x[max(which(y_smooth_interval <= left_valley_treshold)) + left_valley$index_num]
      
      if (peak_left_foot>x[peak$index_num]-50) {
        peak_left_foot <- x[left_valley$index_num]
      }
      
      peak_wv_left <- x[i]-peak_left_foot
      
      #then can find slope of peak to the border of valley
      peak_slopev_left <- delta_y/peak_wv_left #slope from the centre of the peak to the start of the valley
      
      
    }
    
    
    #summary for the peak if both valleys exist....
    
    if (is.na(left_valley$index_num)==FALSE && is.na(right_valley$index_num)==FALSE){
      #if both valleys exist
      valleys_int <- (right_valley$intensity + left_valley$intensity)/2
      rel_peakint <- peak$intensity - valleys_int
     
      peak_wc_total <- peak_wc_left + peak_wc_right
      peak_Wv_total <- peak_wv_left + peak_wv_right
      #avg slope of peak
      peak_slopec_total <- (peak_slopec_left + peak_slopec_right)/2
      peak_slopev_total <- (peak_slopev_left + peak_slopev_right)/2
      
    }
    
    #if one of the valley is missing...
    
    if (is.na(left_valley$index_num)) { #if there is no previous valley, such as when the peak is at the beginning of the plot
      peak_wc_total <- 2*peak_wc_right
      peak_Wv_total <- 2*peak_wv_right
      peak_slopec_total <- peak_slopec_right
      peak_slopev_total <- peak_slopev_right
      valleys_int <- right_valley$intensity
      rel_peakint <- peak$intensity - right_valley$intensity
      peak_left_foot <- x[i] - peak_wv_right
      
      if (peak_left_foot<0){
        peak_left_foot <- 0
      }
      
    }
    
    
    if (is.na(right_valley$index_num)) { #if there is no next valley, such as when the peak is at the beginning of the plot
      peak_wc_total <- 2*peak_wc_left
      peak_Wv_total <- 2*peak_wv_left
      peak_slopec_total <- peak_slopec_left
      peak_slopev_total <- peak_slopev_left
      valleys_int <- left_valley$intensity
      rel_peakint <- peak$intensity - left_valley$intensity
      peak_right_foot <- x[i] + peak_wv_left
      
      if (peak_left_foot > max(x)){
        peak_left_foot <- max(x)
      }
      
    }
    
    
    #a peak is a peak only if the reletive intensity is more than the average residual over the intenisty of the valleys near it
    if (rel_peakint > treshold*avgres){
      
      peak_p <- data.frame(peak_ind=peak$index_num, 
                           abs_int=peak$intensity, 
                           rel_int=rel_peakint, 
                           left_foot=peak_left_foot, 
                           right_foot=peak_right_foot,
                           slope_c=peak_slopec_total,
                           slope_v=peak_slopev_total,
                           width_c=peak_wc_total,
                           width_v=peak_Wv_total)
      
      line_peaks_recap <- rbind(line_peaks_recap, peak_p)
      
      
      #if a peak is apeak then we also save its valleys
      
      if (is.na(left_valley$index_num)==FALSE && is.na(right_valley$index_num)==FALSE){
        #if both valleys exist
        line_valleys_recap <- rbind(line_valleys_recap, left_valley)
        line_valleys_recap <- rbind(line_valleys_recap, right_valley)
        
      }
      
      if (is.na(left_valley$index_num)) { #if there is no previous valley, such as when the peak is at the beginning of the plot                   
        line_valleys_recap <- rbind(line_valleys_recap, right_valley)
      }
      
      if (is.na(right_valley$index_num)) { #if there is no previous valley, such as when the peak is at the beginning of the plot                   
        line_valleys_recap <- rbind(line_valleys_recap, left_valley)
      }
      
    }#this closes saving the peak only if high enough
    
  } #this closes loop along the peaks of this grid line
  
  line_peaks_recap
  line_valleys_recap
  
  plot(x,y, col=my_pal_gray[5])
  lines(x, y_smooth, col='blue')
  abline(v=x[line_peaks_recap$peak_ind], col=okabe_pal[6])
  #abline(v=x[line_valleys_recap$index_num], col='blue')
  abline(v=line_peaks_recap$left_foot, col=my_pal_div[8], lty=2)
  abline(v=line_peaks_recap$right_foot, col=my_pal_div[8], lty=2)
  
  

  
  #calculate peak mean slope along this grid line
  mean_v_slope <- mean(line_peaks_recap$slope_v)
  mean_c_slope <- mean(line_peaks_recap$slope_c)
  
  
  list(y_smooth= y_smooth, #fitted loess on this line profile
       x=x, #distance
       peaks_index = x[line_peaks_recap$peak_ind],
       abs_peaksint=line_peaks_recap$abs_int, #absolute intensity of each peak
       abs_valleysint=line_valleys_recap$intensity, #absolute values of valleys int
       rel_peaksint=line_peaks_recap$rel_int, #relative intensity of each peak
       mean_abs_peaksint= mean(line_peaks_recap$abs_int), #mean absolute intensity of the peaks
       mean_abs_valleyint = mean(line_valleys_recap$intensity), #mean absolute intensity of the valleys
       mean_rel_peakint = mean(line_peaks_recap$rel_int), #relative intensity of the peak above its neighboring valleys
       peak_L_feet = line_peaks_recap$left_foot,#position of feet
       peak_R_feet = line_peaks_recap$right_foot,#position of feet
       peak_w_c = line_peaks_recap$width_c, #width of each peak to centre of valley
       peak_W_v = line_peaks_recap$width_v,#width of each peak to edge of valley
       mean_peak_w_c = mean(line_peaks_recap$width_c), #mean width of each peak to centre of valley
       mean_peak_W_v = mean(line_peaks_recap$width_v),#mean width of each peak to edge of valley
       slope_v = line_peaks_recap$slope_v, #avg slope per each peak to valley edge
       slope_c = line_peaks_recap$slope_c, #avg slope peak peak to centre valley
       mean_slope_v = mean_v_slope, #avg slopes across all peaks in this grid
       mean_slope_c = mean_c_slope #avg slopes across all peaks in this grid
  )
  
  
}


#Function 2

open_grid_profiles <- function(s) {
  
  NAME <- paste(s,".csv", sep="")
  
  df <- read.csv(NAME, header=T)
  tail(df,5)
  head(df,10)
    
  #delete column 1, which is just index from imagej
  df1<-df[,-1]
  #reorder columns so col 2:n are gridlines, while col 1 is the distance
  df2<-df1[,c(2,1,3:ncol(df1))]
    
  profiles_grid <- df2
    
  return(profiles_grid)
  
}


#Function 3 not used, kept giving error about unable to open png at image 55

quick_plot <- function(grid_line_info, x, y, g, s) {
  
  plot.new()
  
  png(file=file.path(plotdir, paste(s, "grid_line", g, ".png")), 
      width=600, 
      height=350)
 
  plot(x,y, col='grey')
  abline(v=grid_line_info$peaks_index, col=okabe_pal[6])
  lines(x, grid_line_info$y_smooth, col='blue')
  abline(v=grid_line_info$peak_L_feet, col=my_pal_div[8], lty=2)
  abline(v=grid_line_info$peak_R_feet, col=my_pal_div[8], lty=2)
  dev.off()
  
}


###==========================================================================================================
##============================================= Function definition finished =================================

##------------------------------------------things to adjust for each experiment

EXP <- "...."

experiment_folder <- "......"


treatments <- c('est','mock')


list.files(pattern = "\\.csv$") 
##to list all csv files
#make sure each folder has a start_data excel file with three columns ('treatment', 'sample_name', 'relative leaf')

#---------Things to adjust only if necessary: decide the optimal spans, window width and treshold
w=25
span=0.1
treshold=2 #the higher this number the more peaks you will discard beacuse too low

#---------loops that run on all folders on all plants

#set wd to experiment folder
setwd(experiment_folder)

#workbook for the experiment
workbook_exp <- createWorkbook(paste(EXP, ".xlsx"))

#df to fill up for the total experiment
total_lines_df <- data.frame(grid_line=numeric(),
                           peaks_index=numeric(),
                           abs_peaksint=numeric(),
                           rel_peaksint=numeric(),
                           peak_R_feet=numeric(),
                           peak_L_feet=numeric(),
                           peak_w_c=numeric(),
                           peak_W_v=numeric(),
                           slope_v=numeric(),
                           slope_c=numeric(),
                           sample=character(),
                           trt=character(),
                           relative_leaf=numeric()
)

#2
total_lines_avgdf<- data.frame(grid_line=numeric(),
                             mean_abs_peaksint=numeric(),
                             mean_abs_valleyint=numeric(),
                             mean_rel_peakint=numeric(),
                             mean_peak_w_c=numeric(),
                             mean_peak_W_v=numeric(),
                             mean_slope_v=numeric(),
                             mean_slope_c=numeric(),
                             sample=character(),
                             trt=character(),
                             relative_leaf=numeric()
)

#3
total_lines_valley_df <- data.frame(grid_line=numeric(),
                                  abs_valleysint=numeric(),
                                  sample=character(),
                                  trt=character(),
                                  relative_leaf=numeric()
)



plot.new()

for (t in treatments) {
  
  setwd(experiment_folder)
  
  treatment_folder <- file.path(getwd(), t)
  
  setwd(treatment_folder)
  
  starting_info <- data.frame(read.xlsx("start_data.xlsx", 1))

  sample_name <- unique(starting_info[,2])
  
  workbook_trt <- createWorkbook(paste(EXP, t, ".xlsx"))

  #for each treatment we will have 3 dataframe to summarise
  #1
  trt_lines_df <- data.frame(grid_line=numeric(),
                             peaks_index=numeric(),
                             abs_peaksint=numeric(),
                             rel_peaksint=numeric(),
                             peak_R_feet=numeric(),
                             peak_L_feet=numeric(),
                             peak_w_c=numeric(),
                             peak_W_v=numeric(),
                             slope_v=numeric(),
                             slope_c=numeric(),
                             sample=character(),
                             trt=character(),
                             relative_leaf=numeric()
  )
  
  #2
  trt_lines_avgdf<- data.frame(grid_line=numeric(),
                               mean_abs_peaksint=numeric(),
                               mean_abs_valleyint=numeric(),
                               mean_rel_peakint=numeric(),
                               mean_peak_w_c=numeric(),
                               mean_peak_W_v=numeric(),
                               mean_slope_v=numeric(),
                               mean_slope_c=numeric(),
                               sample=character(),
                               trt=character(),
                               relative_leaf=numeric()
  )
  
  #3
  trt_lines_valley_df <- data.frame(grid_line=numeric(),
                                    abs_valleysint=numeric(),
                                    sample=character(),
                                    trt=character(),
                                    relative_leaf=numeric()
  )
  


for (s in sample_name) {
  
  #create a workbook for the sample
  workbook_sample <- createWorkbook(paste(EXP, s, ".xlsx"))
  
  relative_leaf <- starting_info[sample_name==s, 'relative.leaf']
  
  grid_profiles <- open_grid_profiles (s=s)
  
  x <- grid_profiles[,1] #distance is in col 1 of the grid profiles
  
  #create a folder for the plots
  plotdir <- file.path(getwd(), paste(s, " grid_lines"))
  dir.create(plotdir)
  
  #for each sample we will have 4 df to summarize the results:
  #1
  smooth_profiles <- data.frame(d=x)
  
  #2 average peak details for each line
  lines_avgdf <- data.frame(grid_line=numeric(),
                            mean_abs_peaksint=numeric(),
                            mean_abs_valleyint=numeric(),
                            mean_rel_peakint=numeric(),
                            mean_peak_w_c=numeric(),
                            mean_peak_W_v=numeric(),
                            mean_slope_v=numeric(),
                            mean_slope_c=numeric(),
                            sample=character(),
                            trt=character(),
                            relative_leaf=numeric()
                            )
  #3
  lines_df <- data.frame(grid_line=numeric(),
                         peaks_index=numeric(),
                         abs_peaksint=numeric(),
                         rel_peaksint=numeric(),
                         peak_R_feet=numeric(),
                         peak_L_feet=numeric(),
                         peak_w_c=numeric(),
                         peak_W_v=numeric(),
                         slope_v=numeric(),
                         slope_c=numeric(),
                         sample=character(),
                         trt=character(),
                         relative_leaf=numeric()
                        )
  #4
  lines_valley_df <- data.frame(grid_line=numeric(),
                                abs_valleysint=numeric(),
                                sample=character(),
                                trt=character(),
                                relative_leaf=numeric()
                                )
  

  grid_lines <- 2:ncol(grid_profiles)
  
  
  for (g in grid_lines) {
    
    y <- grid_profiles[,g]
    
    grid_line_info <- find_peaks_and_Valleys (x=x, y=y,w=w, span=span, treshold=treshold) # runs the fundtion to find peaks and valley on this grid line raw profile
    
    if ( any(is.na(grid_line_info)) == TRUE) {
      print(paste('grid line', g, 'in sample', s, 'skipped because of NAs'))
      
      plot.new()
      #plot the profile for trouble shooting
      png(file=file.path(plotdir, paste(s, "discarded_grid_line", g, ".png", sep="")), 
          width=600, 
          height=350)
      plot(x,y, col='grey')
      lines(x, grid_line_info$y_smooth, col='blue')
      dev.off()
    }
    
    if ( any(is.na(grid_line_info)) == FALSE) {
    
    grid_line_avgdf <- data.frame(grid_line=g,
                                  mean_abs_peaksint=grid_line_info$mean_abs_peaksint,
                                  mean_abs_valleyint=grid_line_info$mean_abs_valleyint,
                                  mean_rel_peakint=grid_line_info$mean_rel_peakint,
                                  mean_peak_w_c=grid_line_info$mean_peak_w_c,
                                  mean_peak_W_v=grid_line_info$mean_peak_W_v,
                                  mean_slope_v=grid_line_info$mean_slope_v,
                                  mean_slope_c=grid_line_info$mean_slope_c,
                                  sample=s,
                                  trt=t,
                                  relative_leaf=relative_leaf
                                  ) #fish out all mean values of this gridline from the list
  
    lines_avgdf <- rbind(lines_avgdf, grid_line_avgdf) #populate this line after line
    
    
    
    grid_line_df <- data.frame(grid_line=g,
                               peaks_index=grid_line_info$peaks_index,
                               abs_peaksint=grid_line_info$abs_peaksint,
                               rel_peaksint=grid_line_info$rel_peaksint,
                               peak_R_feet=grid_line_info$peak_R_feet,
                               peak_L_feet=grid_line_info$peak_L_feet,
                               peak_w_c=grid_line_info$peak_w_c,
                               peak_W_v=grid_line_info$peak_W_v,
                               slope_v=grid_line_info$slope_v,
                               slope_c=grid_line_info$slope_c,
                               sample=s,
                               trt=t,
                               relative_leaf=relative_leaf
                              ) #fish out all values of this gridline from the list
    
    lines_df <- rbind(lines_df, grid_line_df) #populate this line after line
    
    
    grid_line_valley_df <- data.frame(grid_line=g,
                                      abs_valleysint=grid_line_info$abs_valleysint,
                                      sample=s,
                                      trt=t,
                                      relative_leaf=relative_leaf
                                      )
    
    lines_valley_df <- rbind(lines_valley_df, grid_line_valley_df)
    
  
    smooth_profile <- grid_line_info$y_smooth
    first_free_col <- ncol(smooth_profiles)
    smooth_profiles[,first_free_col] <- smooth_profile #attach the profile of this grid line in the df of smooth profiles
    
    #run plotting function
    #quick_plot (grid_line_info, x=x, y=y, g=g, s=s)
    
    #just plot, plotting functions guves error
    plot.new()
    
    png(file=file.path(plotdir, paste(s, "grid_line ", g, " .png", sep="")), 
        width=600, 
        height=350)
    
    plot(x,y, col='grey')
    abline(v=grid_line_info$peaks_index, col=okabe_pal[6])
    lines(x, grid_line_info$y_smooth, col='blue')
    abline(v=grid_line_info$peak_L_feet, col=my_pal_div[8], lty=2)
    abline(v=grid_line_info$peak_R_feet, col=my_pal_div[8], lty=2)
    dev.off()
    
    }
    
    
  }#this closes the grid line, so the sample is complete
  
  #excel workbook for each sample  (workbook_sample) has
  #sheet1 - smooth profiles
  addWorksheet(workbook_sample, 'loess profiles')
  writeData(workbook_sample, 'loess profiles', smooth_profiles)
  saveWorkbook(workbook_sample, file=paste(EXP, s, ".xlsx"), overwrite = TRUE)
  
  
  #sheet2 - details of each peak of each line
  addWorksheet(workbook_sample, 'peaks details')
  writeData(workbook_sample, 'peaks details', lines_df)
  saveWorkbook(workbook_sample, file=paste(EXP, s, ".xlsx"), overwrite = TRUE)
  
  
  #sheet3 - average details of each line
  addWorksheet(workbook_sample, 'lines average')
  writeData(workbook_sample, 'lines average', lines_avgdf)
  saveWorkbook(workbook_sample, file=paste(EXP, s, ".xlsx"), overwrite = TRUE)
  
  #sheet4 - detail of each valley of each line
  addWorksheet(workbook_sample, 'valleys details')
  writeData(workbook_sample, 'valleys details', lines_valley_df)
  saveWorkbook(workbook_sample, file=paste(EXP, s, ".xlsx"), overwrite = TRUE)
  
  print(paste(s,'sample saved'))
  
  #now store the sample info in a new df that will be filled with the other samples too
  trt_lines_df <- rbind(trt_lines_df, lines_df)
  
  trt_lines_avgdf <- rbind(trt_lines_avgdf, lines_avgdf)
  
  trt_lines_valley_df <- rbind(trt_lines_valley_df, lines_valley_df)
  
  print('next sample')

}#this closes the sample
  #excel workbook for each treatment  (workbook_trt) has

  #sheet1 - details of each peak of each line in all samples of this trt
  addWorksheet(workbook_trt, 'peaks details')
  writeData(workbook_trt, 'peaks details', trt_lines_df)
  saveWorkbook(workbook_trt, file=paste(EXP, t, ".xlsx"), overwrite = TRUE)
  
  
  #sheet2 - average details of each line in all samples of this trt
  addWorksheet(workbook_trt, 'lines average')
  writeData(workbook_trt, 'lines average', trt_lines_avgdf)
  saveWorkbook(workbook_trt, file=paste(EXP, t, ".xlsx"), overwrite = TRUE)
  

  #sheet3 - detail of each valley of each line in all samples of this trt
  addWorksheet(workbook_trt, 'valleys details')
  writeData(workbook_trt, 'valleys details', trt_lines_valley_df)
  saveWorkbook(workbook_trt, file=paste(EXP, t, ".xlsx"), overwrite = TRUE)
  

#now rbind to a df that will be filled with the other treatment
total_lines_df <- rbind(total_lines_df,trt_lines_df)
total_lines_avgdf <- rbind(total_lines_avgdf,trt_lines_avgdf)
total_lines_valley_df <- rbind(total_lines_valley_df,trt_lines_valley_df)

print(paste(t,'treatment finished'))

}#this closes per treatment

print('next treatment')

setwd(experiment_folder)

addWorksheet(workbook_exp, 'peaks details')
writeData(workbook_exp, 'peaks details', total_lines_df)
saveWorkbook(workbook_exp, file=paste(EXP, ".xlsx"), overwrite = TRUE)


#sheet2 - average details of each line in all samples of this trt
addWorksheet(workbook_exp, 'lines average')
writeData(workbook_exp, 'lines average', total_lines_avgdf)
saveWorkbook(workbook_exp, file=paste(EXP, ".xlsx"), overwrite = TRUE)


#sheet3 - detail of each valley of each line in all samples of this trt
addWorksheet(workbook_exp, 'valleys details')
writeData(workbook_exp, 'valleys details', total_lines_valley_df)
saveWorkbook(workbook_exp, file=paste(EXP, ".xlsx"), overwrite = TRUE)



