                                        ##################################
                                        #           Analyze              #
                                        ##################################
#Still missing some data, like top file data

#Averages
  #Find averages of jumps by individual first
  #DONE: Find proportion of jumps as wall jumps


#run without script echo: cmd+shift+s
#run by line: cmd+enter                                    

#'''TODO: Remove rows with NA vel_start'''
                                        
                                        ##################################
                                        #           Set shit up          #
                                        ##################################
#----------------------------------------------------------------------------------------------------------------------
                                        
#clear a workspace
    rm(list = ls())

    setwd("/Users/judyjinn/R/walljumping")

# Show line numbers with error messages
    options(show.error.locations = TRUE)    

#load some functions
    source("wj_freq_func.R")
    
#Gain some knowledge and read your library
    library(ggplot2) #open ggplots
    library(plyr)   #renaming variables
    library(reshape2) #melting data into long form
    library(grid)  #manipulating plots
    library(gridExtra) #making multi plots
    library(boot)   #can't remember. Related to descriptive stats
    library(pastecs) #combines things for the descriptive stats
    library(psych) #simple descriptive stats
    library(directlabels)

    
#Grab a legend to use later. This is the function to do so
    get_legend<-function(myggplot){
        tmp= ggplot_gtable(ggplot_build(myggplot))
        leg= which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend= tmp$grobs[[leg]]
        return(legend)
    }
                                        ##################################
                                        #           Load all first       #
                                        ##################################
#----------------------------------------------------------------------------------------------------------------------
#open data
    alldata = read.csv("walljump_rawdata.csv") #all trials
    
# Remove trials where the trial was too short to get vel/accel data
    alldata = alldata[!is.na(alldata$vel_start),]
    # alldata$trial = as.factor(alldata$trial)
#ignore NA cells with  na.rm=TRUE for most stats functions

  variables = c('name', 'jump', 'trial', 
                'num_Rfl_steps','num_Lfl_steps', 'num_Rbl_steps', 'num_Lbl_steps', 
                'vert_err_full', 'vert_err_pre', 'vert_err_post',  
                'wj_time_start', 'wj_time_end', 'max_heightX', 'max_heightY', 
                'vel_start', 'pre_wj_vel', 'post_wj_vel', 'vel_end',  
                'accel_start', 'pre_wj_accel', 'post_wj_accel','accel_end',  
                'vel_fl_prewj1', 'vel_fl_postwj1', 'accel_fl_prewj1', 'accel_fl_postwj1', 
                'vel_bl_prewj1', 'vel_bl_postwj1', 'accel_bl_prewj1', 'accel_bl_postwj1', 
                'vel_fl_prewj2', 'vel_fl_postwj2', 'accel_fl_prewj2', 'accel_fl_postwj2', 
                'vel_bl_prewj2', 'vel_bl_postwj2', 'accel_bl_prewj2', 'accel_bl_postwj2', 
                'vel_fl_prewj3', 'vel_fl_postwj3', 'accel_fl_prewj3', 'accel_fl_postwj3', 
                'vel_bl_prewj3', 'vel_bl_postwj3', 'accel_bl_prewj3', 'accel_bl_postwj3', 
                'vel_fl_prewj4', 'vel_fl_postwj4', 'accel_fl_prewj4', 'accel_fl_postwj4', 
                'vel_bl_prewj4', 'vel_bl_postwj4', 'accel_bl_prewj4', 'accel_bl_postwj4')
 

  names = list('Beyonce', 'Britney', 'Cash', 'Iggy', 'John', 'Quasi', 'Ringo','Stevie')

# When tracking, we set the origin (0,0) to be the center of the dowel. However,
# 'H' jumps are 20cm higher than the midline and 'L' jumps are 20 cm below.
# This means that for the XY coordinates of the feet, a squirrel which touches at (-50, 10) for a 100 cm jump
# Is not at the same location as a squirrel which touches at (-50, 10) for a 100H jump. The latter is actually
# 30 cm higher than the former (Because the origin of the 100H jump is 100 higher)
  
#for ordering the jump types 
  jumps = c('50', '50L', '100H', '100', '100L','150H', '150')

  
  
  
                                        ##################################
                                        #       Basic Wall Jump          #
                                        ##################################
#----------------------------------------------------------------------------------------------------------------------
#grabs all the wall jumping frequency data
# saves plots of absolute number of wall jumps and also proportions of jumps
wj_freq = wj_freq_func(alldata, jumps, names)
  
  
  
                                        ##################################
                                        #        Additional Calcs        #
                                        ##################################
#----------------------------------------------------------------------------------------------------------------------
#Parsing out the velocity data
    alldata$wj_total_time = (alldata$wj_time_end-alldata$wj_time_start)*1000
  

  
  
                                        ##################################
                                        #         Find Averages          #
                                        ##################################
#----------------------------------------------------------------------------------------------------------------------
#All trials should not be taken as independent. There is definitely some effect of practice
#Average all trials first

#find the averages by name and jump
    name_jump_avg = aggregate(alldata[, 4:ncol(alldata)], by=list(name = alldata$name, jump=alldata$jump), 
                              FUN=mean, na.rm=TRUE, na.action=na.pass)

#find just average of jumps
    jump_avg = aggregate(name_jump_avg[, 3:ncol(name_jump_avg)], by=list(name_jump_avg$jump), 
                         FUN=mean, na.rm=TRUE, na.action=na.pass)
    colnames(jump_avg)[1] = "jump" #relabel the first column
#Reorder to proper order of jumps
    jump_avg = jump_avg[match(jumps, jump_avg$jump),]

#----------------------------- descriptive stats -----------------------------------
    alldata_desc = do.call("rbind",by(data = alldata[, 
        c('num_Rfl_steps', 'num_Lfl_steps', 'num_Rbl_steps', 'num_Lbl_steps', 
          'vert_err_full', 'vert_err_pre', 'vert_err_post', 
          'wj_time_start', 'wj_time_end', 'max_heightX', 'max_heightY', 
          'vel_start', 'pre_wj_vel', 'post_wj_vel', 'vel_end', 
          'accel_start', 'pre_wj_accel', 'post_wj_accel', 'accel_end', 
          'vel_fl_prewj1', 'vel_fl_postwj1', 
          'accel_fl_prewj1', 'accel_fl_postwj1', 
          'vel_bl_prewj1', 'vel_bl_postwj1', 
          'accel_bl_prewj1', 'accel_bl_postwj1', 
          'vel_fl_prewj2', 'vel_fl_postwj2', 
          'accel_fl_prewj2', 'accel_fl_postwj2', 
          'vel_bl_prewj2', 'vel_bl_postwj2', 
          'accel_bl_prewj2', 'accel_bl_postwj2', 
          'vel_fl_prewj3', 'vel_fl_postwj3', 
          'accel_fl_prewj3', 'accel_fl_postwj3', 
          'vel_bl_prewj3', 'vel_bl_postwj3', 
          'accel_bl_prewj3', 'accel_bl_postwj3', 
          'vel_fl_prewj4', 'vel_fl_postwj4', 
          'accel_fl_prewj4', 'accel_fl_postwj4', 
          'vel_bl_prewj4', 'vel_bl_postwj4', 
          'accel_bl_prewj4', 'accel_bl_postwj4', 
          'Rfl_touchX1', 'Rfl_touchY1', 'Rfl_stance1', 'top_Rfleg_td1', 'top_Rfleg_lo1', 
          'Lfl_stance1', 'top_Lfleg_td1', 'top_Lfleg_lo1', 
          'Rbl_touchX1', 'Rbl_touchY1', 'Rbl_stance1', 'top_Rbleg_td1', 'top_Rbleg_lo1', 
          'Lbl_stance1', 'top_Lbleg_td1', 'top_Lbleg_lo1', 
          'Rfl_touchX2', 'Rfl_touchY2', 'Rfl_stance2', 'top_Rfleg_td2', 'top_Rfleg_lo2', 'Rfl_swing1', 
          'Lfl_stance2', 'top_Lfleg_td2', 'top_Lfleg_lo2', 'Lfl_swing1', 
          'Rbl_touchX2', 'Rbl_touchY2', 'Rbl_stance2', 'top_Rbleg_td2', 'top_Rbleg_lo2', 'Rbl_swing1', 
          'Lbl_stance2', 'top_Lbleg_td2', 'top_Lbleg_lo2', 'Lbl_swing1', 
          'Lfl_stance3', 'top_Lfleg_td3', 'top_Lfleg_lo3', 'Lfl_swing2', 
          'Lbl_stance3', 'top_Lbleg_td3', 'top_Lbleg_lo3', 'Lbl_swing2', 'wj_total_time')], 
        INDICES = alldata$jump, FUN=stat.desc, basic= TRUE))
    write.csv(alldata_desc, file="alldata_desc.csv") #saalldata


    jump_avg_desc = do.call("rbind",by(data = name_jump_avg[, c( 
        'num_Rfl_steps', 'num_Lfl_steps', 'num_Rbl_steps', 'num_Lbl_steps', 
        'vert_err_full', 'vert_err_pre', 'vert_err_post', 
        'wj_time_start', 'wj_time_end', 'max_heightX', 'max_heightY', 
        'vel_start', 'pre_wj_vel', 'post_wj_vel', 'vel_end', 
        'accel_start', 'pre_wj_accel', 'post_wj_accel', 'accel_end', 
        'vel_fl_prewj1', 'vel_fl_postwj1', 
        'accel_fl_prewj1', 'accel_fl_postwj1', 
        'vel_bl_prewj1', 'vel_bl_postwj1', 
        'accel_bl_prewj1', 'accel_bl_postwj1', 
        'vel_fl_prewj2', 'vel_fl_postwj2', 
        'accel_fl_prewj2', 'accel_fl_postwj2', 
        'vel_bl_prewj2', 'vel_bl_postwj2', 
        'accel_bl_prewj2', 'accel_bl_postwj2', 
        'vel_fl_prewj3', 'vel_fl_postwj3', 
        'accel_fl_prewj3', 'accel_fl_postwj3', 
        'vel_bl_prewj3', 'vel_bl_postwj3', 
        'accel_bl_prewj3', 'accel_bl_postwj3', 
        'vel_fl_prewj4', 'vel_fl_postwj4', 
        'accel_fl_prewj4', 'accel_fl_postwj4', 
        'vel_bl_prewj4', 'vel_bl_postwj4', 
        'accel_bl_prewj4', 'accel_bl_postwj4', 
        'Rfl_touchX1', 'Rfl_touchY1', 'Rfl_stance1', 'top_Rfleg_td1', 'top_Rfleg_lo1', 
        'Lfl_stance1', 'top_Lfleg_td1', 'top_Lfleg_lo1', 
        'Rbl_touchX1', 'Rbl_touchY1', 'Rbl_stance1', 'top_Rbleg_td1', 'top_Rbleg_lo1', 
        'Lbl_stance1', 'top_Lbleg_td1', 'top_Lbleg_lo1', 
        'Rfl_touchX2', 'Rfl_touchY2', 'Rfl_stance2', 'top_Rfleg_td2', 'top_Rfleg_lo2', 'Rfl_swing1', 
        'Lfl_stance2', 'top_Lfleg_td2', 'top_Lfleg_lo2', 'Lfl_swing1', 
        'Rbl_touchX2', 'Rbl_touchY2', 'Rbl_stance2', 'top_Rbleg_td2', 'top_Rbleg_lo2', 'Rbl_swing1', 
        'Lbl_stance2', 'top_Lbleg_td2', 'top_Lbleg_lo2', 'Lbl_swing1', 
        'Lfl_stance3', 'top_Lfleg_td3', 'top_Lfleg_lo3', 'Lfl_swing2', 
        'Lbl_stance3', 'top_Lbleg_td3', 'top_Lbleg_lo3', 'Lbl_swing2', 'wj_total_time')], 
      INDICES = name_jump_avg$jump, FUN=stat.desc, basic= TRUE))
    write.csv(jump_avg_desc, file="jumpsavg_desc.csv")




                                ############################################
                                #        Velocity and Accelerations       #
                                ###########################################
#----------------------------------------------------------------------------------------------------------------------
    
    velocities = jump_avg[c("jump", "vel_start", "pre_wj_vel", 'post_wj_vel', "vel_end")]
    
# change ot long format for plotting
    velocities_long = melt(velocities, id.vars=c("jump"))
    
#relabel values
    velocities_long$variable = revalue(velocities_long$variable, c("vel_start"="start", "pre_wj_vel"="pre wall contact", 
                                                                   "post_wj_vel"="post wall contact", "vel_end"="end"))
#add a new line between each space so graph labels don't overlap
    levels(velocities_long$variable)= gsub(" ", "\n", levels(velocities_long$variable))
    
#Acceleration data in the same format as velocity
    accelerations = jump_avg[c("jump", "accel_start", "pre_wj_accel", 'post_wj_accel', "accel_end")]
    accelerations_long = melt(accelerations, id.vars=c("jump"))
    accelerations_long$variable = revalue(accelerations_long$variable, 
                                          c("accel_start"="start", "pre_wj_accel"="pre wall contact",                                                                   "post_wj_accel"="post wall contact", "accel_end"="end"))
    levels(accelerations_long$variable)= gsub(" ", "\n", levels(accelerations_long$variable))
    
    
#Because R sucks and I can't figure out how to access things nicely like a matlab struct... break all data into subsets
    vel_Rfl_wj = jump_avg[c("jump", "vel_start", "vel_fl_prewj1","vel_fl_postwj1","vel_fl_prewj2","vel_fl_postwj2",
                            "vel_end")]
    vel_Rbl_wj = jump_avg[c("jump", "vel_start","vel_bl_prewj1","vel_bl_postwj1","vel_bl_prewj2","vel_bl_postwj2",
                            "vel_end")]
    accel_Rfl_wj = jump_avg[c("jump", "vel_start","accel_fl_prewj1","accel_fl_postwj1","accel_fl_prewj2",
                             "accel_fl_postwj2", "vel_end")]
    accel_Rbl_wj = jump_avg[c("jump", "vel_start","accel_bl_prewj1","accel_bl_postwj1","accel_bl_prewj2",
                             "accel_bl_postwj2", "vel_end")]
#melt into long format for plotting to be able to sort by variable name
    vel_Rfl_wj_long = melt(vel_Rfl_wj, id.vars=c("jump"))
#recode the labels so they don't look stupid on the plots
    vel_Rfl_wj_long$variable = revalue(vel_Rfl_wj_long$variable, 
                                          c("vel_start"="start",
                                            "vel_fl_prewj1"="pre wall jump 1", "vel_fl_postwj1"="post wall jump 1",                                                   "vel_fl_prewj2"="pre wall jump 2", "vel_fl_postwj2"="post wall jump 2",
                                          "vel_end"="end"))
#add some spaces between the labels so they don't overlap on the grids
    levels(vel_Rfl_wj_long$variable)= gsub(" ", "\n", levels(vel_Rfl_wj_long$variable))
    
#Repeat all for every other subset
    vel_Rbl_wj_long = melt(vel_Rbl_wj, id.vars=c("jump"))
    vel_Rbl_wj_long$variable = revalue(vel_Rbl_wj_long$variable, 
                                      c("vel_start"="start",
                                        "vel_bl_prewj1"="pre wall jump 1", "vel_bl_postwj1"="post wall jump 1",                                                     "vel_bl_prewj2"="pre wall jump 2", "vel_bl_postwj2"="post wall jump 2",
                                        "vel_end"="end"))
    levels(vel_Rbl_wj_long$variable)= gsub(" ", "\n", levels(vel_Rbl_wj_long$variable))
    
    accel_Rfl_wj_long = melt(accel_Rfl_wj, id.vars=c("jump"))
    accel_Rfl_wj_long$variable = revalue(accel_Rfl_wj_long$variable, 
                                      c("vel_start"="start",
                                        "accel_fl_prewj1"="pre wall jump 1", "accel_fl_postwj1"="post wall jump 1",                                             "accel_fl_prewj2"="pre wall jump 2", "accel_fl_postwj2"="post wall jump 2",
                                        "vel_end"="end"))
    levels(accel_Rfl_wj_long$variable)= gsub(" ", "\n", levels(accel_Rfl_wj_long$variable))
    
    accel_Rbl_wj_long = melt(accel_Rbl_wj, id.vars=c("jump"))
    accel_Rbl_wj_long$variable = revalue(accel_Rbl_wj_long$variable, 
                                      c("vel_start"="start",
                                        "accel_bl_prewj1"="pre wall jump 1", "accel_bl_postwj1"="post wall jump 1",                                             "accel_bl_prewj2"="pre wall jump 2", "accel_bl_postwj2"="post wall jump 2", 
                                        "vel_end"="end"))
    levels(accel_Rbl_wj_long$variable)= gsub(" ", "\n", levels(accel_Rbl_wj_long$variable))
    
                             
    
    
    
                                ############################################
                                #        Stance, Swing, Stride Length      #
                                ###########################################
#----------------------------------------------------------------------------------------------------------------------
# Going to find the over all averages for stance/swing not by each number of steps.
# Subset all the data we want, convert it into long form, relabel them generaically as stance, swing, stride
# Find the descriptive stats that way
    
    
#Stance data
    Lfl_stance = jump_avg[c("jump", 'Lfl_stance1', 'Lfl_stance2', 'Lfl_stance3')]
    Lfl_stance[,2:length(Lfl_stance)]=Lfl_stance[,2:length(Lfl_stance)]
    Lfl_stance_long = melt(Lfl_stance, id.vars=c("jump")) #change to long form
    Lfl_stance_long =rename(Lfl_stance_long, c("value"="Lfl_stance")) #rename the variable
    Lfl_stance_long$variable = revalue(Lfl_stance_long$variable, 
                                      c("Lfl_stance1"="Stance 1","Lfl_stance2"="Stance 2",
                                        "Lfl_stance3"="Stance 3"))
    
    Rfl_stance = jump_avg[c("jump", 'Rfl_stance1', 'Rfl_stance2')]
    Rfl_stance[,2:length(Rfl_stance)]=Rfl_stance[,2:length(Rfl_stance)]
    Rfl_stance_long = melt(Rfl_stance, id.vars=c("jump")) #change to long form
    Rfl_stance_long =rename(Rfl_stance_long, c("value"="Rfl_stance")) #rename the variable
    Rfl_stance_long$variable = revalue(Rfl_stance_long$variable, 
                                       c("Rfl_stance1"="Stance 1","Rfl_stance2"="Stance 2"))
    
    Lbl_stance = jump_avg[c("jump", 'Lbl_stance1', 'Lbl_stance2', 'Lbl_stance3')]
    Lbl_stance[,2:length(Lbl_stance)]=Lbl_stance[,2:length(Lbl_stance)]
    Lbl_stance_long = melt(Lbl_stance, id.vars=c("jump")) #change to long form
    Lbl_stance_long =rename(Lbl_stance_long, c("value"="Lbl_stance")) #rename the variable
    Lbl_stance_long$variable = revalue(Lbl_stance_long$variable, 
                                       c("Lbl_stance1"="Stance 1","Lbl_stance2"="Stance 2",
                                         "Lbl_stance3"="Stance 3"))
    
    Rbl_stance = jump_avg[c("jump", 'Rbl_stance1', 'Rbl_stance2')]
    Rbl_stance[,2:length(Rbl_stance)]=Rbl_stance[,2:length(Rbl_stance)]
    Rbl_stance_long = melt(Rbl_stance, id.vars=c("jump")) #change to long form
    Rbl_stance_long =rename(Rbl_stance_long, c("value"="Rbl_stance")) #rename the variable
    Rbl_stance_long$variable = revalue(Rbl_stance_long$variable, 
                                       c("Rbl_stance1"="Stance 1","Rbl_stance2"="Stance 2"))
    
    # Merge dataframes. Crappy code, but no need for anything complicated...
    stance = merge(Lfl_stance_long, Rfl_stance_long, by=c("variable","jump"), all.x=TRUE)
    stance = merge(stance, Lbl_stance_long, by=c("variable","jump"), all.x=TRUE)
    stance = merge(stance, Rbl_stance_long, by=c("variable","jump"), all.x=TRUE)

    stance_desc = do.call("rbind",by(data = stance[,c("Rfl_stance","Lfl_stance","Rbl_stance","Lbl_stance")], 
                                      INDICES = stance$jump, FUN=stat.desc, basic= TRUE))
    write.csv(stance_desc, file="stance_desc.csv") 
    
    
#Swing Data    
    Lfl_swing = jump_avg[c("jump", 'Lfl_swing1', 'Lfl_swing2')]
    Lfl_swing[,2:length(Lfl_swing)]=Lfl_swing[,2:length(Lfl_swing)]
    Lfl_swing_long = melt(Lfl_swing, id.vars=c("jump")) #change to long form
    Lfl_swing_long =rename(Lfl_swing_long, c("value"="Lfl_swing")) #rename the variable
    Lfl_swing_long$variable = revalue(Lfl_swing_long$variable, 
                                       c("Lfl_swing1"="swing 1","Lfl_swing2"="swing 2"))
    
    Rfl_swing = jump_avg[c("jump", 'Rfl_swing1')]
    Rfl_swing[,2:length(Rfl_swing)]=Rfl_swing[,2:length(Rfl_swing)]
    Rfl_swing_long = melt(Rfl_swing, id.vars=c("jump")) #change to long form
    Rfl_swing_long =rename(Rfl_swing_long, c("value"="Rfl_swing")) #rename the variable
    Rfl_swing_long$variable = revalue(Rfl_swing_long$variable, 
                                       c("Rfl_swing1"="swing 1"))
    
    Lbl_swing = jump_avg[c("jump", 'Lbl_swing1', 'Lbl_swing2')]
    Lbl_swing[,2:length(Lbl_swing)]=Lbl_swing[,2:length(Lbl_swing)]
    Lbl_swing_long = melt(Lbl_swing, id.vars=c("jump")) #change to long form
    Lbl_swing_long =rename(Lbl_swing_long, c("value"="Lbl_swing")) #rename the variable
    Lbl_swing_long$variable = revalue(Lbl_swing_long$variable, 
                                       c("Lbl_swing1"="swing 1","Lbl_swing2"="swing 2"))
    
    Rbl_swing = jump_avg[c("jump", 'Rbl_swing1')]
    Rbl_swing[,2:length(Rbl_swing)]=Rbl_swing[,2:length(Rbl_swing)]
    Rbl_swing_long = melt(Rbl_swing, id.vars=c("jump")) #change to long form
    Rbl_swing_long =rename(Rbl_swing_long, c("value"="Rbl_swing")) #rename the variable
    Rbl_swing_long$variable = revalue(Rbl_swing_long$variable, 
                                       c("Rbl_swing1"="swing 1"))
    
    # Merge dataframes. Crappy code, but no need for anything complicated...
    swing = merge(Lfl_swing_long, Rfl_swing_long, by=c("variable","jump"), all.x=TRUE)
    swing = merge(swing, Lbl_swing_long, by=c("variable","jump"), all.x=TRUE)
    swing = merge(swing, Rbl_swing_long, by=c("variable","jump"), all.x=TRUE)
    
    swing_desc = do.call("rbind",by(data = swing[,c("Rfl_swing","Lfl_swing","Rbl_swing","Lbl_swing")], 
                                     INDICES = swing$jump, FUN=stat.desc, basic= TRUE))
    write.csv(swing_desc, file="swing_desc.csv") 
    
#stride length data.
    # Have to manually look for number of touches (or write a script for it later)
    Rfl_touch = jump_avg[c("jump","Rfl_touchX1", "Rfl_touchY1", "Rfl_touchX2", "Rfl_touchY2")]   
    Rfl_stride = data.frame(Rfl_touch$jump)
    
    #-1 for jump name col, /2 because you only need the X or Y to count number of steps
    # then /2 one more time to actually get the number of steps taken
    Rfl_stride_num = (ncol(Rfl_touch)-1)/2/2 
    
    for (i in 1:(Rfl_stride_num)){
        #sqrt((X2-X1)**2 + (Y2-Y1)**2) for length of a stride
        #Rfl_stride[i+1] to skip over jump name column, the other 2*i, etc are to pull correct X/Y values
        Rfl_stride[i+1] = sqrt((Rfl_touch[2*i]-Rfl_touch[2*i+2])**2 + (Rfl_touch[i*2+1]-Rfl_touch[i*2+3])**2)
        
        #rename column names for stride length
        colnames(Rfl_stride)[2:( Rfl_stride_num+1)]=paste("stride",toString(i),sep="")
    }
    
    
    #renaming variables, fl_stride is the average for each wall jump
    Rfl_stride = rename(Rfl_stride, c("Rfl_touch.jump"="jump"))
        
    #find an overall average for all wall jumps per jump type if there's more than 1 step
    Rfl_stride_long = melt(Rfl_stride, id.vars=c("jump"))

        Rfl_stride_avg = aggregate(Rfl_stride_long[, 3:ncol(Rfl_stride_long)], by=list(Rfl_stride_long$jump), 
                             FUN=mean, na.rm=TRUE, na.action=na.pass)
        Rfl_stride_avg = Rfl_stride_avg[match(jumps, Rfl_stride_avg$Group.1),]
    #concatenate it back into the stride lengths
        Rfl_stride$strideavg = Rfl_stride_avg$x 
    
        Rfl_stride_long =rename(Rfl_stride_long, c("value"="Rfl_stride"))
        
        
        Rfl_stride_desc = do.call("rbind",by(data = Rfl_stride_long[,c("Rfl_stride")], 
                                            INDICES = Rfl_stride_long$jump, FUN=stat.desc, basic= TRUE))
        write.csv(Rfl_stride_desc, file="Rfl_stride_desc.csv")
        
    
    
    # stride length for back leg
    Rbl_touch = jump_avg[c("jump","Rbl_touchX1","Rbl_touchY1","Rbl_touchX2","Rbl_touchY2")]   
    Rbl_stride = data.frame(Rbl_touch$jump)
    
    Rbl_stride_num = (ncol(Rbl_touch)-1)/2/2 
    
    for (i in 1:(Rbl_stride_num)){
        Rbl_stride[i+1] = sqrt((Rbl_touch[2*i]-Rbl_touch[2*i+2])**2 + (Rbl_touch[i*2+1]-Rbl_touch[i*2+3])**2)
        
        #rename column names for stride length
        colnames(Rbl_stride)[2:( Rbl_stride_num+1)]=paste("stride",toString(i),sep="")
    }
    
    Rbl_stride = rename(Rbl_stride, c("Rbl_touch.jump"="jump"))

    Rbl_stride_long = melt(Rbl_stride, id.vars=c("jump")) 

        Rbl_stride_avg = aggregate(Rbl_stride_long[, 3:ncol(Rbl_stride_long)], by=list(Rbl_stride_long$jump), 
                                  FUN=mean, na.rm=TRUE, na.action=na.pass)
        Rbl_stride_avg = Rbl_stride_avg[match(jumps, Rbl_stride_avg$Group.1),]
        Rbl_stride$strideavg = Rbl_stride_avg$x
        Rbl_stride_long =rename(Rbl_stride_long, c("value"="Rbl_stride"))
        
        Rbl_stride_desc = do.call("rbind",by(data = Rbl_stride_long[,c("Rbl_stride")], 
                                        INDICES = Rbl_stride_long$jump, FUN=stat.desc, basic= TRUE))
        write.csv(Rbl_stride_desc, file="Rbl_stride_desc.csv") 
    
    







                                        ##################################
                                        #         Changes By Trial       #
                                        ##################################
#----------------------------------------------------------------------------------------------------------------------
#find the averages by jump and trial number
    jump_trial = aggregate(alldata[, 4:ncol(alldata)], by=list(jump=alldata$jump,trial=alldata$trial), 
                 FUN=mean, na.rm=TRUE, na.action=na.pass)
    


    
    





                                        ##################################
                                        #         Plotting Data          #
                                        ##################################
#----------------------------------------------------------------------------------------------------------------------
    
#plotting the average vertical error using all data
avg_vert_err_full = ggplot(data=jump_avg, aes(x = jump, y = vert_err_full)) + 
  scale_x_discrete(limits = jumps) + #change this to change order of jump type
  geom_point(size=4)+ 
  labs(x="Jump Type", y = "Average Vertical Error using All Data (cm)")+
  scale_fill_manual(values=c('black','lightgray')) +
  theme_classic() +
  geom_hline(aes(yintercept = 0), linetype="dashed") + 
  theme(
    title = element_text(face="bold", colour=, size=14),
    axis.title.x = element_text(face="bold", colour=, size=12),
    axis.text.x  = element_text(angle=, vjust=, size=10),
    axis.title.y = element_text(face="bold", colour=, size=12),
    axis.text.y  = element_text(angle=, vjust=, size=10),
    plot.background = element_blank(),
    panel.grid.major = element_line(colour = "light gray"),
    panel.grid.minor = element_blank(),
    aspect.ratio=(9/10)) +
    theme(axis.line.x = element_line(color="black"), #ggplot error where my axes suddenly disappeared. Readd manually
          axis.line.y = element_line(color="black"))

  ggsave("avg_vert_err_full.pdf", avg_vert_err_full,width = 4, height = 4, units = "in")
  
  #plotting the average vertical error using only pre jump points
  avg_vert_err_pre = ggplot(data=jump_avg, aes(x = jump, y = vert_err_pre)) + 
      scale_x_discrete(limits = jumps) + #change this to change order of jump type
      geom_point(size=4)+ 
      labs(x="Jump Type", y = "Estimated Vertical Error using Before Wall Jump Data(cm)")+
      scale_fill_manual(values=c('black','lightgray')) +
      theme_classic() +
      geom_hline(aes(yintercept = 0), linetype="dashed") + 
      theme(
          title = element_text(face="bold", colour=, size=14),
          axis.title.x = element_text(face="bold", colour=, size=12),
          axis.text.x  = element_text(angle=, vjust=, size=10),
          axis.title.y = element_text(face="bold", colour=, size=12),
          axis.text.y  = element_text(angle=, vjust=, size=10),
          plot.background = element_blank(),
          panel.grid.major = element_line(colour = "light gray"),
          panel.grid.minor = element_blank(),
          aspect.ratio=(9/10)) +
      theme(axis.line.x = element_line(color="black"), #ggplot error where my axes suddenly disappeared. Readd manually
            axis.line.y = element_line(color="black"))
  
  ggsave("avg_vert_err_pre.pdf", avg_vert_err_pre,width = 4, height = 4, units = "in")
  
  
  #plotting the average vertical error using only post jump points
  avg_vert_err_post = ggplot(data=jump_avg, aes(x = jump, y = vert_err_post)) + 
      scale_x_discrete(limits = jumps) + #change this to change order of jump type
      geom_point(size=4)+ 
      labs(x="Jump Type", y = "Estimated Vertical Error using After Wall Jump Data(cm)")+
      scale_fill_manual(values=c('black','lightgray')) +
      theme_classic() +
      geom_hline(aes(yintercept = 0), linetype="dashed") + 
      theme(
          title = element_text(face="bold", colour=, size=14),
          axis.title.x = element_text(face="bold", colour=, size=12),
          axis.text.x  = element_text(angle=, vjust=, size=10),
          axis.title.y = element_text(face="bold", colour=, size=12),
          axis.text.y  = element_text(angle=, vjust=, size=10),
          plot.background = element_blank(),
          panel.grid.major = element_line(colour = "light gray"),
          panel.grid.minor = element_blank(),
          aspect.ratio=(9/10)) +
      theme(axis.line.x = element_line(color="black"), #ggplot error where my axes suddenly disappeared. Readd manually
            axis.line.y = element_line(color="black"))
  
  ggsave("avg_vert_err_post.pdf", avg_vert_err_post, width = 4, height = 4, units = "in")
  
  

#find the proportion of the distance in which the wall jump was generally initated
  jump_startX = matrix(c(-50,-67.08,-77.46,-100,-118.30,-128.5,-150))
  jump_avg$jump_startX = jump_startX
  wj_start_propX = matrix(jump_startX)
  for (i in 1:length(jump_avg$jump)){
    dist_startX = jump_avg$jump_startX[i]-jump_avg$Rfl_touchX1[i]
    wj_start_propX[i] = dist_startX/jump_avg$jump_startX[i]
  }
  
  wj_start_prop_table = data.frame(jumps,wj_start_propX)

#plotting the average of when a wall jump occurs in proportion to x-distance from goal
  prop_startwj_x = ggplot(data=wj_start_prop_table, aes(x = jumps, y = wj_start_propX)) + 
    scale_x_discrete(limits = jumps) + #change this to change order of jump type
    geom_bar(stat = "identity") + 
    xlab("Jump Type") + 
    ylab(expression(atop(paste("Percent of Jump Distance"),"Before Wall Contact")))+
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values=c('black','lightgray')) +
    theme_classic() +
    theme(
      title = element_text(face="bold", colour=, size=14),
      axis.title.x = element_text(face="bold", colour=, size=12),
      axis.text.x  = element_text(angle=, vjust=, size=10),
      axis.title.y = element_text(face="bold", colour=, size=12),
      axis.text.y  = element_text(angle=, vjust=, size=10),
      plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      aspect.ratio=(9/10)) +
      theme(axis.line.x = element_line(color="black"), #ggplot error where my axes suddenly disappeared. Readd manually
            axis.line.y = element_line(color="black"))
  
  ggsave("prop_startwj_x.pdf", prop_startwj_x, width = 4, height = 4, units = "in")
  

#Plot average height of Y of the first wall jump
#A negative value means they contacted the wall below where the perch was, positive is vice versa
  avg_wj_Y = ggplot(data=jump_avg, aes(x = jump, y = Rfl_touchY1)) + 
    scale_x_discrete(limits = jumps) + #change this to change order of jump type
    geom_point(size=4)+ 
    labs(x="Jump Type", y = "Average Height of Wall Contact (cm)")+
    coord_cartesian(ylim = c(-10, 30)) +
    scale_fill_manual(values=c('black','lightgray')) +
    geom_hline(aes(yintercept = 0), linetype="dashed") + 
      theme_bw() + 
      theme(
          title = element_text(face="bold", colour=, size=14),
          axis.title.x = element_text(face="bold", colour=, size=12),
          axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
          axis.title.y = element_text(face="bold", colour=, size=12),
          axis.text.y  = element_text(angle=, vjust=, size=8),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(angle=, vjust=, size=8),
          plot.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = "gray"),
          panel.grid.minor = element_blank(),
          aspect.ratio=(7/8)) +
      theme(axis.line.x = element_line(color="black"),
            axis.line.y = element_line(color="black")) 
  ggsave("avg_wj_Y.pdf", avg_wj_Y, width = 4, height = 4, units = "in")
  
  

#Velocities at start, wall jump and end
  vel = ggplot(velocities_long, aes(x = variable, y = value, group=jump, shape=jump, colour = jump)) + 
      geom_line() +
      geom_point() +
      # scale_x_discrete(limits = jumps) + #change this to change order of jump type
      labs(x="Time of Jump", y = "Velocity (cm/s)")+
      scale_y_continuous(minor_breaks = seq(0 , 800, 50), breaks = seq(0, 800, 100)) +
      # scale_fill_manual(values=c('black','lightgray')) +
      theme_bw() + 
      theme(
          title = element_text(face="bold", colour=, size=14),
          axis.title.x = element_text(face="bold", colour=, size=12),
          axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
          axis.title.y = element_text(face="bold", colour=, size=12),
          axis.text.y  = element_text(angle=, vjust=, size=8),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(angle=, vjust=, size=8),
          plot.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = "gray"),
          panel.grid.minor = element_blank(),
          aspect.ratio=(7/8)) +
      theme(axis.line.x = element_line(color="black"),
            axis.line.y = element_line(color="black")) 
  ggsave("vel.pdf", vel, width = 6, height = 4, units = "in")
  
  
#Accelerations at start, wall jump and end
  accel = ggplot(accelerations_long, aes(x = variable, y = value, group=jump, shape=jump, colour = jump)) + 
      geom_line() +
      geom_point() +
      # scale_x_discrete(limits = jumps) + #change this to change order of jump type
      labs(x="Time of Jump", y = "Acceleration (cm/ms2)")+
      # scale_y_continuous(minor_breaks = seq(0 , 800, 50), breaks = seq(0, 800, 100)) +
      # scale_fill_manual(values=c('black','lightgray')) +
      theme_bw() + 
      theme(
          title = element_text(face="bold", colour=, size=14),
          axis.title.x = element_text(face="bold", colour=, size=12),
          axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
          axis.title.y = element_text(face="bold", colour=, size=12),
          axis.text.y  = element_text(angle=, vjust=, size=8),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(angle=, vjust=, size=8),
          plot.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = "gray"),
          panel.grid.minor = element_blank(),
          aspect.ratio=(7/8)) +
      theme(axis.line.x = element_line(color="black"),
            axis.line.y = element_line(color="black")) 
  ggsave("accel.pdf", accel, width = 6, height = 4, units = "in")
  
  
#Velocities at pre and post wall jump for front leg
  vel_Rfl = ggplot(vel_Rfl_wj_long, aes(x = variable, y = value, group=jump, shape=jump, colour = jump)) + 
      geom_line() +
      geom_point() +
      ggtitle("Front Leg Velocity") + 
      labs(x="Time of Jump", y = "Velocity (cm/s)")+
      scale_y_continuous(minor_breaks = seq(0 , 800, 50), breaks = seq(0, 800, 80)) +
      theme_bw() + 
      theme(
          title = element_text(face="bold", colour=, size=14),
          axis.title.x = element_text(face="bold", colour=, size=12),
          axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
          axis.title.y = element_text(face="bold", colour=, size=12),
          axis.text.y  = element_text(angle=, vjust=, size=8),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(angle=, vjust=, size=8),
          plot.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = "gray"),
          panel.grid.minor = element_blank(),
          aspect.ratio=(7/8)) +
      theme(axis.line.x = element_line(color="black"),
            axis.line.y = element_line(color="black")) 

#Velocities at pre and post wall jump for back leg
  vel_Rbl = ggplot(vel_Rbl_wj_long, aes(x = variable, y = value, group=jump, shape=jump, colour = jump)) + 
      geom_line() +
      geom_point() +
      ggtitle("Hind Leg Velocity") + 
      labs(x="Time of Jump", y = "Velocity (cm/s)")+
      scale_y_continuous(minor_breaks = seq(0 , 800, 50), breaks = seq(0, 800, 80)) +
      theme_bw() + 
      theme(
          title = element_text(face="bold", colour=, size=14),
          axis.title.x = element_text(face="bold", colour=, size=12),
          axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
          axis.title.y = element_text(face="bold", colour=, size=12),
          axis.text.y  = element_text(angle=, vjust=, size=8),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(angle=, vjust=, size=8),
          plot.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = "gray"),
          panel.grid.minor = element_blank(),
          aspect.ratio=(7/8)) +
      theme(axis.line.x = element_line(color="black"),
            axis.line.y = element_line(color="black")) 

  
#Acceleration at pre and post wall jump for front leg
  accel_Rfl = ggplot(accel_Rfl_wj_long, aes(x = variable, y = value, group=jump, shape=jump, colour = jump)) + 
      geom_line() +
      geom_point() +
      ggtitle("Front Leg Acceleration") + 
      labs(x="Time of Jump", y = "Accleration (cm/s)")+
      theme_bw() + 
      theme(
          title = element_text(face="bold", colour=, size=14),
          axis.title.x = element_text(face="bold", colour=, size=12),
          axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
          axis.title.y = element_text(face="bold", colour=, size=12),
          axis.text.y  = element_text(angle=, vjust=, size=8),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(angle=, vjust=, size=8),
          plot.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = "gray"),
          panel.grid.minor = element_blank(),
          aspect.ratio=(7/8)) +
      theme(axis.line.x = element_line(color="black"),
            axis.line.y = element_line(color="black")) 

#Acceleration at pre and post wall jump for back leg
  accel_Rbl = ggplot(accel_Rbl_wj_long, aes(x = variable, y = value, group=jump, shape=jump, colour = jump)) + 
      geom_line() +
      geom_point() +
      ggtitle("Hind Leg Acceleration") + 
      labs(x="Time of Jump", y = "Acceleration (cm/s)")+
      theme_bw() + 
      theme(
          title = element_text(face="bold", colour=, size=14),
          axis.title.x = element_text(face="bold", colour=, size=12),
          axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
          axis.title.y = element_text(face="bold", colour=, size=12),
          axis.text.y  = element_text(angle=, vjust=, size=8),
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(angle=, vjust=, size=8),
          plot.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = "gray"),
          panel.grid.minor = element_blank(),
          aspect.ratio=(7/8)) +
      theme(axis.line.x = element_line(color="black"),
            axis.line.y = element_line(color="black")) 

# This is a blank plot to hold the place of a null graph when organizing plotsinto cols and rows 
  blankPlot= ggplot()+geom_blank(aes(1,1))+
      theme(
          plot.background = element_blank(), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(), 
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank()
      )
  
#Following section combines all the vel and accel fl/bl plots into one

    
    #getting a legend
    legend= get_legend(accel_Rbl)
    
    # Removing all other legends now
    vel_Rfl = vel_Rfl + theme(legend.position="none")
    vel_Rbl = vel_Rbl + theme(legend.position="none")
    accel_Rfl = accel_Rfl + theme(legend.position="none")
    accel_Rbl = accel_Rbl + theme(legend.position="none")
    

    #Combine all graphs into a 3x2 grid. col 3 consists of 1 blank graph and the legend
    Rlegs_vel_accel = grid.arrange(vel_Rfl, vel_Rbl, blankPlot, accel_Rfl, accel_Rbl, legend,
               ncol=3, nrow=2, widths=c(5, 5, 1), heights=c(3, 3))
    
    ggsave("Rlegs_vel_accel.pdf", Rlegs_vel_accel, width = 8, height = 8, units = "in")
    

#Stance, swing, stride data
    #Acceleration at pre and post wall jump for front leg
    stance_Rfl = ggplot(Rfl_stance_long, aes(x = variable, y = Rfl_stance, group=jump, shape=jump, colour = jump)) + 
        geom_line() +
        geom_point() +
        ggtitle("Front Leg Stance Time") + 
        labs(x="Step Number", y = "Stance Time (ms)")+
        theme_bw() + 
        theme(
            title = element_text(face="bold", colour=, size=14),
            axis.title.x = element_text(face="bold", colour=, size=12),
            axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
            axis.title.y = element_text(face="bold", colour=, size=12),
            axis.text.y  = element_text(angle=, vjust=, size=8),
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(angle=, vjust=, size=8),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "gray"),
            panel.grid.minor = element_blank(),
            aspect.ratio=(7/8)) +
        theme(axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black")) 
    
    stance_Rbl = ggplot(Rbl_stance_long, aes(x = variable, y = Rbl_stance, group=jump, shape=jump, colour = jump)) + 
        geom_line() +
        geom_point() +
        ggtitle("Back Leg Stance") + 
        labs(x="Step Number", y = "Stance Time (ms)")+
        theme_bw() + 
        theme(
            title = element_text(face="bold", colour=, size=14),
            axis.title.x = element_text(face="bold", colour=, size=12),
            axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
            axis.title.y = element_text(face="bold", colour=, size=12),
            axis.text.y  = element_text(angle=, vjust=, size=8),
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(angle=, vjust=, size=8),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "gray"),
            panel.grid.minor = element_blank(),
            aspect.ratio=(7/8)) +
        theme(axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black")) 

    swing_Rfl = ggplot(Rfl_swing_long, aes(x = variable, y = Rfl_swing, group=jump, shape=jump, colour = jump)) + 
        geom_line() +
        geom_point() +
        ggtitle("Front Leg swing") + 
        labs(x="Step Number", y = "Swing Time (ms)")+
        theme_bw() + 
        theme(
            title = element_text(face="bold", colour=, size=14),
            axis.title.x = element_text(face="bold", colour=, size=12),
            axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
            axis.title.y = element_text(face="bold", colour=, size=12),
            axis.text.y  = element_text(angle=, vjust=, size=8),
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(angle=, vjust=, size=8),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "gray"),
            panel.grid.minor = element_blank(),
            aspect.ratio=(7/8)) +
        theme(axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black")) 
    
    swing_Rbl = ggplot(Rbl_swing_long, aes(x = variable, y = Rbl_swing, group=jump, shape=jump, colour = jump)) + 
        geom_line() +
        geom_point() +
        ggtitle("Back Leg swing") + 
        labs(x="Step Number", y = "Swing Time (ms)")+
        theme_bw() + 
        theme(
            title = element_text(face="bold", colour=, size=14),
            axis.title.x = element_text(face="bold", colour=, size=12),
            axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
            axis.title.y = element_text(face="bold", colour=, size=12),
            axis.text.y  = element_text(angle=, vjust=, size=8),
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(angle=, vjust=, size=8),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "gray"),
            panel.grid.minor = element_blank(),
            aspect.ratio=(7/8)) +
        theme(axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black")) 

    stride_Rfl = ggplot(Rfl_stride_long, aes(x = variable, y = Rfl_stride, group=jump, shape=jump, colour = jump)) + 
        geom_line() +
        geom_point() +
        ggtitle("Front Leg stride") + 
        labs(x="Step Number", y = "Stride Length (cm)")+
        theme_bw() + 
        theme(
            title = element_text(face="bold", colour=, size=14),
            axis.title.x = element_text(face="bold", colour=, size=12),
            axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
            axis.title.y = element_text(face="bold", colour=, size=12),
            axis.text.y  = element_text(angle=, vjust=, size=8),
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(angle=, vjust=, size=8),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "gray"),
            panel.grid.minor = element_blank(),
            aspect.ratio=(7/8)) +
        theme(axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black")) 
    
    stride_Rbl = ggplot(Rbl_stride_long, aes(x = variable, y = Rbl_stride, group=jump, shape=jump, colour = jump)) + 
        geom_line() +
        geom_point() +
        ggtitle("Back Leg stride") + 
        labs(x="Step Number", y = "Stride Length (cm)")+
        theme_bw() + 
        theme(
            title = element_text(face="bold", colour=, size=14),
            axis.title.x = element_text(face="bold", colour=, size=12),
            axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
            axis.title.y = element_text(face="bold", colour=, size=12),
            axis.text.y  = element_text(angle=, vjust=, size=8),
            legend.title = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(angle=, vjust=, size=8),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "gray"),
            panel.grid.minor = element_blank(),
            aspect.ratio=(7/8)) +
        theme(axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black")) 
    
#getting a legend
    legend= get_legend(stride_Rbl)
    
# Removing all other legends now'
    stance_Rfl = stance_Rfl + theme(legend.position="none")
    swing_Rfl = swing_Rfl + theme(legend.position="none")
    stride_Rfl = stride_Rfl + theme(legend.position="none")
    stance_Rbl = stance_Rbl + theme(legend.position="none")
    swing_Rbl = swing_Rbl + theme(legend.position="none")
    stride_Rbl = stride_Rbl + theme(legend.position="none")


#Combine all graphs into a 3x2 grid. col 3 consists of 1 blank graph and the legend
    kinematics = grid.arrange(stance_Rfl, swing_Rfl, stride_Rfl, blankPlot, 
                              stance_Rbl, swing_Rbl, stride_Rbl, legend,
                                  ncol=4, nrow=2, widths=c(4,4,4, 1), heights=c(4,4))
    
    ggsave("kinematics.pdf", kinematics, width = 8, height = 6, units = "in")
    
    
#Plotting Vertical Error Using full data set against Trial
    trial_error_full = ggplot(jump_trial, aes(x = trial, y = vert_err_full, group=jump, shape=jump, colour = jump)) + 
        geom_line() +
        geom_point() +
        labs(x="Trial", y = "Vertical Error Full (cm)")+
        scale_y_continuous(minor_breaks = seq(-5 , 10, 1), breaks = seq(-4, 10, 2)) +
        scale_x_continuous(breaks = seq(0, 10, 1)) +
        theme_bw() + 
        theme(
            title = element_text(face="bold", colour=, size=14),
            axis.title.x = element_text(face="bold", colour=, size=12),
            axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
            axis.title.y = element_text(face="bold", colour=, size=12),
            axis.text.y  = element_text(angle=, vjust=, size=8),
            legend.title = element_blank(),
            legend.text = element_text(angle=, vjust=, size=8),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "gray"),
            panel.grid.minor = element_blank(),
            aspect.ratio=(7/8)) +
        theme(axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black")) 
    
    ggsave("trial_error_full.pdf", trial_error_full, width = 6, height = 4, units = "in")

#Plotting Vertical Error Pre wall jump Data against Trial
    trial_error_pre = ggplot(jump_trial, aes(x = trial, y = vert_err_pre, group=jump, shape=jump, colour = jump)) + 
        geom_line() +
        geom_point() +
        labs(x="Trial", y = "Est. Vertical Error using Pre Walljump data (cm)")+
        scale_y_continuous(minor_breaks = seq(-400 , 400, 50), breaks = seq(-500, 500, 100)) +
        scale_x_continuous(breaks = seq(0, 10, 1)) +
        theme_bw() + 
        theme(
            title = element_text(face="bold", colour=, size=14),
            axis.title.x = element_text(face="bold", colour=, size=12),
            axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
            axis.title.y = element_text(face="bold", colour=, size=12),
            axis.text.y  = element_text(angle=, vjust=, size=8),
            legend.title = element_blank(),
            legend.text = element_text(angle=, vjust=, size=8),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "gray"),
            panel.grid.minor = element_blank(),
            aspect.ratio=(7/8)) +
        theme(axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black")) 
    
    ggsave("trial_error_pre.pdf", trial_error_pre, width = 6, height = 4, units = "in")

#Plotting Post Vertical Error Etimates against Trial
    trial_error_post = ggplot(jump_trial, aes(x = trial, y = vert_err_post, group=jump, shape=jump, colour = jump)) + 
        geom_line() +
        geom_point() +
        labs(x="Trial", y = "Estimated Vertical Error using Pre Walljump data(cm)")+
        scale_y_continuous(minor_breaks = seq(-40 , 10, 1), breaks = seq(-40, 10, 10)) +
        scale_x_continuous(breaks = seq(0, 10, 1)) +
        theme_bw() + 
        theme(
            title = element_text(face="bold", colour=, size=14),
            axis.title.x = element_text(face="bold", colour=, size=12),
            axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
            axis.title.y = element_text(face="bold", colour=, size=12),
            axis.text.y  = element_text(angle=, vjust=, size=8),
            legend.title = element_blank(),
            legend.text = element_text(angle=, vjust=, size=8),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "gray"),
            panel.grid.minor = element_blank(),
            aspect.ratio=(7/8)) +
        theme(axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black")) 
    
    ggsave("trial_error_post.pdf", trial_error_post, width = 6, height = 4, units = "in")
    
    
#First contact with wall against Trial
    firstXY_100 = ggplot(subset(jump_trial, jump_trial$jump==100), 
                           aes(x = Rfl_touchX1, y = Rfl_touchY1, label=trial, group=factor(trial), 
                               colour = factor(trial))) + 
        geom_point() +
        geom_text(hjust=-1, vjust=-1, check_overlap = TRUE) + 
        ggtitle("100") + 
        labs(x="Distance from Goal (cm)", y = "Height (cm)")+
        ylim(0,15) +
        xlim(-50,-30) +
        theme_bw() + 
        theme(
            title = element_text(face="bold", colour=, size=14),
            axis.title.x = element_text(face="bold", colour=, size=12),
            axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
            axis.title.y = element_text(face="bold", colour=, size=12),
            axis.text.y  = element_text(angle=, vjust=, size=8),
            legend.title = element_blank(),
            legend.text = element_text(angle=, vjust=, size=8),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "gray"),
            panel.grid.minor = element_blank(),
            aspect.ratio=(7/8)) +
        theme(axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black")) 

    firstXY_100H = ggplot(subset(jump_trial, jump_trial$jump=="100H"), 
                          aes(x = Rfl_touchX1, y = Rfl_touchY1, label = trial, group=trial, colour = trial)) +
        geom_point() +
        ggtitle("100H") + 
        geom_text(hjust=-1, vjust=-1, check_overlap = TRUE) + 
        labs(x="Distance from Goal (cm)", y = "Height (cm)")+
        ylim(-15,15) +
        scale_x_continuous(breaks = seq(-150, 0, 10)) +
        theme_bw() + 
        theme(
            title = element_text(face="bold", colour=, size=14),
            axis.title.x = element_text(face="bold", colour=, size=12),
            axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
            axis.title.y = element_text(face="bold", colour=, size=12),
            axis.text.y  = element_text(angle=, vjust=, size=8),
            legend.title = element_blank(),
            legend.text = element_text(angle=, vjust=, size=8),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "gray"),
            panel.grid.minor = element_blank(),
            aspect.ratio=(7/8)) +
        theme(axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black")) 
    
    
    firstXY_100L = ggplot(subset(jump_trial, jump_trial$jump=="100L"), 
                          aes(x = Rfl_touchX1, y = Rfl_touchY1, label = trial, group=trial, colour = trial)) +
        geom_point() +
        ggtitle("100L") + 
        geom_text(hjust=-1, vjust=-1, check_overlap = TRUE) + 
        labs(x="Distance from Goal (cm)", y = "Height (cm)")+
        ylim(-15,15) +
        scale_x_continuous(breaks = seq(-150, 0, 10)) +
        theme_bw() + 
        theme(
            title = element_text(face="bold", colour=, size=14),
            axis.title.x = element_text(face="bold", colour=, size=12),
            axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
            axis.title.y = element_text(face="bold", colour=, size=12),
            axis.text.y  = element_text(angle=, vjust=, size=8),
            legend.title = element_blank(),
            legend.text = element_text(angle=, vjust=, size=8),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "gray"),
            panel.grid.minor = element_blank(),
            aspect.ratio=(7/8)) +
        theme(axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black")) 
    
    firstXY_150 = ggplot(subset(jump_trial, jump_trial$jump=="150"), 
                         aes(x = Rfl_touchX1, y = Rfl_touchY1, label = trial, group=trial, colour = trial)) +
        geom_point() +
        ggtitle("150") + 
        geom_text(hjust=-1, vjust=-1, check_overlap = TRUE) + 
        labs(x="Distance from Goal (cm)", y = "Height (cm)")+
        ylim(-15,15) +
        scale_x_continuous(breaks = seq(-150, 0, 10)) +
        theme_bw() + 
        theme(
            title = element_text(face="bold", colour=, size=14),
            axis.title.x = element_text(face="bold", colour=, size=12),
            axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
            axis.title.y = element_text(face="bold", colour=, size=12),
            axis.text.y  = element_text(angle=, vjust=, size=8),
            legend.title = element_blank(),
            legend.text = element_text(angle=, vjust=, size=8),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "gray"),
            panel.grid.minor = element_blank(),
            aspect.ratio=(7/8)) +
        theme(axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black")) 
    
    firstXY_150H = ggplot(subset(jump_trial, jump_trial$jump=="150H"), 
                          aes(x = Rfl_touchX1, y = Rfl_touchY1, label = trial, group=trial, colour = trial)) +
        geom_point() +
        ggtitle("150H") + 
        geom_text(hjust=-1, vjust=-1, check_overlap = TRUE) + 
        labs(x="Distance from Goal (cm)", y = "Height (cm)")+
        ylim(-15,15) +
        scale_x_continuous(breaks = seq(-150, 0, 10)) +
        theme_bw() + 
        theme(
            title = element_text(face="bold", colour=, size=14),
            axis.title.x = element_text(face="bold", colour=, size=12),
            axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
            axis.title.y = element_text(face="bold", colour=, size=12),
            axis.text.y  = element_text(angle=, vjust=, size=8),
            legend.title = element_blank(),
            legend.text = element_text(angle=, vjust=, size=8),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "gray"),
            panel.grid.minor = element_blank(),
            aspect.ratio=(7/8)) +
        theme(axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black")) 
    
    #getting a legend
    legend2 = get_legend(firstXY_100)
    
    # Removing all other legends now'
    firstXY_100 = firstXY_100 + theme(legend.position="none")
    firstXY_100H = firstXY_100H + theme(legend.position="none")
    firstXY_100L = firstXY_100L + theme(legend.position="none")
    firstXY_150 = firstXY_150 + theme(legend.position="none")
    firstXY_150H = firstXY_150H + theme(legend.position="none")
    
    #Combine all graphs into a 3x2 grid. col 3 consists of 1 blank graph and the legend
    trial_contact = grid.arrange(firstXY_100, firstXY_100H, firstXY_100L,
                                 firstXY_150, firstXY_150H, legend2,
                              ncol=3, nrow=2, widths=c(4,4,4), heights=c(4,4))
    
# All jumps for first contact
    all_jumps = ggplot(jump_trial, aes(x = Rfl_touchX1, y = Rfl_touchY1, shape=jump, group=jump, 
                             colour=jump)) + 
        geom_point() +
        geom_dl(aes(label=jump),method="smart.grid") +
        ggtitle("First Contact by Jump Distance and Trial") + 
        labs(x="Distance from Goal (cm)", y = "Height (cm)")+
        ylim(-15,30) +
        xlim(-100,10) +
        theme_bw() + 
        theme(
            title = element_text(face="bold", colour=, size=14),
            axis.title.x = element_text(face="bold", colour=, size=12),
            axis.text.x  = element_text(angle=, hjust=, vjust=, size=8),
            axis.title.y = element_text(face="bold", colour=, size=12),
            axis.text.y  = element_text(angle=, vjust=, size=8),
            legend.title = element_blank(),
            legend.text = element_text(angle=, vjust=, size=8),
            plot.background = element_blank(),
            panel.border = element_blank(),
            panel.grid.major = element_line(colour = "gray"),
            panel.grid.minor = element_blank(),
            aspect.ratio=(5/8)) +
        theme(axis.line.x = element_line(color="black"),
              axis.line.y = element_line(color="black")) 
    ggsave("all_jumps_contact.pdf", all_jumps, width = 8, height = 5, units = "in")
    
