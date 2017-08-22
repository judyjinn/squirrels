%% Clear

clc;
clear all;
close all;
tic

cd('/Users/judyjinn/MATLAB/walljumping/matfiles');

%% Notes
    %0 is going to mean a missing value or NaN, make sure to convert for whatever language gets used for stats.
    %Be careful about missing top file data. That will continue to happen until all files tracked
    %Time passed is probably roughly (1/120)*50 at most (50 frames, 120 frames per sec)
    %sometimes with multiple steps, no swing/stance value is recorded.

%%  Concatenate data from trials into a massive structure
% [sqrl_wj_struct] = sqrl_wj_concat(); %leave commented if you don't need to reconcatenate all the files

%% Set up fields for looping through the data
load('wall_jumping_struct_data.mat');

% If testing a simple data set, load Ringo.
% load('Ringo_side_150_1_data.mat'); %7,6,1 %test data for ringo, 150,trial 1
%     

% Organizing the names, jumps, and trial numbers to allow looping through the structure
names = {'Beyonce', 'Britney', 'Cash', 'Iggy', 'John', 'Quasi', 'Ringo','Stevie'};
jump = {'50', '50L', '100', '100L', '100H', '150', '150L', '150H'};
max_trials=10; %maximum jump trials performed

% weights = [869, 810, ???, 833, 680, ???, 730, ???]

%old struct names and field names for looping through
j_struct_names = {'j50', 'j50L', 'j100', 'j100L', 'j100H', 'j150', 'j150L', 'j150H'};
%fieldnames are just for reference. Never used.
fieldnames = {'COM_est_coeff', 'COM_est', 'COM_traj_coeff', 'COM_traj_coeff_pre','COM_traj_coeff_post',... 
    'noseX', 'noseY', 'bodymidX', 'bodymidY', 'buttX', ...
    'buttY', 'tailmidX', 'tailmidY', 'tailX', 'tailY'...%15 cells for side view
    'side_Rfl_touchdownF', 'side_Rfl_liftoffF', 'side_Rbl_touchdownF', 'side_Rbl_liftoffF', ...
    'top_Rfleg_td', 'top_Rfleg_lo', 'top_Lfleg_td', 'top_Lfleg_lo', ...
    'top_Rbleg_td', 'top_Rbleg_lo', 'top_Lbleg_td', 'top_Lbleg_lo', ...
    'Rfl_stance', 'Lfl_stance', 'Rbl_stance', 'Lbl_stance', ... 
    'Rfl_swing', 'Lfl_swing', 'Rbl_swing', 'Lbl_swing', ... 
    'Rfl_touchXY', 'Lbl_touchXY', 'lo_start_frame',... %22 cells for wj
    'num_Rfl_steps','num_Lfl_steps', 'num_Rbl_steps', 'num_Lbl_steps', ...
    'wj_start', 'wj_end', ... %6 doubles for wj
    'verticalErrorFull','VertErrPre', 'VertErrPost'}; %3 double for everything else

%% Overall trajectories

sqrl_pos_vel_accel = struct();

fieldnames2 = {'position', 'velocity', 'accleration', 'abs_vel', 'abs_accel'};
%loop through names and create a nested struct
    for n=1: length(names)
        sqrl_pos_vel_accel = setfield(sqrl_wj_struct, names{n},struct());
    end
    
    %the actual field names now of data we care about and want to analyze later in R or Python
    for n=1: length(names)
        for j=1: length(j_struct_names)
            %nest the 9 jump conditions into each squirrels' name
            sqrl_pos_vel_accel.(names{n}) = setfield(sqrl_pos_vel_accel.(names{n}), j_struct_names{j}, struct());

            for f=1:length(fieldnames2)        
                sqrl_pos_vel_accel.(names{n}).(j_struct_names{j})().(fieldnames2{f})(10,1)={[]};
            end
        end
    end

%% Wall jumping outputs
% New struct for loading in data
variables = {'name', 'jump', 'trial', ...
    'num_Rfl_steps','num_Lfl_steps', 'num_Rbl_steps', 'num_Lbl_steps', ...
    'vert_err_full', 'vert_err_pre', 'vert_err_post',  ... 
    'wj_time_start', 'wj_time_end', 'max_heightX', 'max_heightY',  ...
    'vel_start', 'pre_wj_vel', 'post_wj_vel', 'vel_end', ... 
    'accel_start', 'pre_wj_accel', 'post_wj_accel','accel_end',   ... 
    'vel_fl_prewj1', 'vel_fl_postwj1', 'accel_fl_prewj1', 'accel_fl_postwj1', ...
    'vel_bl_prewj1', 'vel_bl_postwj1', 'accel_bl_prewj1', 'accel_bl_postwj1', ...
    'vel_fl_prewj2', 'vel_fl_postwj2', 'accel_fl_prewj2', 'accel_fl_postwj2', ...
    'vel_bl_prewj2', 'vel_bl_postwj2', 'accel_bl_prewj2', 'accel_bl_postwj2', ...
    'vel_fl_prewj3', 'vel_fl_postwj3', 'accel_fl_prewj3', 'accel_fl_postwj3', ...
    'vel_bl_prewj3', 'vel_bl_postwj3', 'accel_bl_prewj3', 'accel_bl_postwj3', ....
    'vel_fl_prewj4', 'vel_fl_postwj4', 'accel_fl_prewj4', 'accel_fl_postwj4', ...
    'vel_bl_prewj4', 'vel_bl_postwj4', 'accel_bl_prewj4', 'accel_bl_postwj4'} ; 
    %don't really need to declare the stance-touch variables. They will self initialize. Highest num is 4 touches.

walljump_rawdata = struct();

% Need to set field because some trials have more wall touches than others so the table needs to record accurately.
for v=1:length(variables) 
    walljump_rawdata = setfield(walljump_rawdata, variables{v}, []);
end



%Put data into table (7,6,1 %test data for ringo, 150,trial 1 (1,3,1 beyonce errored trial))

% loop by names, then jumps, then trial number 
row=1; %need row count for each different trial# to organize all into a vertical table with proper label. Dumb, but works.
for n= 1:length(names)
    for j=1:length(jump)
        for trial=1:max_trials
            disp([names{n}, ' ', jump{j}, ' ', num2str(trial)]);
            
            %only get non 0 data by checking verticalError exists
            if (sqrl_wj_struct.(names{n}).(j_struct_names{j}).verticalErrorFull(trial) ~=0)


                %% Basic data from the squirrel struct
                walljump_rawdata(row).name = names{n};
                walljump_rawdata(row).jump = jump{j};
                walljump_rawdata(row).trial = trial;
                walljump_rawdata(row).vert_err_full = sqrl_wj_struct.(names{n}).(j_struct_names{j}).verticalErrorFull(trial);
                walljump_rawdata(row).vert_err_pre = sqrl_wj_struct.(names{n}).(j_struct_names{j}).VertErrPre(trial);
                walljump_rawdata(row).vert_err_post = sqrl_wj_struct.(names{n}).(j_struct_names{j}).VertErrPost(trial);
                walljump_rawdata(row).wj_time_start = ...
                    sqrl_wj_struct.(names{n}).(j_struct_names{j}).wj_start(trial)/60.0;
                walljump_rawdata(row).wj_time_end = ...
                    sqrl_wj_struct.(names{n}).(j_struct_names{j}).wj_end(trial)/60.0;
                
                
                %find the max height
                max_xy = max(sqrl_wj_struct.(names{n}).(j_struct_names{j}).COM_est{trial});
                walljump_rawdata(row).max_heightX=max_xy(1);
                walljump_rawdata(row).max_heightY=max_xy(2);
                
                    
                    
                %% Butterworth filtering for velocities
                COM_est = sqrl_wj_struct.(names{n}).(j_struct_names{j}).COM_est{trial};
                side_Rfl_touchdownF = sqrl_wj_struct.(names{n}).(j_struct_names{j}).side_Rfl_touchdownF{trial};
                side_Rfl_liftoffF = sqrl_wj_struct.(names{n}).(j_struct_names{j}).side_Rfl_liftoffF{trial};
                side_Rbl_touchdownF = sqrl_wj_struct.(names{n}).(j_struct_names{j}).side_Rbl_touchdownF{trial};
                side_Rbl_liftoffF = sqrl_wj_struct.(names{n}).(j_struct_names{j}).side_Rbl_liftoffF{trial};
                
                lo_start_frame = sqrl_wj_struct.(names{n}).(j_struct_names{j}).lo_start_frame{trial};
                
%                 disp(['trial length ', num2str(length(COM_est)-lo_start_frame)]);
                if (length(COM_est)-lo_start_frame < 6)
                    walljump_rawdata(row).vel_start = 0;
                    walljump_rawdata(row).pre_wj_vel = 0;
                    walljump_rawdata(row).post_wj_vel = 0;
                    walljump_rawdata(row).vel_end = 0;

                    walljump_rawdata(row).accel_start = 0;
                    walljump_rawdata(row).pre_wj_accel = 0;
                    walljump_rawdata(row).post_wj_accel = 0;
                    walljump_rawdata(row).accel_end = 0;
                else
                
                    [start_vel,  pre_wj_vel, post_wj_vel, end_vel, ...
                     pre_fl_wj_vel, post_fl_wj_vel, pre_bl_wj_vel, post_bl_wj_vel, ... 
                     start_accel, pre_wj_accel, post_wj_accel, end_accel,  ...
                     pre_fl_wj_accel, post_fl_wj_accel, pre_bl_wj_accel, post_bl_wj_accel, ...
                     COM_butter, vel, accel, abs_vel, abs_accel] ...
                    = sqrl_vel_accel(COM_est, side_Rfl_touchdownF, side_Rfl_liftoffF, ...
                                    side_Rbl_touchdownF, side_Rbl_liftoffF, lo_start_frame);

                    walljump_rawdata(row).vel_start = start_vel;
                    walljump_rawdata(row).pre_wj_vel = pre_wj_vel;
                    walljump_rawdata(row).post_wj_vel = post_wj_vel;
                    walljump_rawdata(row).vel_end = end_vel;

                    walljump_rawdata(row).accel_start = start_accel;
                    walljump_rawdata(row).pre_wj_accel = pre_wj_accel;
                    walljump_rawdata(row).post_wj_accel = post_wj_accel;
                    walljump_rawdata(row).accel_end = end_accel;
                    
          
                    sqrl_pos_vel_accel.(names{n}).(j_struct_names{j}).position(trial) = {COM_butter};
                    sqrl_pos_vel_accel.(names{n}).(j_struct_names{j}).velocity(trial) = {vel};
                    sqrl_pos_vel_accel.(names{n}).(j_struct_names{j}).accleration(trial) = {accel};
                    sqrl_pos_vel_accel.(names{n}).(j_struct_names{j}).abs_vel(trial) = {abs_vel};
                    sqrl_pos_vel_accel.(names{n}).(j_struct_names{j}).abs_accel(trial) = {abs_accel};
                end
              
                %% WJ steps and stances
                num_Rfl_steps = sqrl_wj_struct.(names{n}).(j_struct_names{j}).num_Rfl_steps(trial);
                walljump_rawdata(row).num_Rfl_steps = num_Rfl_steps;
                
                if (num_Rfl_steps~=0) %if there were any wj steps then get data
                    %use number of fl steps to get the adequate number of wj variables (swing, stance, xy)
                    
                    for i=1:num_Rfl_steps
                        %get the Rfl_touchX
                        walljump_rawdata(row).(strcat('Rfl_touchX',num2str(i)))... 
                        =sqrl_wj_struct.(names{n}).(j_struct_names{j}).Rfl_touchXY{trial}(i,1);

                         %get the fl_touchY
                        walljump_rawdata(row).(strcat('Rfl_touchY',num2str(i)))... 
                        =sqrl_wj_struct.(names{n}).(j_struct_names{j}).Rfl_touchXY{trial}(i,2);
                    
                        %get the fl_stances
                        walljump_rawdata(row).(strcat('Rfl_stance',num2str(i)))... 
                        =sqrl_wj_struct.(names{n}).(j_struct_names{j}).Rfl_stance{trial}(i);
                    
                        walljump_rawdata(row).(strcat('vel_fl_prewj',num2str(i))) = pre_fl_wj_vel(i);
                        walljump_rawdata(row).(strcat('vel_fl_postwj',num2str(i))) = post_fl_wj_vel(i);
                        walljump_rawdata(row).(strcat('accel_fl_prewj',num2str(i))) = pre_fl_wj_accel(i);
                        walljump_rawdata(row).(strcat('accel_fl_postwj',num2str(i))) = post_fl_wj_accel(i);
                        
                        walljump_rawdata(row).(strcat('top_Rfleg_td',num2str(i))) = sqrl_wj_struct.(names{n}).(j_struct_names{j}).top_Rfleg_td{trial}(i);
                        walljump_rawdata(row).(strcat('top_Rfleg_lo',num2str(i))) = sqrl_wj_struct.(names{n}).(j_struct_names{j}).top_Rfleg_lo{trial}(i);
                    end
                    
                    for i=1:(num_Rfl_steps-1) %because there's always 1 less swing/stance than there are steps
                        %get the Rfl_swing
                        walljump_rawdata(row).(strcat('Rfl_swing',num2str(i)))... 
                        =sqrl_wj_struct.(names{n}).(j_struct_names{j}).Rfl_swing{trial}(i);
                    end
                   

                end
                
                num_Lfl_steps = sqrl_wj_struct.(names{n}).(j_struct_names{j}).num_Lfl_steps(trial);
                walljump_rawdata(row).num_Lfl_steps = num_Lfl_steps;
                
                
                if (num_Lfl_steps~=0) %if there were any wj steps then get data
                    
                    %use number of left fl steps to get the adequate number of wj variables (swing, stance, xy)
                    for i=1:num_Lfl_steps
                        %get the Lfl_stances
                        walljump_rawdata(row).(strcat('Lfl_stance',num2str(i)))... 
                        =sqrl_wj_struct.(names{n}).(j_struct_names{j}).Lfl_stance{trial}(i);
                        walljump_rawdata(row).(strcat('top_Lfleg_td',num2str(i))) = sqrl_wj_struct.(names{n}).(j_struct_names{j}).top_Lfleg_td{trial}(i);
                        walljump_rawdata(row).(strcat('top_Lfleg_lo',num2str(i))) = sqrl_wj_struct.(names{n}).(j_struct_names{j}).top_Lfleg_lo{trial}(i);
                    end
                    
                    for i=1:(num_Lfl_steps-1) %because there's always 1 less swing/stance than there are steps
                        %get the Lfl_swing
                        walljump_rawdata(row).(strcat('Lfl_swing',num2str(i)))... 
                        =sqrl_wj_struct.(names{n}).(j_struct_names{j}).Lfl_swing{trial}(i);
                    end

                end

                %Back legs
                num_Rbl_steps = sqrl_wj_struct.(names{n}).(j_struct_names{j}).num_Rbl_steps(trial);
                walljump_rawdata(row).num_Rbl_steps = num_Rbl_steps;
                                
                if (num_Rbl_steps~=0) %if there were any wj steps then get data                    
                    
                    %use number of fl steps to get the adequate number of wj variables (swing, stance, xy)
                    for i=1:num_Rbl_steps
                        %get the bl_touchX
                        walljump_rawdata(row).(strcat('Rbl_touchX',num2str(i)))... 
                        =sqrl_wj_struct.(names{n}).(j_struct_names{j}).Rbl_touchXY{trial}(i,1);

                         %get the bl_touchY
                        walljump_rawdata(row).(strcat('Rbl_touchY',num2str(i)))... 
                        =sqrl_wj_struct.(names{n}).(j_struct_names{j}).Rbl_touchXY{trial}(i,2);
                    
                       %get the bl_stances
                        walljump_rawdata(row).(strcat('Rbl_stance',num2str(i)))... 
                        =sqrl_wj_struct.(names{n}).(j_struct_names{j}).Rbl_stance{trial}(i);
                    
                        walljump_rawdata(row).(strcat('vel_bl_prewj',num2str(i))) = pre_bl_wj_vel(i);
                        walljump_rawdata(row).(strcat('vel_bl_postwj',num2str(i))) = post_bl_wj_vel(i);
                        walljump_rawdata(row).(strcat('accel_bl_prewj',num2str(i))) = pre_bl_wj_accel(i);
                        walljump_rawdata(row).(strcat('accel_bl_postwj',num2str(i))) = post_bl_wj_accel(i);
                        
                        walljump_rawdata(row).(strcat('top_Rbleg_td',num2str(i))) = sqrl_wj_struct.(names{n}).(j_struct_names{j}).top_Rbleg_td{trial}(i);
                        walljump_rawdata(row).(strcat('top_Rbleg_lo',num2str(i))) = sqrl_wj_struct.(names{n}).(j_struct_names{j}).top_Rbleg_lo{trial}(i);
                    end
                    
                    for i=1:(num_Rbl_steps-1) %because there's always 1 less swing/stance than there are steps
                        %get the bl_swing
                        walljump_rawdata(row).(strcat('Rbl_swing',num2str(i)))... 
                        =sqrl_wj_struct.(names{n}).(j_struct_names{j}).Rbl_swing{trial}(i);
                    end
                end
                    
                num_Lbl_steps = sqrl_wj_struct.(names{n}).(j_struct_names{j}).num_Lbl_steps(trial);
                walljump_rawdata(row).num_Lbl_steps = num_Lbl_steps;
                                
                if (num_Lbl_steps~=0) %if there were any wj steps then get data
         
                    
                    %use number of fl steps to get the adequate number of wj variables (swing, stance, xy)
                    for i=1:num_Lbl_steps
                       %get the bl_stances
                        walljump_rawdata(row).(strcat('Lbl_stance',num2str(i)))... 
                        =sqrl_wj_struct.(names{n}).(j_struct_names{j}).Lbl_stance{trial}(i);
                    
                        walljump_rawdata(row).(strcat('top_Lbleg_td',num2str(i))) = sqrl_wj_struct.(names{n}).(j_struct_names{j}).top_Lbleg_td{trial}(i);
                        walljump_rawdata(row).(strcat('top_Lbleg_lo',num2str(i))) = sqrl_wj_struct.(names{n}).(j_struct_names{j}).top_Lbleg_lo{trial}(i);
                   
                    end
                    
                    for i=1:(num_Lbl_steps-1) %because there's always 1 less swing/stance than there are steps
                        %get the bl_swing
                        walljump_rawdata(row).(strcat('Lbl_swing',num2str(i)))... 
                        =sqrl_wj_struct.(names{n}).(j_struct_names{j}).Lbl_swing{trial}(i);
                    end                                                    
                end
            
                row=row+1;
            end
 
        end
    end
end

%% get rid of all 0s for variables which are related to walljumps if no wall jump occured
% If any variables (columns) are ever added, make sure to check the for loop ranges to ensure they line up still

for v=4:7 %search through a few of the columns for 0s
    for i=1:length(walljump_rawdata)
       if (walljump_rawdata(i).(variables{v}) == 0)
           walljump_rawdata(i).(variables{v}) = [];
       end
    end
end
%Skip overthe vertical errors because 0 means a perfect landing, not a missing value.
for v=8:11 %search through a few of the columns for -100s
    for i=1:length(walljump_rawdata)
       if (walljump_rawdata(i).(variables{v}) == -100)
           walljump_rawdata(i).(variables{v}) = [];
       end
    end
end

for v=11:length(variables) %search through a few of the columns for 0s
    for i=1:length(walljump_rawdata)
       if (walljump_rawdata(i).(variables{v}) == 0)
           walljump_rawdata(i).(variables{v}) = [];
       end
    end
end

% convert names and jumps to categorical variables and save
walljump_rawdata = struct2table(walljump_rawdata);
walljump_rawdata.name = categorical(walljump_rawdata.name);
walljump_rawdata.jump = categorical(walljump_rawdata.jump);
writetable(walljump_rawdata,'walljump_rawdata.csv','Delimiter',',');

save('wj_sqrl_pos_vel_accel', 'sqrl_pos_vel_accel');
