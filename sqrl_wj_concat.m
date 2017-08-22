function [sqrl_wj_struct] = sqrl_wj_concat() %comment out this function if you don't want to have to run it from the other file
    %% Clear

    clc;
    clear all;
    close all;
    tic


    %% NOTE TO SELF: JOHN_150_5 IS A MISS. HE DOES NOT MAKE THE JUMP. DO NOT STANDARDIZE HIS DATA FOR DISTANCE


    %% Set up 

    % create new file for buggy files
    file_report = fopen('filereport.txt', 'w');

    %load the wall jump touch down, lift off frame numbers
    wj_frames = readtable('WallJumpingFrames.csv','Format','%s%u%u%u');
    
    %load the frame numbers for initial lift off from start platform
    lo_start = readtable('footliftoff.csv','Format','%s%u'); 


    %Organizing the names, view, jumps, and trial numbers to allow looping through the .mat files
    names = {'Beyonce', 'Britney', 'Cash', 'Iggy', 'John', 'Quasi', 'Ringo','Stevie'};
    jump = {'50', '50L', '100', '100L', '100H', '150', '150L', '150H'};
    max_trials=10; %maximum jump trials performed

    %for counting purposes to ensure all files have been accounted for. 
    missingside=0;
    missingtop=0;
    missingwj=0;
    emptytrial=0;
    missingsidewj=0;
    missingboth=0;
    jump_num=0;
    wj_num=0;


    %% Create a struct to store relevant data to use for analysis within MATLAB

    %struct fields can't be numbers, give them a new name
    wj_struct_names = {'j50', 'j50L', 'j100', 'j100L', 'j100H', 'j150', 'j150L', 'j150H'};
    fieldnames = {'COM_est_coeff', 'COM_est', 'COM_traj_coeff', 'COM_traj_coeff_pre','COM_traj_coeff_post',... 
    'noseX', 'noseY', 'bodymidX', 'bodymidY', 'buttX', ...
    'buttY', 'tailmidX', 'tailmidY', 'tailX', 'tailY', ...%15 cells for side view
    'side_Rfl_touchdownF', 'side_Rfl_liftoffF', 'side_Rbl_touchdownF', 'side_Rbl_liftoffF', ...
    'side_Lfl_touchdownF', 'side_Lfl_liftoffF', 'side_Lbl_touchdownF', 'side_Lbl_liftoffF', ...
    'top_Rfleg_td', 'top_Rfleg_lo', 'top_Lfleg_td', 'top_Lfleg_lo', ...
    'top_Rbleg_td', 'top_Rbleg_lo', 'top_Lbleg_td', 'top_Lbleg_lo', ...
    'Rfl_stance', 'Lfl_stance', 'Rbl_stance', 'Lbl_stance', ... 
    'Rfl_swing', 'Lfl_swing', 'Rbl_swing', 'Lbl_swing', ... 
    'Rfl_touchXY', 'Rbl_touchXY', 'lo_start_frame', ... %27 cells for wj
    'num_Rfl_steps','num_Lfl_steps', 'num_Rbl_steps', 'num_Lbl_steps', ...
    'wj_start', 'wj_end', ... %6 doubles for wj
    'verticalErrorFull','VertErrPre', 'VertErrPost'}; %3 double for everything else
    side_cells=15;
    wj_cells = 27;
    wj_doubles = 6;
    doubles=3; 

    %starting struct
    sqrl_wj_struct = struct(); 

    %loop through names and create a nested struct
    for n=1: length(names)
        sqrl_wj_struct = setfield(sqrl_wj_struct, names{n},struct());
    end

    %create another nested layer for the jumps which includes..
    %the actual field names now of data we care about and want to analyze later in R or Python
    for n=1: length(names)
        for j=1: length(wj_struct_names)
            %nest the 9 jump conditions into each squirrels' name
            sqrl_wj_struct.(names{n}) = setfield(sqrl_wj_struct.(names{n}), wj_struct_names{j}, struct());

            for f=1:length(fieldnames)        
                if (f<=(length(fieldnames)-(wj_doubles+doubles))) %for only cell arrays
        %             disp(['creating ',fieldnames{f}]);
                    sqrl_wj_struct.(names{n}).(wj_struct_names{j})().(fieldnames{f})(10,1)={[]};
                else
                    %Within each jump condition, enter the rest of field names as blank double arrays
        %             disp(['creating ',fieldnames{f}]);
                    sqrl_wj_struct.(names{n}).(wj_struct_names{j})().(fieldnames{f})=[];
                end
            end

        end
    end



%% loop through aforementioned file names to gather data in sqrl_wj_struct
    % Note May 6,2016: This section looks for side view files first, then top.
    % If missing a top view, it will appear that the file was not a wall jump in the data, even if false
    % There is a checker for these mismatched files to print out the discrepencies, 
    % but be aware the data may not be 100% right now if used for data analysis.

    
    %initiating values
    sqrl_file_top = 'a';
    
    %loop by names, then jumps, then trial number
     %7,6,1 %test data for ringo, 150,trial 1 (1,3,1 beyonce errored trial)
    for n= 1:length(names)
        disp(names{n}); %just for something to watch so you know how far the code is
        for j= 1:length(jump)
            for trial= 1:max_trials

                %Used for counting missing data
                side_open=0;
                top_open=0;
                wall_jump=0;
                sqrl_file_side = '';
                sqrl_file_top = '';

            % Side view files opened first. If successful, THEN attemps to open a top file
                % Attempt to open file

                sqrl_file_side = strcat(names{n}, '_side_', jump{j}, '_', num2str(trial), '_data.mat');
                if (exist(sqrl_file_side)>0)
                    side_struct = load(sqrl_file_side);
                    
                    jumpname=jump{j}; %needed to pass name to sqrl_Trot for standardizing distances

                    %Converts the file into a table then translates and rotates points
                    [Trot_X, Trot_Y, Trot_butt, Trot_nose, rotrad_M, bodymidX, bodymidY, ...
                    noseX, noseY, buttX, buttY, sidetable] = sqrl_Trot(side_struct.squirrelStructure, jumpname);

                    
                    %If the code is successful thus far, then side file exists
                    %Now need to check if there's any top view data to use for breaking COM estimates into pre and post

                    
                    %% Top view 
                    % Attempt to open top view files

%                         sqrl_file_top = strcat(names{n}, '_top_', jump{j}, '_', num2str(trial), '_walljumpdata.mat');
%                         % only for 2 limbs

                        % Use following line of code to analyze 4 limbs
                    sqrl_file_top = strcat(names{n}, '_top_', jump{j}, '_', num2str(trial), '_top4data.mat');
                    if (exist(sqrl_file_top)>0)
                         top_struct = load(sqrl_file_top);


                        %Get values for wall jump related metrics
                        [toptable, num_Rfl_steps, num_Lfl_steps, num_Rbl_steps, num_Lbl_steps, ...
                        side_Rfleg_td, side_Rfleg_lo, side_Rbleg_td, side_Rbleg_lo, ...
                        side_Lfleg_td, side_Lfleg_lo, side_Lbleg_td, side_Lbleg_lo, ...
                        top_Rfleg_td, top_Rfleg_lo, top_Lfleg_td, top_Lfleg_lo, ...
                        top_Rbleg_td, top_Rbleg_lo, top_Lbleg_td, top_Lbleg_lo, ...
                        Rfl_stance, Lfl_stance, Rbl_stance, Lbl_stance, Rfl_swing, Lfl_swing, ...
                        Rbl_swing, Lbl_swing, Rfl_touchXY, Rbl_touchXY, wj_start, wj_end] ...
                        = sqrl_td_lo_4(top_struct, sidetable);


                        %Store values in struct
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).side_Rfl_touchdownF(trial) = {side_Rfleg_td};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).side_Rfl_liftoffF(trial) = {side_Rfleg_lo};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).side_Rbl_touchdownF(trial) = {side_Rbleg_td};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).side_Rbl_liftoffF(trial) = {side_Rbleg_lo};

                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).side_Lfl_touchdownF(trial) = {side_Lfleg_td};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).side_Lfl_liftoffF(trial) = {side_Lfleg_lo};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).side_Lbl_touchdownF(trial) = {side_Lbleg_td};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).side_Lbl_liftoffF(trial) = {side_Lbleg_lo};

                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).top_Rfleg_td(trial) = {top_Rfleg_td};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).top_Rfleg_lo(trial) = {top_Rfleg_lo};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).top_Lfleg_td(trial) = {top_Lfleg_td};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).top_Lfleg_lo(trial) = {top_Lfleg_lo};

                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).top_Rbleg_td(trial) = {top_Rbleg_td};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).top_Rbleg_lo(trial) = {top_Rbleg_lo};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).top_Lbleg_td(trial) = {top_Lbleg_td};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).top_Lbleg_lo(trial) = {top_Lbleg_lo};

                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Rfl_touchXY(trial)={Rfl_touchXY};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Rbl_touchXY(trial)={Rbl_touchXY};

                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).num_Rfl_steps(end+1) = num_Rfl_steps;
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).num_Lfl_steps(end+1) = num_Lfl_steps;
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).num_Rbl_steps(end+1) = num_Rbl_steps;
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).num_Lbl_steps(end+1) = num_Lbl_steps;
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).wj_start(end+1) = wj_start;
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).wj_end(end+1) = wj_end;                                                        

                        %Check if there's something to store, else put it in as 0
                        if (isempty(Rfl_stance)==0) 
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Rfl_stance(trial) = {Rfl_stance};
                        else
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Rfl_stance(trial) = {0};
                        end
                        if (isempty(Lfl_stance)==0) 
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Lfl_stance(trial) = {Lfl_stance};
                        else
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Lfl_stance(trial) = {0};
                        end
                        if (isempty(Rbl_stance)==0) 
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Rbl_stance(trial) = {Rbl_stance};
                        else
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Rbl_stance(trial) = {0};
                        end
                        if (isempty(Lbl_stance)==0) 
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Lbl_stance(trial) = {Lbl_stance};
                        else
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Lbl_stance(trial) = {0};
                        end
                        if (isempty(Rfl_swing)==0) 
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Rfl_swing(trial) = {Rfl_swing};
                        else
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Rfl_swing(trial) = {0};
                        end
                        if (isempty(Lfl_swing)==0) 
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Lfl_swing(trial) = {Lfl_swing};
                        else
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Lfl_swing(trial) = {0};
                        end
                        if (isempty(Rbl_swing)==0) 
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Rbl_swing(trial) = {Rbl_swing};
                        else
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Rbl_swing(trial) = {0};
                        end
                        if (isempty(Lbl_swing)==0) 
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Lbl_swing(trial) = {Lbl_swing};
                        else
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j}).Lbl_swing(trial) = {0};
                        end


                        %This is for finding the foot lift off frame number
                        side_name = strcat(names{n}, '_side_', jump{j}, '_', num2str(trial));
                        for i=1:length(lo_start.name)
                            if (strcmp(lo_start.name(i),side_name)==1)
                                lo_start_frame = lo_start.foot_lo(i);
                                if lo_start.foot_lo(i)==0
                                    disp(['Foot lift off frame missing for ', side_name]);
                                    temp_str = ['\nFoot lift off frame missing for ', side_name,'\n'];
                                    fprintf(file_report, temp_str);

                                end
                            end
                        end


                        %find the COM for the squirrel
                        [COM_est_coeff, COM_est, COM_traj_coeff,COM_traj_coeff_pre,COM_traj_coeff_post, ...
                        verticalErrorFull, VertErrPre, VertErrPost, ...
                        tailX, tailY, tailmidX, tailmidY] = ...
                        sqrl_COM(Trot_X, Trot_Y, Trot_butt, Trot_nose, ...
                        rotrad_M, bodymidX, bodymidY, noseX, noseY, buttX, buttY, lo_start_frame, sidetable, ...
                        wj_start, wj_end);


                        %store values in the data struct                        
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).COM_est_coeff(trial) = {COM_est_coeff};

                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).COM_est(trial) = {COM_est};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).COM_traj_coeff(trial) = {COM_traj_coeff};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).COM_traj_coeff_pre(trial) = {COM_traj_coeff_pre};  
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).COM_traj_coeff_post(trial) = {COM_traj_coeff_post};  

                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).noseX(trial) = {noseX};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).noseY(trial) = {noseY};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).bodymidX(trial) = {bodymidX};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).bodymidY(trial) = {bodymidY};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).buttX(trial) = {buttX};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).buttY(trial) = {buttY};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).tailmidX(trial) = {tailmidX};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).tailmidY(trial) = {tailmidY};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).tailX(trial) = {tailX};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).tailY(trial) = {tailY};

                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).verticalErrorFull(end+1) = verticalErrorFull;
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).VertErrPre(end+1) = VertErrPre;
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).VertErrPost(end+1) = VertErrPost;

                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).lo_start_frame(trial) = {lo_start_frame};


                    % If top file does not open, set all pertaining to touch down lift off and foot positions to 0
                    else
%                         disp(['failed ', sqrl_file_top]);
                        for f=(side_cells+wj_cells):(length(fieldnames)-doubles)
                            if (f<=(side_cells+wj_cells)) %just the cell arrays
                                sqrl_wj_struct.(names{n}).(wj_struct_names{j}).(fieldnames{f})(trial)={0};
                            else
                                sqrl_wj_struct.(names{n}).(wj_struct_names{j})().(fieldnames{f})(end+1)=0;
                            end
                        end
                        
                        
                        
                       %This is for finding the foot lift off frame number
                        side_name = strcat(names{n}, '_side_', jump{j}, '_', num2str(trial));
                        for i=1:length(lo_start.name)
                            if (strcmp(lo_start.name(i),side_name)==1)
                                lo_start_frame = lo_start.foot_lo(i);
                                if lo_start.foot_lo(i)==0
                                    disp(['Foot lift off frame missing for ', side_name]);
                                    temp_str = ['\nFoot lift off frame missing for ', side_name,'\n'];
                                    fprintf(file_report, temp_str);
                                end
                            end
                        end                        
                      
                        side_fleg_td = 0; 
                        side_bleg_lo = 0;
                        
                        %find the COM for the squirrel based on its non wall jump 
                        [COM_est_coeff, COM_est, COM_traj_coeff,COM_traj_coeff_pre,COM_traj_coeff_post, ...
                        verticalErrorFull, VertErrPre, VertErrPost, ...
                        tailX, tailY, tailmidX, tailmidY] = ...
                        sqrl_COM(Trot_X, Trot_Y, Trot_butt, Trot_nose, ...
                        rotrad_M, bodymidX, bodymidY, noseX, noseY, buttX, buttY, lo_start_frame, sidetable, ...
                        side_fleg_td,side_bleg_lo);
                    
                        %store values in the data struct
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).COM_est_coeff(trial) = {COM_est_coeff};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).COM_est(trial) = {COM_est};
                        
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).COM_traj_coeff(trial) = {COM_traj_coeff};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).COM_traj_coeff_pre(trial) = {COM_traj_coeff_pre};  
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).COM_traj_coeff_post(trial) = {COM_traj_coeff_post}; 

                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).noseX(trial) = {noseX};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).noseY(trial) = {noseY};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).bodymidX(trial) = {bodymidX};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).bodymidY(trial) = {bodymidY};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).buttX(trial) = {buttX};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).buttY(trial) = {buttY};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).tailmidX(trial) = {tailmidX};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).tailmidY(trial) = {tailmidY};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).tailX(trial) = {tailX};
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).tailY(trial) = {tailY};
                        
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).verticalErrorFull(end+1,:) = verticalErrorFull;
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).VertErrPre(end+1,:) = VertErrPre;
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).VertErrPost(end+1,:) = VertErrPost;
                        
                        sqrl_wj_struct.(names{n}).(wj_struct_names{j}).lo_start_frame(trial) = {lo_start_frame};
                        

                    end % exist(top_file)           
                    
                

                % If side file does not exist, set all value to 0                   
                else
                    for f=1:length(fieldnames)
                        if (f<=(side_cells+wj_cells)) %stop before doubles
%                             disp(fieldnames{f});
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j})().(fieldnames{f})(trial)={0};  
                        else % for all doubles, put in 0s.
                            sqrl_wj_struct.(names{n}).(wj_struct_names{j})().(fieldnames{f})(end+1)=0;
                        end
                    end
                end %exist side_file

            
            %% Checks for missing data and for the total number of files checked
                %check if side file exists
                if (exist(sqrl_file_side,'file')==2)
                    side_open = 1;
                end                
                
                %check if top file exists
                if (exist(sqrl_file_top,'file')==2)
                    top_open=1;
                end

                %This is to count if we're missing some wall jumping data 
                top_name = strcat(names{n}, '_top_', jump{j}, '_', num2str(trial));
                for i=1:length(wj_frames.name)
                    if (strcmp(wj_frames.name(i),top_name)==1)
                        if (wj_frames.touch(i)~=0)
                            wall_jump=1;
                        else
                            wall_jump=0;
                        end 
                    end
                end

                
                
                filecheck = [side_open, top_open, wall_jump];
                if (isequal(filecheck,[1,0,0]))
                    disp([names{n}, ' ', jump{j}, ' ', num2str(trial), ': side data exists, no wall jump occurred']);
                    temp_str = ['\n', names{n}, ' ', jump{j}, ' ', num2str(trial), ': side data exists, no wall jump occurred' ,'\n'];
                    fprintf(file_report, temp_str);
                    jump_num=jump_num+1;
                elseif (isequal(filecheck,[1,1,0]))
                    disp(['missing wall jump frames: ', sqrl_file_top])
                    temp_str = ['\n','missing wall jump frames: ', sqrl_file_top ,'\n'];
                    fprintf(file_report, temp_str);
                    missingwj=missingwj+1;
                elseif (isequal(filecheck,[1,0,1]))
                    disp(['missing top jump file: ', sqrl_file_top])
                    temp_str = ['\n','missing top jump file: ', sqrl_file_top ,'\n'];
                    fprintf(file_report, temp_str);
                    missingtop=missingtop+1;
                elseif (isequal(filecheck,[0,1,1]))
                    disp(['missing side file: ', sqrl_file_side]);
                    temp_str = ['\n', 'missing side file: ', sqrl_file_side ,'\n'];
                    fprintf(file_report, temp_str);
                    missingside=missingside+1;
                elseif (isequal(filecheck,[0,0,1]))
                    disp(['missing side and top files, have walljump numbers: ', sqrl_file_top,' & ', sqrl_file_side]);
                    temp_str = ['\n', 'missing side and top files, have walljump numbers: ', sqrl_file_top,' & ', sqrl_file_side ,'\n'];
                    fprintf(file_report, temp_str);
                    missingboth=missingboth+1;
                elseif (isequal(filecheck,[0,0,0]))
%                     disp(['no data for : ', sqrl_file_top,' & ', sqrl_file_side]);
                    emptytrial=emptytrial+1;
                elseif (isequal(filecheck,[0,1,0]))
                    disp(['missing side files and wall jump numbers: ', sqrl_file_side]);
                    temp_str = ['\n', 'missing side files and wall jump numbers: ', sqrl_file_side ,'\n'];
                    fprintf(file_report, temp_str);
                    missingsidewj=missingsidewj+1;
                elseif (isequal(filecheck,[1,1,1]))
                    disp([names{n}, ' ', jump{j}, ' ', num2str(trial), ': all data found']);
                    temp_str = ['\n', names{n}, ' ', jump{j}, ' ', num2str(trial), ': all data found' ,'\n'];
                    fprintf(file_report, temp_str);
                    wj_num=wj_num+1;

                end

            end
        end
    end
    
    totaltrials=jump_num+wj_num; 
    totalfileschecked=wj_num+missingsidewj+emptytrial+missingboth+missingside+missingtop+missingwj+jump_num;
    disp(['totaltrials: ', num2str(totaltrials), ' total files checked: ', num2str(totalfileschecked)]);
    
    fclose(file_report);

    save('wall_jumping_struct_data', 'sqrl_wj_struct');
    
    toc
end
