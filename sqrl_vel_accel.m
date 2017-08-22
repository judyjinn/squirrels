function [start_vel,  pre_wj_vel, post_wj_vel, end_vel, ...
    pre_fl_wj_vel, post_fl_wj_vel, pre_bl_wj_vel, post_bl_wj_vel, ... 
    start_accel, pre_wj_accel, post_wj_accel, end_accel,  ...
    pre_fl_wj_accel, post_fl_wj_accel, pre_bl_wj_accel, post_bl_wj_accel, ...
    COM_butter, vel, accel, abs_vel, abs_accel]  ... 
    = sqrl_vel_accel(COM_est, side_Rfl_touchdownF, side_Rfl_liftoffF, ... 
                    side_Rbl_touchdownF, side_Rbl_liftoffF, lo_start_frame)

    %% note to self: find optimal cutoff frequency

        %% butterworth filter
format long       
        
        %Uncomment section for testing individuals (comment out next block)
        % 7,6,1 %test data for ringo, 150,trial 1
%         disp('Calculating only one individual in vel_accel');
%         load('wall_jumping_struct_data.mat');
%         COM_est = sqrl_wj_struct.Beyonce.j100.COM_est{1};
% 
%         fl_td = (sqrl_wj_struct.Beyonce.j100.side_Rfl_touchdownF{1});
%         fl_lo = (sqrl_wj_struct.Beyonce.j100.side_Rfl_liftoffF{1});
%         bl_td = (sqrl_wj_struct.Beyonce.j100.side_Rbl_touchdownF{1});
%         bl_lo = (sqrl_wj_struct.Beyonce.j100.side_Rbl_liftoffF{1});
%         lo_start_frame = 6;
        

        %Uncomment/comment section for running in the full program

        fl_td = side_Rfl_touchdownF;
        fl_lo = side_Rfl_liftoffF;
        bl_td = side_Rbl_touchdownF;
        bl_lo = side_Rbl_liftoffF;


        
            
        
        % time. Changed from /120 to /60. Turns out videos were exported from premiere at half fps for whatever reason.
        time=(0.0:(1/60):0.5);
        
        temp = nan(lo_start_frame, 2);
        % Filtered data
        cutoff=20; %Note to self: find an optimal cutoff later
        samplerate=60;
        Wn = cutoff/samplerate;
        [B,A] = butter(2,Wn);
        
        COM_butter = filtfilt(B,A,COM_est(lo_start_frame:end,:)); %double pass, 2nd order forwards and backwards
        
        % filtfilt won't work with NaNs which are originally in COM_est. Use temp to store NaNs then concatenate back
        % into COM_butter. This is required because when use td_lo numbers, they refer to the full length of COM_est array
        % and not just the truncated COM_butter array
        COM_butter = [temp;COM_butter];
        
        
        
        %1st deriv Gradient filtered for velocity of filtered
        [~, vel] = gradient(COM_butter ./ (1/samplerate));

        
        
        %2nd deriv Gradientfiltered for accel of filtered
        [~, accel] = gradient(vel ./ (1/samplerate));
      

        
%% Following section can largely be ignored. It was used for graphing purposes to make sure data was correct
        %1st deriv original for velocity of orig data
        dx_COM_est = diff(COM_est ./ (1/samplerate));
        %2nd deriv original for accel of orig data
        dx2_COM_est = diff(dx_COM_est ./ (1/samplerate));
            
        %1st deriv filtered for velocity of filtered
        dx_COM_butter = diff(COM_butter ./ (1/samplerate));
        %2nd deriv filtered for accel of filtered
        dx2_COM_butter = diff(dx_COM_butter ./ (1/samplerate));
        
        
        


    %% Searching for optimal cut off frequencies.

        % Don't forget this!



    %% Absolute Velocity and Acceleration
        %uses the butterworth filtered data

        pre_wj_vel = [];
        post_wj_vel =[];
        
        pre_fl_wj_vel = []; %velocity right before the wall jump (should be slowed down)
        post_fl_wj_vel = []; %velocity right after the wall jump (should be higher)
        pre_bl_wj_vel = []; 
        post_bl_wj_vel = []; 

         
            %Absolute Velocity            
            abs_vel = sqrt((vel(:,1).^2) + (vel(:,2).^2));
            
            % Followng 2 lines can be ignored. Was used to check data
            abs_vel_COM = sqrt((dx_COM_est(:,1).^2) + (dx_COM_est(:,2).^2));
            abs_vel_butter = sqrt((dx_COM_butter(:,1).^2) + (dx_COM_butter(:,2).^2));
            
            
            start_vel = abs_vel(lo_start_frame+1);
            
            %for all the wall jumps get the pre and post wj
            if (isempty(fl_td) == 0)
                for i=1:length(fl_td)
                    pre_fl_wj_vel(end+1) = abs_vel(fl_td(i));
                    post_fl_wj_vel(end+1) = abs_vel(fl_lo(i));
                end
                pre_wj_vel = pre_fl_wj_vel(1);
            end            
            
            if (isempty(bl_td) == 0)
                for i=1:length(bl_td)
                    pre_bl_wj_vel(end+1) = abs_vel(bl_td(i));
                    if (bl_lo(i)-1>length(abs_vel))
                        post_bl_wj_vel(end+1) = abs_vel(bl_lo(i)-2);
                    elseif (bl_lo(i)>length(abs_vel))
                        post_bl_wj_vel(end+1) = abs_vel(bl_lo(i)-1);
                    else                        
                        post_bl_wj_vel(end+1) = abs_vel(bl_lo(i));
                    end
                end
                 post_wj_vel = post_bl_wj_vel(end);
            end
            
           
            end_vel = abs_vel(end);


        pre_fl_wj_accel = []; %velocity right before the wall jump (should be slowed down)
        post_fl_wj_accel = []; %velocity right after the wall jump (should be higher)
        pre_bl_wj_accel = []; 
        post_bl_wj_accel = []; 
        pre_wj_accel = [];
        post_wj_accel =[];
        
        
        % Absolute acceleration
        abs_accel = sqrt((accel(:,1).^2) + (accel(:,2).^2));
        
        % Ignore these two
            abs_accel_COM = sqrt((dx2_COM_est(:,1).^2) + (dx2_COM_est(:,2).^2));
            abs_accel_butter = sqrt((dx2_COM_butter(:,1).^2) + (dx2_COM_butter(:,2).^2));
            
            
            start_accel = abs_accel(lo_start_frame+1);
            
            %for all the wall jumps get the pre and post wj
            %Make sure it's not empty first
            
            if (isempty(fl_td) == 0)
                for i=1:length(fl_td)
                    pre_fl_wj_accel(end+1) = abs_accel(fl_td(i));
                    post_fl_wj_accel(end+1) = abs_accel(fl_lo(i)); 
                end
                pre_wj_accel = pre_fl_wj_accel(1);
            end
            
            

            if (isempty(bl_td) == 0)
                for i=1:length(bl_td)
                    pre_bl_wj_accel(end+1) = abs_accel(bl_td(i));
                    
                    %subtract 1 because differentiting takes away a time point and might end up with less time in
                    %accel then original trajectory time frame. (eg bl_lo(end) = 26, but len(accel) = 25).
                    if (bl_lo(i)-1>length(abs_accel))
                        post_bl_wj_accel(end+1) = abs_accel(bl_lo(i)-2);
                    elseif ((bl_lo(i))>length(abs_accel))
                        post_bl_wj_accel(end+1) = abs_accel(bl_lo(i)-1);
                    else
                        post_bl_wj_accel(end+1) = abs_accel(bl_lo(i));
                    end
                end
                post_wj_accel = post_bl_wj_accel(end);
            end
            
            
            end_accel = abs_accel(end);


%% Plotting Unfiltered vs Filtered and Diff() dadta

%     % plot all data with diff
%         figure('Position', [700,300, 600, 600]);
% 
%         %original data
%         subplot(5,2,1), scatter(COM_est(:,1), COM_est(:,2)); %subplot(row, column (overall size),  the plot number)
%         title('Trajectory');
%         ylabel('Height (cm)');
%         xlabel('Distance from Goal (cm)');
% 
%         %filtered data
%         subplot(5,2,2), scatter(COM_butter(:,1), COM_butter(:,2));
%         title('Trajectory filtered');
%         ylabel('Height (cm)');
%         xlabel('Distance from Goal (cm)');
% 
% 
%         %Velocity
%             %orig velocity
%             subplot(5,2,3), scatter(time(1:length(dx_COM_est)),dx_COM_est(:,1));
%             hold on;
%             subplot(5,2,3), scatter(time(1:length(dx_COM_est)),dx_COM_est(:,2));
%             title('Velocity');
%             ylabel('Velocity cm/s');
%             xlabel('time (s)');
%             hline = refline([0 0]);
%             hline.Color = 'r';
% 
%             %filtered velocity
%             subplot(5,2,4), scatter(time(1:length(dx_COM_butter)),dx_COM_butter(:,1));
%             hold on;
%             subplot(5,2,4), scatter(time(1:length(dx_COM_butter)),dx_COM_butter(:,2));
%             hline = refline([0 0]);
%             hline.Color = 'r';
%             title('Velocity for filtered data')
%             ylabel('Velocity cm/s');
%             xlabel('time (s)');
%             legend('x-component','y-component','Location','east');
% 
%         %Absolute velocity
%             %orig abs velocity
%             subplot(5,2,5), scatter(time(1:length(abs_vel_COM)),abs_vel_COM(:,1));
%             title('Absolute Velocity');
%             ylabel('Velocity cm/s');
%             xlabel('time (s)');
%             hline = refline([0 0]);
%             hline.Color = 'r';
% 
%             %filtered abs velocity
%             subplot(5,2,6), scatter(time(1:length(abs_vel_butter)),abs_vel_butter(:,1));
%             title('Absolute Velocity for Filtered Data');
%             ylabel('Velocity cm/s');
%             xlabel('time (s)');
%             hline = refline([0 0]);
%             hline.Color = 'r';
% 
% 
%         %acceleration
%             %orig accleration
%             subplot(5,2,7), scatter(time(1:length(dx2_COM_est)),dx2_COM_est(:,1));
%             hold on;
%             subplot(5,2,7), scatter(time(1:length(dx2_COM_est)),dx2_COM_est(:,2));
%             title('Acceleration');
%             ylabel('acceleration cm/s2');
%             xlabel('time (s)');
%             hline = refline([0 0]);
%             hline.Color = 'r';
% 
%             %filtered acceleration
%             subplot(5,2,8), scatter(time(1:length(dx2_COM_butter)),dx2_COM_butter(:,1));
%             hold on;
%             subplot(5,2,8), scatter(time(1:length(dx2_COM_butter)),dx2_COM_butter(:,2));
%             title('Acceleration for filtered data');
%             ylabel('acceleration cm/s2');
%             xlabel('time (s)');
%             hline = refline([0 0]);
%             hline.Color = 'r';
% 
%         %Absolute acceleration
%             %orig abs acceleration
%             subplot(5,2,9), scatter(time(1:length(abs_accel_COM)),abs_accel_COM(:,1));
%             title('Absolute Acceleration');
%             ylabel('Acceleration cm/s');
%             xlabel('time (s)');
%             hline = refline([0 0]);
%             hline.Color = 'r';
% 
%             %filtered abs acceleration
%             subplot(5,2,10), scatter(time(1:length(abs_accel_butter)),abs_accel_butter(:,1));
%             title('Absolute Acceleration for Filtered Data');
%             ylabel('acceleration cm/s');
%             xlabel('time (s)');
%             hline = refline([0 0]);
%             hline.Color = 'r';


COM_est(isnan(COM_est)) = [];
dx_COM_est(isnan(dx_COM_est)) = [];
dx2_COM_est(isnan(dx_COM_est)) = [];
        
end
        
        
        
