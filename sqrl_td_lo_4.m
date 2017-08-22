function [toptable,num_Rfl_steps, num_Lfl_steps, num_Rbl_steps, num_Lbl_steps, ...
    side_Rfleg_td, side_Rfleg_lo, side_Rbleg_td, side_Rbleg_lo, ...
    side_Lfleg_td, side_Lfleg_lo, side_Lbleg_td, side_Lbleg_lo, ...
    top_Rfleg_td, top_Rfleg_lo, top_Lfleg_td, top_Lfleg_lo, ...
    top_Rbleg_td, top_Rbleg_lo, top_Lbleg_td, top_Lbleg_lo, ...
    Rfl_stance, Lfl_stance, Rbl_stance, Lbl_stance, Rfl_swing, Lfl_swing, ...
    Rbl_swing, Lbl_swing, Rfl_touchXY, Rbl_touchXY, wj_start, wj_end] ...
    = sqrl_td_lo_4(top_struct, sidetable)

%% Set up the table
%     % Next 7 lines only used for testing individual files
%     disp('Calculating only one individual in td_lo_4');
%     sqrl_file_top = strcat('Beyonce_top_100H_5_top4data.mat');
%     top_struct = load(sqrl_file_top);
%     sqrl_file_side = strcat('Beyonce_side_100H_5_data.mat');
%     side_struct = load(sqrl_file_side);
%     sidetable = struct2table(side_struct.squirrelStructure);
%     times = sidetable.time(1,:)';
%     sidetable.time = times;
    
    
    
    % Keep uncommented
    toptable = struct2table(top_struct.squirrelStructure);

%Effed up times, fix. (misrecorded during tracking)
    times = toptable.time(1,:)';
    toptable.time = times;

%% Tracking the leg touchdowns and lift offs

%Front leg
    %set up empty arrays for touch downs and lift off
    top_Rfleg_td=[];
    top_Rfleg_lo=[];
    top_Lfleg_td=[];
    top_Lfleg_lo=[];

    %if it starts with a touchdown, note down the touch down frame is 1
    if (toptable.Rfleg_touch(1)==1)
        top_Rfleg_td(end+1)=1;
    end                            
    if (toptable.Lfleg_touch(1)==1)
        top_Lfleg_td(end+1)=1;
    end


    %find the differences in values (0, 1) that denote touchdown lift off  in the table
    td_lo = diff(toptable.Rfleg_touch);

    %if difference is -1, then lift off, if 1 it is touch down
    for i=1:length(td_lo)
        if (td_lo(i)==-1) 
            top_Rfleg_lo(end+1)=i;
        elseif (td_lo(i)==1)
            top_Rfleg_td(end+1)=i+1;
        end
    end
    %count number of steps taken
    num_Rfl_steps = length(top_Rfleg_td);

    td_lo = diff(toptable.Lfleg_touch);

    %if difference is -1, then lift off, if 1 it is touch down
    for i=1:length(td_lo)
        if (td_lo(i)==-1) 
            top_Lfleg_lo(end+1)=i;
        elseif (td_lo(i)==1)
            top_Lfleg_td(end+1)=i+1;
        end
    end
    %count number of steps taken
    num_Lfl_steps = length(top_Lfleg_td);

%Back Leg
    top_Rbleg_td=[];
    top_Rbleg_lo=[];
    top_Lbleg_td=[];
    top_Lbleg_lo=[];


    td_lo = diff(toptable.Rbleg_touch);
    for i=1:length(td_lo)
        if (td_lo(i)==-1) 
            top_Rbleg_lo(end+1)=i;
        elseif (td_lo(i)==1)
            top_Rbleg_td(end+1)=i+1;
        end
    end
    
    %if time ends with back leg still touching then take that frame number
    if (toptable.Rbleg_touch(length(toptable.time))==1)
        top_Rbleg_lo(end+1)=length(toptable.time);
    end
    num_Rbl_steps = length(top_Rbleg_td);

    td_lo = diff(toptable.Lbleg_touch);
    for i=1:length(td_lo)
        if (td_lo(i)==-1) 
            top_Lbleg_lo(end+1)=i;
        elseif (td_lo(i)==1)
            top_Lbleg_td(end+1)=i+1;
        end
    end
    %if time ends with back leg still touching then take that frame number
    if (toptable.Lbleg_touch(length(toptable.time))==1)
        top_Lbleg_lo(end+1)=length(toptable.time);
    end
    num_Lbl_steps = length(top_Lbleg_td);

%% Stance-Swing Time

%Stance time                 
    Rfl_stance = [];
    %Right Front leg
    timestep = toptable.time(2)-toptable.time(1);
    for i=1:length(top_Rfleg_td)
        starttime = toptable.time(top_Rfleg_td(i));
        endtime = toptable.time(top_Rfleg_lo(i));
        Rfl_stance(end+1) = (endtime-starttime+timestep)*1000;
    end

    Lfl_stance = [];
    %Front leg
    for i=1:length(top_Lfleg_td)
        starttime = toptable.time(top_Lfleg_td(i));
        endtime = toptable.time(top_Lfleg_lo(i));
        Lfl_stance(end+1) = (endtime-starttime+timestep)*1000;
    end

    Rbl_stance = [];
    %Front leg
    for i=1:length(top_Rbleg_td)
        starttime = toptable.time(top_Rbleg_td(i));
        endtime = toptable.time(top_Rbleg_lo(i));
        Rbl_stance(end+1) = (endtime-starttime+timestep)*1000;
    end

    Lbl_stance = [];
    %Front leg
    for i=1:length(top_Lbleg_td)
        starttime = toptable.time(top_Lbleg_td(i));
        endtime = toptable.time(top_Lbleg_lo(i));
        Lbl_stance(end+1) = (endtime-starttime+timestep)*1000;
    end



%Swing time 
    Rfl_swing = [];
    %Front leg
    for i=1:(length(top_Rfleg_td)-1)
        starttime = toptable.time(top_Rfleg_lo(i));
        endtime = toptable.time(top_Rfleg_td(i+1));
        Rfl_swing(end+1) = (endtime-starttime+timestep)*1000;
    end

    Lfl_swing = [];
    %Front leg
    for i=1:(length(top_Lfleg_td)-1)
        starttime = toptable.time(top_Lfleg_lo(i));
        endtime = toptable.time(top_Lfleg_td(i+1));
        Lfl_swing(end+1) = (endtime-starttime+timestep)*1000;
    end

    Rbl_swing = [];
    %Front leg
    for i=1:(length(top_Rbleg_td)-1)
        starttime = toptable.time(top_Rbleg_lo(i));
        endtime = toptable.time(top_Rbleg_td(i+1));
        Rbl_swing(end+1) = (endtime-starttime+timestep)*1000;
    end

    Lbl_swing = [];
    %Front leg
    for i=1:(length(top_Lbleg_td)-1)
        starttime = toptable.time(top_Rbleg_lo(i));
        endtime = toptable.time(top_Lbleg_td(i+1));
        Lbl_swing(end+1) = (endtime-starttime+timestep)*1000;
    end



%% Get the x-y coordinates of the feet
    % Can only get right leg because that's the only leg tracked in the side view.
    Rfl_touchXY_temp=[];
    Rfl_touchXY =[];

    %find the very first frame where either right or left foot touches down in the top view
    if (isempty(top_Lfleg_td) && isempty(top_Rfleg_td))
        first_td_top = min(top_Lbleg_td);
    elseif (isempty(top_Rfleg_td))
        first_td_top = top_Lfleg_td(1);
    elseif (isempty(top_Lfleg_td))
        first_td_top = top_Rfleg_td(1);
    else
        first_td_top = min([top_Lfleg_td,top_Rfleg_td]);
    end
    
    %find where the row values line up in time for side and top view using the first touch down frame
    for t=1:length(sidetable.time)
        if (sidetable.time(t)==toptable.time(first_td_top))
            frame_side_wj_start = t;
        end 
    end
    
    side_Rfleg_td=[];
    side_Rfleg_lo=[];
    side_Rbleg_td=[];
    side_Rbleg_lo=[];
    
    for i=1:length(top_Rfleg_td)
        %convert the td lo of top view to side view
        % if the first foot that touched down was the Rfleg, then just store the frame_side_wj_start
        if (i==1 && top_Rfleg_td(1)==first_td_top)
            side_Rfleg_td(end+1) = frame_side_wj_start;
            %Rfleg lo will be X frames after the td, so take the difference of lo frame from the first td
            side_Rfleg_lo(end+1) = frame_side_wj_start + top_Rfleg_lo(i)-first_td_top;
        else
            %all subsequent td/lo will be the side frame start, plus the difference of the new td/lo to the first td
            side_Rfleg_td(end+1) = frame_side_wj_start + top_Rfleg_td(i)-first_td_top;
            side_Rfleg_lo(end+1) = frame_side_wj_start + top_Rfleg_lo(i)-first_td_top;
        end

        XYstart = side_Rfleg_td(i);
        XYend = side_Rfleg_lo(i);

        for xy=XYstart:XYend
            Rfl_touchXY_temp(end+1,:) = [sidetable.frontlegx(xy), sidetable.frontlegy(xy)];
        end

        Rfl_touchXY(end+1,:) = mean(Rfl_touchXY_temp,1);
        Rfl_touchXY_temp=[];
    end

    for i=1:length(top_Rbleg_td)
        %convert the td lo of top view to side view
        side_Rbleg_td(end+1) = frame_side_wj_start + top_Rbleg_td(i) - first_td_top;
        side_Rbleg_lo(end+1) = frame_side_wj_start + top_Rbleg_lo(i) - first_td_top;
    end

    Rbl_touchXY_temp=[];
    Rbl_touchXY =[];
    for i=1:length(top_Rbleg_td)
        XYstart = side_Rbleg_td(i);
        XYend = side_Rbleg_lo(i);

        for xy=XYstart:XYend
            Rbl_touchXY_temp(end+1,:) = [sidetable.backlegx(xy),sidetable.backlegy(xy)];
        end

        Rbl_touchXY(end+1,:) = mean(Rbl_touchXY_temp,1);
        Rbl_touchXY_temp=[];
    end
    
    
    side_Lfleg_td=[];
    side_Lfleg_lo=[];
    side_Lbleg_td=[];
    side_Lbleg_lo=[];
    
    for i=1:length(top_Lfleg_td)
        %convert the td lo of top view to side view
        % if the first foot that touched down was the Lfleg, then just store the frame_side_wj_start
        if (i==1 && top_Lfleg_td(1)==first_td_top)
            side_Lfleg_td(end+1) = (frame_side_wj_start); 
            %Lfleg lo will be X frames after the td, so take the difference of lo frame from the first td
            side_Lfleg_lo(end+1) = frame_side_wj_start + top_Lfleg_lo(i)-first_td_top;
        else
            %all subsequent td/lo will be the side frame start, plus the difference of the new td/lo to the first td
            side_Lfleg_td(end+1) = frame_side_wj_start + top_Lfleg_td(i)-first_td_top;
            side_Lfleg_lo(end+1) = frame_side_wj_start + top_Lfleg_lo(i)-first_td_top;
        end
    end

    for i=1:length(top_Lbleg_td)
        %convert the td lo of top view to side view
        side_Lbleg_td(end+1) = frame_side_wj_start + top_Lbleg_td(i) - first_td_top;
        side_Lbleg_lo(end+1) = frame_side_wj_start + top_Lbleg_lo(i) - first_td_top;
    end

    wj_start=min([side_Lfleg_td, side_Rfleg_td, side_Lbleg_td, side_Rbleg_td]);
    wj_end=max([side_Lbleg_lo, side_Rbleg_lo, side_Lbleg_lo, side_Rbleg_lo]);
end