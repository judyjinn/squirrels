function [Trot_X, Trot_Y, Trot_butt, Trot_nose, rotrad_M, bodymidX, bodymidY, noseX, noseY, buttX, buttY, sidetable ]...
    = sqrl_Trot(side_struct, jumpname)

%open the struct into a table to access data easier
sidetable = struct2table(side_struct);

%Effed up times, fix. (misrecorded during tracking)
    times = sidetable.time(1,:)';
    sidetable.time = times;
   
%remove extraneous data (this is incorrectly calculated because it is not ceneted at the origin and should be ignored)
    sidetable.quadraticButtA=[];
    sidetable.quadraticButtB=[];
    sidetable.quadraticButtC=[];
    sidetable.quadraticTailA=[];
    sidetable.quadraticTailB=[];
    sidetable.quadraticTailC=[];

%% standardize all the distances 
    %The standardize distance (height of landing perch base) in tracking data is not accurate
    dist=0;
    if (strcmp(jumpname,'50'))
        dist=50;
    elseif (strcmp(jumpname,'50L'))
        dist=67.08;
    elseif (strcmp(jumpname,'100'))
        dist=100;
    elseif (strcmp(jumpname,'100L'))
        dist=118.30;
    elseif (strcmp(jumpname,'100H'))
        dist=77.46;
    elseif (strcmp(jumpname,'150'))
        dist=150;
    elseif (strcmp(jumpname,'150L'))
        dist=168.8;
    elseif(strcmp(jumpname,'150H'))
        dist=128.5;
    end
    
    %Have to multiply all distances inside the table by the newfound standard
    last = length(sidetable.frontlegx); %last frame in video
    %find distance between back leg and last frame of first leg. This should be the distance from jump to perch
    legdist = sidetable.frontlegx(last)-sidetable.backlegx(1);
    standardize = dist/legdist;
    for col = 2:23
        sidetable.(col)=sidetable.(col)*standardize;
    end
    
%% quicker access to X-Y points
    buttX = sidetable.buttx;
    buttY = sidetable.butty;
    noseX = sidetable.nosex; %no sex.
    noseY = sidetable.nosey;
    bodymidX = sidetable.BodymidptX;
    bodymidY = sidetable.BodymidptY;
    

    
    rotrad = nan(length(times),1); %stores the amount of rotation needed for each point at a particular time
  
    
    time=1; %only for scatter plot purposes.
%check the scatter for current time
%     figure;
%     scatter([buttX(time),noseX(time), bodymidX(time)], [buttY(time), noseY(time), bodymidY(time)])




%% Translate the points such that body mid is the origin
    T_bodymidX = bodymidX-bodymidX;
    T_bodymidY = bodymidY-bodymidY;
    T_noseX = noseX - bodymidX;
    T_noseY = noseY - bodymidY;
    T_buttX = buttX - bodymidX;
    T_buttY = buttY - bodymidY;

%Combine the data into single arrays
    T_bodymid = [T_bodymidX, T_bodymidY];
    T_nose = [T_noseX, T_noseY];
    T_butt = [T_buttX, T_buttY];

%     %check the scatter for translated points
%         figure;
%         scatter([T_buttX(time), T_noseX(time), T_bodymidX(time)], [T_buttY(time), T_noseY(time), T_bodymidY(time)]);    

    %Create empty arrays of proper size to store values
    Trot_butt = nan(length(time),2);
    Trot_nose = nan(length(time),2);
    Trot_X = nan(length(time),3);
    Trot_Y = nan(length(time),3);


%%recalculate quadratic fits for body at each time to get COMs
    %must shift all points to origin for each time point to balance the
    %points then shift values back to original X-Y positions.
for t = 1: length(times)
    %rotate the butt and nose points around the origin (where bodymid is)

        slope = (T_buttY(t) - T_noseY(t)) / (T_buttX(t) - T_noseX(t));
        rotdegree = atand(0) - atand(slope); %in degrees

        rotrad(t) = rotdegree*pi()/180; %radians

        rotrad_M = [cos(rotrad(t)), -sin(rotrad(t));
                    sin(rotrad(t)), cos(rotrad(t))];

        Trot_butt(t,:) = (rotrad_M*(T_butt(t,:))')';
        Trot_nose(t,:) = (rotrad_M*(T_nose(t,:))')';

    % Create x and y coordinates, butt, mid, nose
        Trot_X(t,:) = [Trot_butt(t,1), T_bodymid(t,1), Trot_nose(t,1)];
        Trot_Y(t,:) = [Trot_butt(t,2), T_bodymid(t,2), Trot_nose(t,2)];

%     % %check the scatter for roated translated points
%         figure;
%         scatter(Trot_X(time,:), Trot_Y(time,:));

%     % %   unrotate test
%         Trotrev_butt = rotrad_M'*(T_butt');
%         Trotrev_nose = rotrad_M'*(T_nose');
    %     figure;
    %     scatter([Trotrev_butt(1), T_bodymidX, Trotrev_nose(1)], [Trotrev_butt(2), T_bodymidY,Trotrev_nose(2)]);
    
    
    

end
