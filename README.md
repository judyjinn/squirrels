# squirrels_wall_jump
MATLAB and R code for squirrels wall jumping

Experiment: assess the kinematics of squirrels performing wall jumps

MATLAB files are for tracking videos of squirrels jumping then transforming the data from euclidenan coordinates to distances to analyze positional, velocity, and acceleration data.

`sqrl_Trot.m` takes a single video's tracking data, rotates them into the proper orientation to pull information about body position and curvature.

`sqrl_td_lo_4.m` is used to track all touch down and lift off data for squirrel's feet to assess gait

`sqrl_vel_accel.m` assesses velocity and acceleration data for each jump

`sqrl_data_generator.m` assess each squirrel tracking data individually and puts all information into a MATLAB struct sorted by squirell name and jump type.

`sqrl_data_generator.m` takes all the data from tracking file, transforms it into a data table, and concatenates all the information together to be exported as a CSV.

`walljump_analysis.R` is then used to analyze the CSV generated from `sqrl_data_generator.m` for basic statistics as well as graphing.


Original data excluded for privacy.
