% Read Heat1D.f90 output file

clear all
close all
clc

% INITIAL CONDITION

% Open file
FileID = fopen('Heat1D_FCTS_explicit-0000.dat');
% Read array dimension
N = fscanf(FileID,'%d \n',1);
% Read data
x  = fscanf(FileID,'%f \n',N);
T0 = fscanf(FileID,'%f \n',N);


% FINAL SOLUTION
% Open file
FileID = fopen('Heat1D_FCTS_explicit-0000.dat');
% Read array dimension
N = fscanf(FileID,'%d \n',1);
% Read data
x  = fscanf(FileID,'%f \n',N);
T1 = fscanf(FileID,'%f \n',N);

% Plot data
plot(x,T0,'bo')
hold on
plot(x,T1,'ro')
legend('IC','Final solution')