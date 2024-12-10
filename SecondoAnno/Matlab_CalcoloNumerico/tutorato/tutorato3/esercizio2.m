close all
clear all
clc

A = [3, 4, 1; 6, 9, 2; 5, 2, 8];
A_inv = inv(A);     % e' ok ma costa troppo quindi NO!

B = [1, 0, 0; 0, 7, 0; 0, 0, 15];
B_inv = diag([1/B(1,1), 1/B(2,2), 1/B(3,3)]);

C = [20, 1, 7; 0, 2, 17; 0, 0, 3];
C_inv = invupper(C);

D = C';
D_inv = C_inv';