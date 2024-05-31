function [y] = my_cos(x)

y = 1 - (x.^2 / factorial(2)) + (x.^4 / factorial(4));