function [y] = my_sin(x)

y = x - (x.^3 / factorial(3)) + (x.^5 / factorial(5));