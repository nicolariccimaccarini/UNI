close all
clear all
clc

x = [-2, -1, 0, 1, 2]; 
y_sin = my_sin(x);
y_cos = my_cos(x);

y_sin_true = sin(x);
y_cos_true = cos(x);

% Errori assoluti
ea_sin = abs(y_sin_true - y_sin);
ea_cos = abs(y_cos_true - y_cos);

% Errori relativi
er_sin = ea_sin ./ abs(y_sin_true);
er_cos = ea_cos ./ abs(y_cos_true);

fprintf('Errore assoluto seno: %e\n', ea_sin);
fprintf('Errore assoluto coseno: %e\n', ea_cos);
fprintf('Errore relatico seno: %e\n', er_sin);
fprintf('Errore relatico coseno: %e\n', er_cos);