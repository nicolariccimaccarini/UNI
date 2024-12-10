close all
clear all
clc

int=input('inserisci un valore in base dieci da convertire, la parte intera: ');
fraz=input('inserisci la parte frazionaria: ');
base=input('inserisci base: ');

[vint]=conv10toBETA_int(int,base);
[vfraz]=conv10toBETA_fraz(fraz,base,4);

fprintf('il valore convertito e'' %s%s',vint,vfraz);