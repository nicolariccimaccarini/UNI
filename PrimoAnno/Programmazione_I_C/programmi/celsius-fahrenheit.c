#include <stdio.h>

int celsiusToFahrenheit()
{
     int i;
     
     for (i=-100; i<=100; i+=10)
     {
          printf ("%d", i);
          printf ("   |   ");
          printf ("%d\n", ((i/5)*9)+32);
          printf ("---------------\n");
     }
}

float fahrenheitToCelsius()
{
     float k;

     for (k=-100.0; k<=300.0; k+=10)
     {
          printf ("%f", k);
          printf ("   |   ");
          printf ("%f\n", (k-32)*5/9);
          printf ("---------------\n");
     }
}

int main()
{
     printf ("Celsius to Fahrenheit:\n");
     printf ("%d\n\n\n", celsiusToFahrenheit());

     printf ("******************************************\n\n\n");

     printf ("Fahrenheit to Celsius:\n");
     printf ("%f\n", fahrenheitToCelsius());

     return 0;
}