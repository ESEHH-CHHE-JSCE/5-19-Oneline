# Plot shoreline
set term win
load 'line.pth'
set title 'Position of shoreline'
set xlabel 'Distance longshore (m)' -4
set ylabel 'Distance offshore (m)' -4
plot 'cstline.dat' using 1:2 title 't=0.0Tmax' with line 1, \
     'cstline.dat' using 1:3 title 't=0.2Tmax' with line 2, \
     'cstline.dat' using 1:4 title 't=0.4Tmax' with line 3, \
     'cstline.dat' using 1:5 title 't=0.6Tmax' with line 4, \
     'cstline.dat' using 1:6 title 't=0.8Tmax' with line 5, \
     'cstline.dat' using 1:7 title 't=1.0Tmax' with line 6
pause -1 'Continue'
set title 'Profile of littral drift flux'
set xlabel 'Distance longshore (m)' -4
set ylabel 'Littral drift flux (m^3/year)' -4
plot 'flux.dat' using 1:2 title 't=Tmax' with line
pause -1 'Continue'
set title 'Profile of breaking wave height'
set xlabel 'Distance longshore (m)' -4
set ylabel 'Wave Height (m)'     -4
plot 'flux.dat' using 1:3 title 't=Tmax' with points
pause -1 'Continue'
set title 'Profile of breaking wave direction'
set xlabel 'Distance longshore (m)' -4
set ylabel 'Wave angle (DEG)'     -4
plot 'flux.dat' using 1:4 title 't=Tmax' with points
pause -1 'Exit'
