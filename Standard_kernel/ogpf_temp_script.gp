# ogpf libray
# Rev. 0.22 of March 9th, 2018
# Licence: MIT

# gnuplot global setting
set term wxt size 640,480 enhanced font "verdana,10" title "ogpf libray: Rev. 0.22 of March 9th, 2018"

# ogpf extra configuration
# -------------------------------------------
# color definitions
set style line 1 lc rgb "#800000" lt 1 lw 2
set style line 2 lc rgb "#ff0000" lt 1 lw 2
set style line 3 lc rgb "#ff4500" lt 1 lw 2
set style line 4 lc rgb "#ffa500" lt 1 lw 2
set style line 5 lc rgb "#006400" lt 1 lw 2
set style line 6 lc rgb "#0000ff" lt 1 lw 2
set style line 7 lc rgb "#9400d3" lt 1 lw 2

# Axes
set border linewidth 1.15
set tics nomirror

# grid
# Add light grid to plot
set style line 102 lc rgb "#d6d7d9" lt 0 lw 1
set grid back ls 102

# plot style
set style data linespoints

# -------------------------------------------

 
# options
set style data linespoints
set xrange[-0.1:1.26]
set yrange [-1000:1000]


 
# plot scale
 
# Annotation: title and labels
set title "Example 1"
set xlabel "x [mm]"
set ylabel "C [Mpa]"
 
# axes setting

plot "-" title "True Stress" lt 6 lc rgb "#000000"
  1.578550542128798E-002   620.070512684372     
  4.218870962220356E-002   367.152936928774     
  7.278550752835933E-002   817.228491866339     
  0.101184967587409        583.605054473383     
  0.131588170128773        602.323030556926     
  0.160879587909560        504.005636817329     
  0.191144436618046        154.353084958513     
  0.220579457159720        194.912265488787     
  0.250720840629633       -13.9191895477304     
  0.280231904836238        14.4718009912647     
  0.310296500735084       -150.893534387125     
  0.339856035428883       -138.380560712106     
  0.369867828526884       -307.983324085073     
  0.399464433595229       -362.014223367880     
  0.429450560843111       -355.146981608748     
  0.459206465197265       -402.222746173736     
  0.489134233284518       -357.263990216342     
  0.518995283688860       -401.480145825169     
  0.548912663137700       -364.445326659316     
e
