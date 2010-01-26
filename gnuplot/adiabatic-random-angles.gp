# The following command was run to extract the data from the database:
#
# > psql vmps -c "select number_of_sites, avg(energy_gap), stddev(energy_gap), min(energy_gap), max(energy_gap), ln(number_of_sites), ln(avg(energy_gap)), ln(avg(energy_gap)+stddev(energy_gap))-ln(avg(energy_gap)-stddev(energy_gap)) from adiabatic_random_angle_simulations group by number_of_sites order by number_of_sites asc;" -tA -F " " > adiabatic-random-angles.dat

# The following commands were used to compute the fit parameters
#
# f(x) = a*x + b
# fit f(x) 'adiabatic-random-angles.dat' u 6:7:8 via a,b

# Now we set the display parameters of the plot
set size square
set logscale xy
set title 'Energy Gap versus Number of Sites'
set xrange [5 to 500]
set xlabel 'Number of Sites'
set yrange [0.01 to 1]
set ylabel 'Energy Gap'
set bmargin 4
set tmargin 3
set lmargin 0
set rmargin 0

# Now we set up the styles for the lines
set style line 1 lt 1 # Fit
set style line 2 lt 3 # Envelope Bounds

# Now onto the plots!

# First we plot the mean / standard deviations
plot 'adiabatic-random-angles.dat' u 1:2:3 w yerrorbars title 'Mean +/- Standard Deviation'

# Then we plot the fit
# (I have manually substituted the values for a and b here in order to ensure
# that the plot matches the label.)
replot (4.2 * x**(-0.95)) w lines ls 1 title 'Regression fit: 4.2 * x^(-0.95)'

# Finally we plot the envelope
replot 'adiabatic-random-angles.dat' u 1:4 w lines ls 2 title 'Upper/Lower Bounds'
replot'adiabatic-random-angles.dat' u 1:5 w lines ls 2 title ''

# Next we set up the terminal to produce postscript output
set terminal postscript
set output 'adiabatic-random-angles.ps'
replot
