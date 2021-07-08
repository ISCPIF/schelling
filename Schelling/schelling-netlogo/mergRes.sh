# header
cd exploration
ls | grep "SOBOL_GRIDS" | head -n 1 | awk '{print "head -n 1 "$0" >> SOBOL_ALLGRIDS.csv"}'| sh
# all files
ls | grep "SOBOL_GRIDS" | awk '{print "tail -n +2 "$0" >> SOBOL_ALLGRIDS.csv"}'| sh

