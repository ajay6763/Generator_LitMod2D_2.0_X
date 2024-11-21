# Generator_LitMod2D_2.0_X
@Ajay November 2024
# This is Generator implementaion from Alex @2023 (Serpentenization) where on HP02 thermodynamics database is used.
For this to run you should have Perple_X release 7.1.1, Jul 13, 2023. installed and its binaries in the path. PLEASE MIND THE PERPLEX_VERSION

To compile:

gfortran Generator_LitMod_Alex.for olivine.for diopside.for garnet.for albite.for anortite.for ortenstatitemg90.for spinel.for  generator_table_atten_corr.for -o  Generator_LitMod_Alex

To run:
./Generator_LitMod_Alex
