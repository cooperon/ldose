MATIO_INCLUDE_DIR=/home/pvv/local/matio-1.5.17/Install/include
MATIO_LIB_DIR=/home/pvv/local/matio-1.5.17/Install/lib
HDF_LIB=/home/pvv/local/hdf5-1.10.4/Install/lib

rm *o
icc -c  -O2 ldos_e.c -I$MATIO_INCLUDE_DIR
icc -c -O2 FileIO.c
icc -c -O2 Utils.c
ifort -c  -O2 -assume byterecl ReadWavecar.f90
icc -oldos_e   -O2 ldos_e.o ReadWavecar.o FileIO.o Utils.o   -L$MATIO_LIB_DIR  -lmatio -L$HDF_LIB  -lhdf5 -lmkl_intel_ilp64  -lmkl_intel_thread -lmkl_core -liomp5 -lm -ldl -lpthread -lifcore 

