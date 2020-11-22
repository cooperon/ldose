void wavecar_info_( char *filename, int *fnameLength, int *nrecl, int *nspin, int *nprec, int *nwk, int *nband, double *ecut,
                        int *npmax, int *nb1max, int *nb2max, int *nb3max, double *a1mag, double *a2mag,double *a3mag,double *a,
                        double *b, int *ierr);


void read_wavecar_bands_ldos_( char *filename, int *fnameLength, double *coords, int *nCoords, int *bands, int *nBands,
                               double *kPoints, int *nKpoints, double *energy, double *lDOS,double *lSigmaX, double *lSigmaY, double *lSigmaZ, int *ierr);


void ludcmp_(double *A, int *N,int *NP,int *INDX,int *D, int *CODE);
void lubksb_(double *A, int  *N, int *NP, int *INDX, double *B);


