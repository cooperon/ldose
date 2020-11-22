#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <matio.h>
#include <float.h>
#include <math.h>
#include <sys/time.h>
#include <unistd.h>
#include <mkl.h>
#include "FileIO.h"
#include "Utils.h"
#include "ReadWavecar.h"

int ConvertToAffineCoordinates(double *r, double *a, double *output);

typedef struct timeval TIME_STRUCT;

void GetTime(TIME_STRUCT *timeStruct);
double GetRunTime(TIME_STRUCT *initialTime);

void HelpOutput();

static char SetupFileName[]="ldos_e.par";

#define _HEADER_TEXT_LENGTH 16254

static char HEADER_TEXT[_HEADER_TEXT_LENGTH];


int main(int argc, char*argv[])
{
 int i,j,k, iPoint;
 long l;
 int nrecl, nspin, nprec, nwk, nband, npmax, nb1max, nb2max, nb3max, ierr;
 double a[9], b[9],ecut, *a_tmp;
 double a1mag,a2mag,a3mag, dE, dx=0.,dy=0.,dz=0.;
 int nz;
 int nKpoints=0;
 double  *energy=NULL, *coords=NULL, *affineCoords=NULL, *kPoints=NULL, *lDOS=NULL, *lSigmaX=NULL, *lSigmaY=NULL, *lSigmaZ=NULL;
 double *transposedCoords=NULL, *transposedKpoints=NULL, *transposedEnergy=NULL;
 int nEnergy=0, nCoords=0;
 long nDensityPoints=0;

 TIME_STRUCT initialTime;
 
 size_t dim[]={1,1,1,1};
 INPUT_DATA data;
 double *tmpArray=NULL;
 matvar_t *matvar;
 mat_t *matfp=NULL;
 double *spinDensity=NULL;
 
 char *commentWaveString="Ampliude indices: xyz point, KzKyKz point, band number";
 char *commentSpinString="Ampliude indices: xyz point, KzKyKz point, band number, SigmaXYZ";
 char *commentKString="K points indices: Kz, Ky, Kz";
 char *commentCoordString="XYZ points indices: X, Y, Z";
 char *commentEnergyString="Energy indices: KxKyKz point, band number";
 
 GetTime(&initialTime);
 if(argc == 1 && CheckFileStatus(SetupFileName))
 {
  HelpOutput();
  return 0;
 }
 SetupRead(SetupFileName, &data );
 ParceCommandString(argc, argv, &data);
 if(VerifyInputData(&data))
  goto _END;
 mkl_set_num_threads(data.nThreads);
 wavecar_info_( data.inputFile, &data.filenameLength, &nrecl, &nspin, &nprec, &nwk, &nband, &ecut, &npmax, &nb1max, &nb2max, &nb3max, &a1mag,&a2mag,&a3mag,a,b,&ierr);
 if(ierr)
 {
  printf("Error: %d\n", ierr);
  return 0;
 }
 printf("nrecl=%d, nspin=%d, nprec=%d, nwk=%d\n", nrecl, nspin, nprec, nwk);
 printf("nband=%d, npmax=%d\n",  nband, npmax);
 printf("nb1max=%d, nb2max=%d, nb3max=%d\n",  nb1max, nb2max, nb3max);
 printf("a1mag=%f, a2mag=%f, a3mag=%f\n",  a1mag, a2mag, a3mag);


 for(i=0;i<data.nBands;i++)
 {
  if(data.bands[i] <=0 || data.bands[i]>nband)
   break;
 }
 if(i<data.nBands)
 	{
 	 printf("Error! Wrong band number %d (i=%d)\n", data.bands[i]	,i+1);
 	 free(data.bands);
 	 return 0;
 	}	
 if(data.nX>1)
 	dx=(data.xMax-data.xMin)/(data.nX-1);
 if(data.nY>1)
 	dy=(data.yMax-data.yMin)/(data.nY-1);
 if(data.nZ>1)
 	dz=(data.zMax-data.zMin)/(data.nZ-1);
 	nCoords = data.nX*data.nY*data.nZ;
 coords = (double*)calloc(3*nCoords , sizeof(double));
 affineCoords = (double*)calloc(3*nCoords , sizeof(double));
 a_tmp=a;
 for(k=0;k<data.nZ;k++)
  for(j=0;j<data.nY;j++)
   for(i=0;i<data.nX;i++)
   {
   	iPoint = 3*((k*data.nY+j)*data.nX+i);
    coords[iPoint]=data.xMin+dx*i;
    coords[iPoint+1]=data.yMin+dy*j;
    coords[iPoint+2]=data.zMin+dz*k;
    if(ConvertToAffineCoordinates(&coords[iPoint], a_tmp,&affineCoords[iPoint])) 
    {
     printf( "Collinear lattice vectors\n");
     goto _END;
    }
    if(a_tmp) a_tmp=NULL;
  }
 nEnergy = data.nBands*nwk;
 energy = (double*)calloc(nEnergy,sizeof(*energy));
 nDensityPoints = (long)nEnergy*data.nX*data.nY*data.nZ;
 lDOS= (double*)calloc(nDensityPoints,sizeof(*lDOS));
 lSigmaX= (double*)calloc(nDensityPoints,sizeof(*lSigmaX));
 lSigmaY= (double*)calloc(nDensityPoints,sizeof(*lSigmaY));
 lSigmaZ= (double*)calloc(nDensityPoints,sizeof(*lSigmaZ));
 kPoints= (double*)calloc(nwk*3,sizeof(*kPoints));
 printf("Fermi energy: %f eV\n", data.eFermi);
 printf("WAVECAR reading started\n");
 read_wavecar_bands_ldos_(  data.inputFile, &data.filenameLength, affineCoords, &nCoords, data.bands, &data.nBands,
                                 kPoints, &nwk, energy, lDOS,lSigmaX,lSigmaY,lSigmaZ, &ierr);

 printf("WAVECAR reading finished\n");
 if(ierr) goto _END;
 	/*
 {
  FILE *fp=fopen("Data.dat","w");
  for(i=0;i<nEnergy;i++)
   fprintf(fp,"%le %le %le\n",energy[i], nFi[i], nHi[i]);
 fclose(fp);
 }
 */
 

 sprintf(HEADER_TEXT,"Wavefuction amplitudes and partial spin densities. Inputfile: %s", data.inputFile);
 if(!(matfp = Mat_CreateVer(data.outputFile,HEADER_TEXT,MAT_FT_DEFAULT)))
 {
  printf("Error creating %s\n", data.outputFile);
  goto _END;
 }
 dim[1]=strlen(commentWaveString);
 if(!(matvar = Mat_VarCreate("WavefunctionComment",MAT_C_CHAR,MAT_T_UINT8,2,dim,commentWaveString,0)))
  printf("Error creating variable for 'Comment'\n");
 else
 {
  Mat_VarWrite(matfp,matvar,MAT_COMPRESSION_NONE);
  Mat_VarFree(matvar);
 }
  dim[1]=strlen(commentSpinString);
 if(!(matvar = Mat_VarCreate("SpinDensityComment",MAT_C_CHAR,MAT_T_UINT8,2,dim,commentSpinString,0)))
  printf("Error creating variable for 'Comment'\n");
 else
 {
  Mat_VarWrite(matfp,matvar,MAT_COMPRESSION_NONE);
  Mat_VarFree(matvar);
 }

 dim[1]=strlen(commentKString);
 if(!(matvar = Mat_VarCreate("KpointComment",MAT_C_CHAR,MAT_T_UINT8,2,dim,commentKString,0)))
  printf("Error creating variable for 'Comment'\n");
 else
 {
  Mat_VarWrite(matfp,matvar,MAT_COMPRESSION_NONE);
  Mat_VarFree(matvar);
 }
 dim[1]=strlen(commentCoordString);
 if(!(matvar = Mat_VarCreate("XYZComment",MAT_C_CHAR,MAT_T_UINT8,2,dim,commentCoordString,0)))
  printf("Error creating variable for 'Comment'\n");
 else
 {
  Mat_VarWrite(matfp,matvar,MAT_COMPRESSION_NONE);
  Mat_VarFree(matvar);
 }
dim[1]=strlen(commentEnergyString);
 if(!(matvar = Mat_VarCreate("EnergyComment",MAT_C_CHAR,MAT_T_UINT8,2,dim,commentEnergyString,0)))
  printf("Error creating variable for 'Comment'\n");
 else
 {
  Mat_VarWrite(matfp,matvar,MAT_COMPRESSION_NONE);
  Mat_VarFree(matvar);
 }
  
 transposedCoords=(double*)calloc(3*nCoords,sizeof(double));

 for(i=0;i<nCoords;i++)
  for(j=0;j<3;j++)
   transposedCoords[j*nCoords+i]=coords[i*3+j];
 dim[0]=nCoords;
 dim[1]=3;
 if(!(matvar = Mat_VarCreate("XYZ",MAT_C_DOUBLE,MAT_T_DOUBLE,2,dim,transposedCoords,0)))
  printf("Error creating variable for ’XYZ’\n");
 else
 {
  Mat_VarWrite(matfp,matvar,MAT_COMPRESSION_NONE);
  Mat_VarFree(matvar);
 }
 
 transposedKpoints=(double*)calloc(3*nwk, sizeof(double));
 for(i=0;i<nwk;i++)
  for(j=0;j<3;j++)
   transposedKpoints[j*nwk+i]=kPoints[i*3+j];

 dim[0]=nwk;
 if(!(matvar = Mat_VarCreate("Kpoints",MAT_C_DOUBLE,MAT_T_DOUBLE,2,dim,transposedKpoints,0)))
  printf("Error creating variable for ’Kpoints’\n");
 else
 {
  Mat_VarWrite(matfp,matvar,MAT_COMPRESSION_NONE);
  Mat_VarFree(matvar);
 }

 transposedEnergy = (double*)calloc(nEnergy,sizeof(double)); 
 for(i=0;i<data.nBands;i++)
  for(j=0;j<nwk;j++)
   transposedEnergy[j*data.nBands+i]=energy[i*nwk+j];
 dim[0] = data.nBands;
 dim[1]=nwk;
 if(!(matvar = Mat_VarCreate("SelfEnergies",MAT_C_DOUBLE,MAT_T_DOUBLE,2,dim,transposedEnergy,0)))
  printf("Error creating variable for ’SelfEnergies’\n");
 else
 {
  Mat_VarWrite(matfp,matvar,MAT_COMPRESSION_NONE);
  Mat_VarFree(matvar);
 }
 
  dim[0]=data.nBands;
 dim[1] = 1;
 
 if(!(matvar = Mat_VarCreate("BandNumbers",MAT_C_INT32,MAT_T_INT32,2,dim,data.bands,0)))
  printf("Error creating variable for ’BandNumbers’\n");
 else
 {
  Mat_VarWrite(matfp,matvar,MAT_COMPRESSION_NONE);
  Mat_VarFree(matvar);
 }
 
 if(data.eFermi != 0)
 {
 	dim[0] = 1;
  dim[1]= 1;
  if(!(matvar = Mat_VarCreate("FermiEnergy",MAT_C_DOUBLE,MAT_T_DOUBLE,2,dim,&data.eFermi,0)))
   printf("Error creating variable for ’FermiEnergy’\n");
  else
  {
   Mat_VarWrite(matfp,matvar,MAT_COMPRESSION_NONE);
   Mat_VarFree(matvar);
  }
 } 
 
 dim[0] = nCoords;
 dim[1]= nwk;
 dim[2] = data.nBands;
 if(!(matvar = Mat_VarCreate("WaveFunctionAmplitudes",MAT_C_DOUBLE,MAT_T_DOUBLE,3,dim,lDOS,0)))
  printf("Error creating variable for ’WaveFunctionAmplitudes’\n");
 else
 {
  Mat_VarWrite(matfp,matvar,MAT_COMPRESSION_ZLIB);
  Mat_VarFree(matvar);
 }


 
 spinDensity = (double*)calloc(nDensityPoints*3, sizeof(double));
 dim[3]=3;
 for(l=0;l<nDensityPoints;l++)
 {
  spinDensity[l]=lSigmaX[l];
  spinDensity[l+nDensityPoints]=lSigmaY[l];
  spinDensity[l+2*nDensityPoints]=lSigmaZ[l];  
 }
  if(!(matvar = Mat_VarCreate("PartialSpinDensity",MAT_C_DOUBLE,MAT_T_DOUBLE,4,dim,spinDensity,0)))
  printf("Error creating variable for ’PartialSpinDensity’\n");
 else
 {
  Mat_VarWrite(matfp,matvar,MAT_COMPRESSION_ZLIB);
  Mat_VarFree(matvar);
 }

  SetupSave(SetupFileName, &data );
 _END:
  if(matfp) Mat_Close(matfp);
  free(energy);
  free(lDOS);
  free(lSigmaX);
  free(lSigmaY);
  free(lSigmaZ);
  free(coords);
  free(affineCoords);
  free(kPoints);
  free(transposedCoords); 
  free(transposedKpoints);
  free(transposedEnergy);
  free(spinDensity);

  printf("Calculation time: %f s\n",GetRunTime(&initialTime));
  return 0;
}

void HelpOutput(void)
{
	printf("Calculation of lDOS(E) in the point with X,Y,Z coordinates defined\n"
		"Usage: ldos_e <Parameter>=<value> <Parameter>=<value> ...\n"
		"Parameters:\n"
		"Xmin - min X\n"
		"Xmax - max X\n"
		"nX - nunber of X points (>0)\n"
                "Ymin - min Y\n"
                "Ymax - max Y\n"
                "nY - nunber of Y points (>0)\n"
		"Zmin - min Z\n"
		"Zmax - max Z\n"
		"nZ - nunber of Z points (>0)\n"
		"InputFile - path to WAVCAR file\n"
		"OutputFile - path to output .mat file\n"
		"EFermi - Fermi energy, 0 if omitted\n"
		"Bands=n1,n2,n3,... - band numbers\n"
		);
} 
         

int ConvertToAffineCoordinates(double *r, double *s, double *output )
{
 static int n=3,np=3,indx[3];
 static double ss[9];
 int d, code=0;
 if(s)
 	{
 	 memcpy(ss,s, 9*sizeof(double));	
 	 ludcmp_(ss, &n, &np, indx, &d, &code);
   if(code) return -1;
 	} 
 memcpy(output, r, 3*sizeof(double)); 
 lubksb_(ss, &n, &np, indx, output);
 return 0;
}



void GetTime(TIME_STRUCT *timeStruct)
{
 struct timezone timeZone;
 gettimeofday(timeStruct,&timeZone);
}

double GetRunTime(TIME_STRUCT *initialTime)
{
 struct timeval currentTime;
 struct timezone timeZone;
 gettimeofday(&currentTime,&timeZone);
 return (double)(currentTime.tv_sec)+(currentTime.tv_usec*1.e-6)-
                 (double)(initialTime->tv_sec)-(initialTime->tv_usec*1.e-6);
}

