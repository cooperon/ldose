#define _FILE_IO

#include <stdio.h>
#include <stdlib.h>
#include <string.h> 
#include <float.h> 
#include <limits.h> 
#include <sys/sysinfo.h>
#include "Utils.h"
#include "FileIO.h"



 int SetupRead(char *filename, INPUT_DATA *data )
  {
	  FILE *fp=NULL;
	  int iret,i;
	  char buffer[_FILE_INPUT_BUFFER_LENGTH+1];
	  char *str, *param;
	  double dummy_f;
	  int dummy_i;
	  unsigned long dummy_l;
	  char *token;
	  
	  iret  = 0;
          *data= DEFAULT_DATA;
	  if(!(fp = fopen(filename,"r"))) {iret = -1; goto _END;}
          while(fgets(buffer, _FILE_INPUT_BUFFER_LENGTH, fp))
	  {
	   str = RemoveBlanks(buffer,0);
	   if((iret = ParameterItem(str,INPUT_KEYWORDS,
				  NUMBER_OF_INPUT_KEYWORDS, &param )) <0) continue;
     switch(iret)
		 {
		  case _Xmin:
		   if(ReadDoubleParam(param, &dummy_f)>=0) 
		    data->xMin = dummy_f;
		  break;
		
		  case _Xmax:
		    if(ReadDoubleParam(param, &dummy_f)>=0) 
		     data->xMax = dummy_f;
		  break;
		  			 
		  case _Nx:
		    if(ReadIntParam(param, &dummy_i)>=0) 
		     data->nX = dummy_i;
		  break;
		  			 			  
		  case _Ymin:
		    if(ReadDoubleParam(param, &dummy_f)>=0) 
		     data->yMin = dummy_f;
		  break;
		  
		  case _Ymax:
		    if(ReadDoubleParam(param, &dummy_f)>=0) 
		     data->yMax = dummy_f;
		   break;
		  			 
		  case _Ny:
		    if(ReadIntParam(param, &dummy_i)>=0) 
		     data->nY = dummy_i;
		  break;
		  			  
		  case _Zmin:
		    if(ReadDoubleParam(param, &dummy_f)>=0) 
		     data->zMin = dummy_f;
		  break;
		  
		  case _Zmax:
		    if(ReadDoubleParam(param, &dummy_f)>=0) 
		     data->zMax = dummy_f;
		  break;
		  			 
		  case _Nz:
		    if(ReadIntParam(param, &dummy_i)>=0) 
		     data->nZ = dummy_i;
		  break;

                  case _EFermi:
                    if(ReadDoubleParam(param, &dummy_f)>=0)
                     data->eFermi = dummy_f;
                  break;

                  case _Nthreads:
                    if(ReadIntParam(param, &dummy_i)>=0)
                     data->nThreads = dummy_i;
                  break;
		  			  
		  case _InputFile:
		    data->inputFile=(char*)realloc(data->inputFile, sizeof(char)*strlen(param)); 
		    data->filenameLength = strlen(param);
                    strcpy(data->inputFile, param);
		  break;
      
		  case _OutputFile:
		     data->outputFile=(char*)realloc(data->outputFile, sizeof(char)*strlen(param));
                     strcpy(data->outputFile, param);
		  break;
      
		  
		  case _Bands:
	   token = strtok(param, " ,;#$_%&");
       while( token != NULL ) 
       {
       	if(ReadIntParam(token, &dummy_i)>=0) 
       	{
         data->bands = (int*)realloc(data->bands, (data->nBands+1)*sizeof(*data->bands) );
         data->bands[data->nBands]= dummy_i;
         data->nBands ++;
        }
        token = strtok(NULL, " ,;#$_%&");
       }
      break;
      
		  default: break;
		 }  
		} 
_END:
	  if(iret) 		
	  if(fp) fclose(fp);
	  return iret;
  }

int SetupSave(char *filename, INPUT_DATA *data )
{
 int i;
 FILE *fp = fopen(filename,"w");
 if(!fp)
  return -1;
 
 fprintf(fp,"%s=%le\n",INPUT_KEYWORDS[_Xmin], data->xMin);
 fprintf(fp, "%s=%le\n", INPUT_KEYWORDS[_Xmax], data->xMax);
 fprintf(fp, "%s=%d\n", INPUT_KEYWORDS[_Nx], data->nX);
 fprintf(fp,"%s=%le\n",INPUT_KEYWORDS[_Ymin], data->yMin);
 fprintf(fp, "%s=%le\n", INPUT_KEYWORDS[_Ymax], data->yMax);
 fprintf(fp, "%s=%d\n", INPUT_KEYWORDS[_Ny], data->nY);
 fprintf(fp,"%s=%le\n",INPUT_KEYWORDS[_Zmin], data->zMin);
 fprintf(fp, "%s=%le\n", INPUT_KEYWORDS[_Zmax], data->zMax);
 fprintf(fp, "%s=%d\n", INPUT_KEYWORDS[_Nz], data->nZ);
 fprintf(fp,"%s=%s\n", INPUT_KEYWORDS[_InputFile], data->inputFile);
 fprintf(fp,"%s=%s\n", INPUT_KEYWORDS[_OutputFile], data->outputFile);
 fprintf(fp,"%s=%d\n", INPUT_KEYWORDS[_Nthreads], data->nThreads);
 fprintf(fp,"%s=",INPUT_KEYWORDS[_Bands]);
 for (i = 0; i < data->nBands; i++) 
   fprintf(fp, " %d", data->bands[i]);
 fprintf(fp, "\n");
 if(data->eFermi != 0.)
  fprintf(fp, "%s=%le\n", INPUT_KEYWORDS[_EFermi], data->eFermi);
 fclose(fp);
 return 0;
}


void ParceCommandString(int argc, char *argv[],INPUT_DATA *data)
{
 int  i, iret;
 char *str, *param, *token;
 double dummy_f;
 int dummy_i;
 int bandsFlag=0;


 if(argc ==1) return;
 for(i=1;i<argc;i++)
 {
  str = RemoveBlanks(argv[i],0);
  if((iret = ParameterItem(str,INPUT_KEYWORDS,
   NUMBER_OF_INPUT_KEYWORDS, &param )) <0) continue;
  switch(iret)
  {
  case _Xmin:
		   if(ReadDoubleParam(param, &dummy_f)>=0) 
		    data->xMin = dummy_f;
		  break;
		
		  case _Xmax:
		    if(ReadDoubleParam(param, &dummy_f)>=0) 
		     data->xMax = dummy_f;
		  break;
		  			 
		  case _Nx:
		    if(ReadIntParam(param, &dummy_i)>=0) 
		     data->nX = dummy_i;
		  break;
		  			 			  
		  case _Ymin:
		    if(ReadDoubleParam(param, &dummy_f)>=0) 
		     data->yMin = dummy_f;
		  break;
		  
		  case _Ymax:
		    if(ReadDoubleParam(param, &dummy_f)>=0) 
		     data->yMax = dummy_f;
		   break;
		  			 
		  case _Ny:
		    if(ReadIntParam(param, &dummy_i)>=0) 
		     data->nY = dummy_i;
		  break;
		  			  
		  case _Zmin:
		    if(ReadDoubleParam(param, &dummy_f)>=0) 
		     data->zMin = dummy_f;
		  break;
		  
		  case _Zmax:
		    if(ReadDoubleParam(param, &dummy_f)>=0) 
		     data->zMax = dummy_f;
		  break;
		  			 
		  case _Nz:
		    if(ReadIntParam(param, &dummy_i)>=0) 
		     data->nZ = dummy_i;
		  break;

                  case _EFermi:
                    if(ReadDoubleParam(param, &dummy_f)>=0)
                     data->eFermi = dummy_f;
                  break;

                  case _Nthreads:
                    if(ReadIntParam(param, &dummy_i)>=0)
                     data->nThreads = dummy_i;
                  break;
 
   case _InputFile:
    data->inputFile=(char*)realloc(data->inputFile, sizeof(char)*strlen(param));
    strcpy(data->inputFile, param);
    data->filenameLength = strlen(param);
   break;

   case _OutputFile:
    data->outputFile=(char*)realloc(data->outputFile, sizeof(char)*strlen(param));
    strcpy(data->outputFile, param);
   break;

   
   case _Bands:
   	if(!bandsFlag)
   	{
   	 free (data->bands);
   	 data->bands = NULL;
   	 data->nBands=0;
   	 bandsFlag=1; 
   	}
	token = strtok(param, ",;#$_%&");
    while( token != NULL ) 
    {
     if(ReadIntParam(token, &dummy_i)>=0) 
     {
      data->bands = (int*)realloc(data->bands, (data->nBands+1)*sizeof(*data->bands) );
      data->bands[data->nBands]= dummy_i;
      data->nBands ++;
     }
     token = strtok(NULL, ",;#$_%&");
    }
   break;

   default: break;
  } 
 }
 return;
}


int VerifyInputData(INPUT_DATA *data)
{
 int iret = 0;
 if(data->xMin ==DBL_MAX)
 {
  printf("Xmin parameter have not defined\n");
  iret++;
 }
 if(data->xMax ==DBL_MAX) 
 {
  printf("xmax parameter have not defined\n");
  iret++;
 }
 if(data->yMin ==DBL_MAX)
 {
  printf("Ymin parameter have not defined\n");
  iret++;
 }
 if(data->yMax ==DBL_MAX) 
 {
  printf("Ymax parameter have not defined\n");
  iret++;
 }
 if(data->zMin ==DBL_MAX)
 {
  printf("Zmin parameter have not defined\n");
  iret++;
 }
 if(data->zMax ==DBL_MAX) 
 {
  printf("Zmax parameter have not defined\n");
  iret++;
 }
 
 if(data->nX <=0) 
 {
  printf("Nx parameter have not defined\n");
  iret++;
 }
 if(data->nY <=0) 
 {
  printf("Ny parameter have not defined\n");
  iret++;
 }
 
 if(data->nZ <=0) 
 {
  printf("Nz parameter have not defined\n");
  iret++;
 }
 
 if(data->inputFile ==NULL)
 {
  printf("InputFile parameter have not defined\n");
  iret++;
 }
 else
  if(CheckFileStatus(data->inputFile))
  {
   printf("File %s can't be opened\n", data->inputFile);
   iret++;
  }
 
 if (data->bands == NULL)
 {
	 printf("No bands defined\n");
	 iret++;
 }

 if (data->nThreads <= 0 || data->nThreads> get_nprocs())
 {
         printf("Wrong number ofThreads defined\n");
         iret++;
 }


 if(data->outputFile ==NULL)
 {
  printf("OutputFile parameter have not defined\n");
  iret++;
 }
 else
 {
  switch(CheckFileStatus(data->outputFile))
  {
    case -1:
     printf("File %s can't be created or opened\n", data->outputFile);
     iret++;
    break;
    case  0:
     printf("File %s exists, overrite it ? (y/n)", data->outputFile);
     for(;;)
     {
      switch(getchar())
      {
       case 'y': case 'Y':
       break;
       case 'n': case 'N':
        iret++;
       break;
       default:
       continue; 
      }
      break;
     }
    break;
    case 1:
    break;
  }
 }

 return iret;
}
