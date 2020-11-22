#define _UTILS

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#include "Utils.h"


/***********************************************************************
 *                                                                     *
 *     Removes blanks at the beginning and at the end of string        *
 *                                                                     *
 *     Input parameters:                                               *
 *                       char *str - string                            *
 *                       int r - 0 - remove blanks at the beginning    *
 *                                   and at the end                    *
 *                               1 - remove blanks at the beginning    *
 *                               2 - remove blanks at the end          *
 *                                                                     *
 *     Return value:                                                   *
 *                  char*str                                           *
 *                                                                     *
 ***********************************************************************/

 char *RemoveBlanks(char *str,int r)
  {
   unsigned int i;
   if(!str[0]) return str;
   switch(r)
    {
     case 0:
      {
       for(i=strlen(str)-1;i!=0;i--)
       if(isspace(str[i])) str[i]=0;
         else break;
       for(i=0;i<strlen(str);i++)
        if(isspace(str[0])) str++;
        else break;
      }
     break;

     case 1:
      {
       for(i=0;i<strlen(str);i++)
        if(isspace(str[0])) str++;
        else break;
      }
     break;

     case 2:
      {
       for(i=strlen(str)-1;i!=0;i--)
       if(isspace(str[i] )) str[i]=0;
       else break;
      }
     break;

     default:
      break;
    }
   return str;
  }


/***********************************************************************
 *                                                                     *
 *           Data saving in ASCII format                               *
 *                                                                     *
 *     Input parameters:                                               *
 *                       char *FileName - file name                    *
 *                       int  ColsNumber - number of cols              *
 *                       long  RowsNumber - number of rows             *
 *                       double **data - data array                    *
 *                                                                     *
 *     Return value:                                                   *
 *                   0 - sucsessfull exit                              *
 *                  -1 - can't open file                               *
 *                  -2 - error during data recording                   *
 *                  -3 - error in input parameters                     *
 *                                                                     *
 ***********************************************************************/
int ASCII_DataSave(char*FileName, int ColsNumber, long RowsNumber, double **data)
 {
  FILE *fp;
  long i;
  int j,iret;
  iret = 0;
  fp = NULL;


  iret = 0;
  if((FileName == NULL) || (ColsNumber == 0) || (RowsNumber == 0) || (data == NULL))
   {
    iret = -3; goto _END;
   }
  fp = fopen(FileName,"w");
  if(fp == NULL) {iret = -1; goto _END;}
  for(i=0; i<RowsNumber; i++)
   {
    for(j=0;j<ColsNumber;j++)
      { if(fprintf(fp,"%14.6le ",(data[j])[i]) <=0) { iret = -2; goto _END;} }
    fprintf(fp,"\n");
   }

 _END:
   if(fp != NULL) fclose(fp);
   return iret;
 }


/***********************************************************************
 *                                                                     *
 *           Data saving in binary format 666                          *
 *                                                                     *
 *     Input parameters:                                               *
 *                       char *FileName - file name                    *
 *                       int  ColsNumber - number of cols              *
 *                       long  RowsNumber - number of rows             *
 *                       char **ColNames   - array of col names        *
 *                       double **data - data array                    *
 *                                                                     *
 *     Return value:                                                   *
 *                   0 - sucsessfull exit                              *
 *                  -1 - can't open file                               *
 *                  -2 - error during data recording                   *
 *                  -3 - error in input parameters                     *
 *                                                                     *
 ***********************************************************************/
int Binary_666_DataSave(char*FileName, int ColsNumber, long RowsNumber,
                            char **ColNames,double **data, int fast_flag)
 {
  FILE *fp;
  long i,ll;
  int j,iret,l;
  char* data_array;
  double *d_ptr;

  data_array = NULL;
  iret = 0;
  fp = NULL;
  if((FileName == NULL) || (ColsNumber <= 0) || (RowsNumber <=0) ||
     (ColNames == NULL) || (data == NULL)) {iret = -3; goto _END;}
  for(j=0;j<ColsNumber;j++)
    { if(ColNames[j] == NULL) { iret = -3; goto _END;} }
  fp = fopen(FileName,"wb");
  if(fp == NULL) { iret = -1; goto _END; }

 /* Header of binary file */
  if(fwrite("666",sizeof(char),3,fp)<=0) {iret = -2; goto _END;}  /* Identifier */
  if(fwrite((short*)&ColsNumber,sizeof(short),1,fp)<=0) { iret = -2; goto _END; }   /* ColsNumber */
  l=4;                 /* length of data in each col - double, type 4 */
  for(j=0;j<ColsNumber;j++)
   {
    if(fwrite((short*)&l,sizeof(short),1,fp)<=0){iret = -2; goto _END;}
   }
  for(j=0;j<ColsNumber;j++)
   {
    l = strlen(ColNames[j]);
    if(fwrite((short*)&l,sizeof(short),1,fp)<=0) {iret = -2; goto _END;}
    if(fwrite(ColNames[j],sizeof(char),l,fp)<=0) { iret = -2; goto _END;}
   }
   /* Data output */
  if(fast_flag)
   {
    data_array = (char*)calloc(RowsNumber*ColsNumber*sizeof(double),sizeof(char));
    if(data_array == NULL) {iret = -100;goto _END;}
    ll=0;
    d_ptr = (double*)data_array;
    for(i=0;i<RowsNumber;i++)
     {
      for(j=0;j<ColsNumber;j++)
       {
        *d_ptr = *(data[j]+i);
        d_ptr+=1;
       }
     }
    if(fwrite(data_array,RowsNumber*ColsNumber*sizeof(double),1,fp)<=0) { iret = -2; goto _END;}
   }
  else
   {
    data_array = (char*)calloc(ColsNumber*sizeof(double),sizeof(char));
    if(data_array == NULL) {iret = -100;goto _END;}
    d_ptr = (double*)data_array;
    for(i=0;i<RowsNumber;i++)
     {
      for(j=0;j<ColsNumber;j++)
       {
        *(d_ptr+j) = *(data[j]+i);
       }
      if(fwrite(data_array,ColsNumber*sizeof(double),1,fp)<=0) { iret = -2; goto _END;}
     }
   }

  _END:
    if(fp != NULL) fclose(fp);
    free(data_array);
    return iret;
 }



/***********************************************************************
 *                                                                     *
 *           Data saving in binary format 777                          *
 *                                                                     *
 *     Input parameters:                                               *
 *                       char *FileName - file name                    *
 *                       int  ColsNumber - number of cols              *
 *                       long  RowsNumber - number of rows             *
 *                       char **ColNames   - array of col names        *
 *                       double **data - data array                    *
 *                       double TimeInterval - time interval (in sec)  *
 *                       int *ChannelIndexes - ADC channels,           *
 *                                            corresponding columns    *
 *                       int *DataTypes - data types:                  *
 *                                         0-1 byte int                *
 *                                         1-2 byte int                *
 *                                         2-4 byte int                *
 *                                         3-4 byte float              *
 *                                         4-8 byte float              *
 *                       All int are supposed as unsigned              *
 *                       int *ChannelIndexes - ADC channel             *
 *                       double *a_coeffs                              *
 *                       double *b_coeffs - coefficients to transform  *
 *                                          data to double             *
 *                                Y=a*X+b                              *
 *                       int fast_flag  0 - slow output, low memory    *
 *                                      1 - fast output, large memory  *
 *                                                                     *
 *     Return value:                                                   *
 *                   0 - sucsessfull exit                              *
 *                  -1 - can't open file                               *
 *                  -2 - error during data recording                   *
 *                  -3 - error in input parameters                     *
 *                                                                     *
 ***********************************************************************/
int Binary_TS_V2_DataSave(char*FileName, long ColsNumber, long RowsNumber,
						double TimeInterval,int *DataTypes,
                        int *ChannelIndexes, double *a_coeffs, double *b_coeffs,
						char **ColNames,void **data)
 {
  FILE *fp;
  int j,iret;
  char c;
  short  si;

  iret = 0;
  fp = NULL;
  if((FileName == NULL) || (ColsNumber <= 0) || (RowsNumber <=0) ||
     (ColNames == NULL) || (data == NULL)) {iret = -3; goto _END;}
  for(j=0;j<ColsNumber;j++)
    { if(ColNames[j] == NULL) { iret = -3; goto _END;} }
  fp = fopen(FileName,"wb");
  if(fp == NULL) { iret = -1; goto _END; }

 /* Header of binary file */
  if(fwrite("TS_V2",sizeof(char),5,fp)<=0) {iret = -2; goto _END;}  /* Identifier */
  if(fwrite(&ColsNumber,sizeof(long),1,fp)<=0) { iret = -2; goto _END; }   /* ColsNumber */
  if(fwrite(&RowsNumber,sizeof(long),1,fp)<=0) { iret = -2; goto _END; }   /* ColsNumber */
  if(fwrite(&TimeInterval,sizeof(double),1,fp)<=0) { iret = -2; goto _END; }   /* ColsNumber */
  for(j=0;j<ColsNumber;j++)
   {
	c=(char)DataTypes[j];
    if(fwrite(&c,sizeof(c),1,fp)<=0){iret = -2; goto _END;}
    si = (short)strlen(ColNames[j]);
    if(fwrite(&si,sizeof(si),1,fp)<=0) {iret = -2; goto _END;}
    if(fwrite(ColNames[j],strlen(ColNames[j]),1,fp)<=0) {iret = -2; goto _END;}
    if(fwrite(&(a_coeffs[j]),sizeof(*a_coeffs),1,fp)<=0) { iret = -2; goto _END;}
    if(fwrite(&(b_coeffs[j]),sizeof(*b_coeffs),1,fp)<=0) { iret = -2; goto _END;}
	c=(char)ChannelIndexes[j];
    if(fwrite(&c,sizeof(c),1,fp)<=0){iret = -2; goto _END;}
   }
    /* Data output */
  for(j=0;j<ColsNumber;j++)
  {
   switch(DataTypes[j])
   {
	case _1_BYTE_DATA:
     if(fwrite(data[j],RowsNumber,1,fp)<=0) { iret = -2; goto _END;}
    break;

	case _2_BYTE_DATA:
     if(fwrite(data[j],RowsNumber*2,1,fp)<=0) { iret = -2; goto _END;}
    break;

	case _4_BYTE_DATA: case _4_FLOAT_DATA:
     if(fwrite(data[j],RowsNumber*4,1,fp)<=0) { iret = -2; goto _END;}
    break;

	case _8_FLOAT_DATA:
     if(fwrite(data[j],RowsNumber*8,1,fp)<=0) { iret = -2; goto _END;}
    break;

	default:
	 iret = -3;
	goto _END;
   }
  }
  _END:
    if(fp != NULL) fclose(fp);
  return iret;
 }




/***********************************************************************
 *                                                                     *
 *            Check file status                                        *
 *                                                                     *
 *   Input parameters:                                                 *
 *                    char *FileName - file name                       *
 *                                                                     *
 *   Return value:                                                     *
 *                 -1 - file can't be created or opened                *
 *                  0 - file exists and can be opened                  *
 *                  1 - file doesnot exist but can be created          *
 *                                                                     *
 ***********************************************************************/

int CheckFileStatus(char*FileName)
 {
  FILE *fp;
  int iret;
  iret = -1;
  if(FileName == NULL) goto _END;
  if(FileName[0] == (char) 0) goto _END;
  fp = NULL;
  fp = fopen(FileName,"r");
  if(fp != NULL) { fclose(fp); iret = 0; goto _END;}
  fp = fopen(FileName,"w");
  if(fp != NULL)
   {
    iret = 1;
    fclose(fp); remove(FileName);
   }

  _END:
     return iret;
 }


/*************************************************************************
 *                                                                       *
 *                     Information about data file                       *
 *                                                                       *
 *    Input parameters:                                                  *
 *                     char* filename - name of file                     *
 *                                                                       *
 *    Output parameters:                                                 *
 *                     int *type - -1 - not exists 0 - ASCII,            *
 *                                  1-binary 666                         *
 *                                  2-binary 777                         *
 *                                  3-unknown                            
 *                     int *NCols - number of coulumns                   *
 *                     long NRows - number of rows                       *
 *                     char** ColNames - names of coulumns               *
 *                                                                       *
 *    Return value:                                                      *
 *                   0 - sucsessfull exit                                *
 *                   -100 - unsufficient memory                          *
 *                                                                       *
 *************************************************************************/

int DataFileInfo(char *filename, int *type, int *NCols, long *NRows,  
				 char ***ColNames, int **ChannelIndexes, double *time)
{
	FILE *fp;
	int iret;
	unsigned int i,j;
	int n_col,flag,flag1,length;
	long l;
	char *buffer, *b;
	int *ColTypes;
	char ID[10];
	FILE_HEADER_TS_V2 header;
	
	iret = 0;
	fp = NULL;
	buffer = NULL;
	*NCols  = 0;
	*NRows = 0;
	*ColNames = NULL;
	ColTypes = NULL;
	buffer = (char*)calloc(_MAX_FILE_INPUT_BUFFER_LENGTH+1,sizeof(*buffer));
	if(buffer == NULL) { iret = -100; goto _END;}
	fp = fopen(filename,"rb");
	if(fp == NULL)  {*type = -1; goto _END;}
	if(!fgets(ID, 9, fp)) {*type = _UNKNOWN_FILE; goto _END;}
	if(!strncmp(ID, "666",3))   /* Type 666 */
	{
		flag = 0;
		iret = BinaryFileHeaderRead(fp, NCols,NULL,ColNames,&ColTypes);
		if(iret == -100) goto _END;
		if((iret == 0)&&(*NCols > 0)&&(*ColNames != NULL))
		{
			flag1 = 1;
			for(i=0;i<(unsigned int)*NCols;i++)
			{
				if((*ColNames)[i] == NULL) {flag1 = 0; break;}
				for(j=0;j<strlen((*ColNames)[i]);j++)
				{
					if((isprint(((*ColNames)[i])[j]) == 0)&&(isspace(((*ColNames)[i])[j]) == 0))
					{ flag1 = 0; break;}
				}
				if(flag1 == 0) break;
			}
			if(flag1 != 0) flag =1;
		}
		if(flag == 1)
		{
			length = 0;
			for(i=0;i<(unsigned int)*NCols;i++)
			{
				switch(ColTypes[i])
				{
				case 1:
					length += sizeof(short);
					break;
				case 2:
					length += sizeof(int);
					break;
				case 3:
					length += sizeof(float);
					break;
				case 4:
					length += sizeof(double);
					break;
				}
			}
			
			b = (char*)calloc(length, sizeof(*b));
			if (b == NULL) {iret = -100; goto _END;}
			
			l=0;
			for(;;)
			{
				if (fread(b,length,1,fp) <= 0) break;
				l++;
			}
			free(b);
			if(iret == 0)
			{
				*type = _BINARY_666_DATA_FILE;
				*NRows = l;
				goto _END;
			}
		}
	}
	
	if(!strncmp(ID, "TS_V2",5))   /* Type TS_V2 */
	{
		iret = BinaryTS_V2FileHeaderRead(fp, NCols,NRows,ColNames,NULL,NULL,&header);
		if(iret == -100) goto _END;
		if((iret == 0)&&(*NCols > 0)&&(*NRows > 0)&&(*ColNames != NULL))
		{
			*type = _BINARY_TS_V2_DATA_FILE;
			if(ChannelIndexes)
			{
				if(!(*ChannelIndexes = (int*)calloc(*NCols, sizeof(int))))
				{iret = -100; goto _END;}
				for(i=0;i<(unsigned)*NCols;i++) 
					(*ChannelIndexes)[i] = header.ColHeaders[i]->chan_index;
			}
			if(time) *time = header.time_interval;
			goto _END;
		}
	}
	fclose(fp); fp = NULL; iret = 0;
	
	fp = fopen(filename,"r");
	if(fp == NULL) { *type = -1; goto _END;}
	
	if(fgets(buffer, _MAX_FILE_INPUT_BUFFER_LENGTH,fp) == NULL) { *type = 2; goto _END;}
	for(i=0;i<strlen(buffer);i++)
	{
		if((isprint(buffer[i]) == 0)&&(isspace(buffer[i]) == 0))
		{ *type = _UNKNOWN_FILE; goto _END;}
	}
	
	iret = ReadNumString(buffer, 0, NULL, NCols);
	
	if(iret != 0) goto _END;
	if (*NCols == 0) { *type = _UNKNOWN_FILE; goto _END;}
	l=1;

	while(fgets(buffer, _MAX_FILE_INPUT_BUFFER_LENGTH,fp) != NULL)
	{
		iret = ReadNumString(buffer, 0, NULL, &n_col);
		if(iret != 0)
		{
			goto _END;
		}
		
		if(*NCols != n_col)
			if((buffer[0] =='\n')||(buffer[0] == '\r')) continue;
			else { *type = _UNKNOWN_FILE; goto _END;}
			
			l++;
	}
	*type = _ASCII_DATA_FILE;
	*NRows = l;
	
_END:
	if(fp != NULL) fclose(fp);
	free(buffer);
	if((iret != 0) || (*type == -1) || (*type == _UNKNOWN_FILE))
	{
		if(*ColNames != NULL)
			for(i=0;i<(unsigned int)*NCols;i++) free((*ColNames)[i]);
			free(*ColNames); *ColNames = NULL;
			*NCols = 0; *NRows = 0;
	}
	return iret;
}

/*************************************************************************
 *                                                                       *
 *                     Binary File Header 666                                *
 *                                                                       *
 *    Input parameters:                                                  *
 *                     FILE *fp - pointer to file (opened as rb)         *
 *                     int flag - 1 - output  ColNames, ColTypes         *
 *                                0 - don't output ColNames,ColTypes     *
 *                                                                       *
 *    Output parameters:                                                 *
 *                      int *NCols - number of coulumns                  *
 *                      long *NRows - number of rows (for future file    *
 *                                          formats                      *
 *                      char ***ColNames = Array of coulumn names        *
 *                                       ( must be freed in colling      *
 *                                         programm)                     *
 *                      int **ColTypes - Coulumn types                    *
 *                                       ( must be freed in colling      *
 *                                         programm)                     *
 *                                                                       *
 *    Return value:                                                      *
 *                   0 - sucsessfull exit                                *
 *                  -1 - wrong file type                                 *
 *                  -2 - error in reading                                *
 *                  -100 - unsufficient memory                           *
 *                                                                       *
 *  After this program exit pointer is set to the beginning of data      *
 *                                                                       *
 *************************************************************************/
 int BinaryFileHeaderRead(FILE*fp, int *NCols, long *NRows,char ***ColNames,int**ColTypes)
  {
   int iret,n_col,len,i;
   long n_row;
   char **names;
   int *types;
   char buffer[_FILE_INPUT_BUFFER_LENGTH+1];
   short s;

   iret = 0;
   types = NULL; names = NULL;
   n_col = 0; n_row = 0;
   rewind(fp);

   if(fread(buffer,sizeof(char),3,fp)<=0) { iret = -2; goto _END;}  /* Identifier */
   buffer[3] = (char)0;
   if(strcmp(buffer,"666") != 0) { iret = -1; goto _END;}
   if(fread(&s,sizeof(s),1,fp)<=0){ iret = -2; goto _END;}    /* ColsNumber */
   n_col = (int)s;
    if((n_col<=0)||(n_col > 1000)) {iret = -1; goto _END;}
                     /* length of data in each col - double, type 4 */
   types = (int*)calloc(n_col,sizeof(*types));
   if(types == NULL) {iret = -100; goto _END;}
   names = (char**)calloc(n_col,sizeof(*names));
   if(names == NULL) {iret = -100; goto _END;}

   for(i=0;i<n_col;i++)
    {
     if(fread(&s,sizeof(s),1,fp)<=0) {iret = -2; goto _END;}
     /* if(len !=4)  {iret = -1; goto _END;} */
     types[i] = (int)s;
    }
   for(i=0;i<n_col;i++)
    {
     if(fread(&s,sizeof(s),1,fp)<=0) {iret = -2; goto _END;}
     len = (int)s;
     if( (len <0) || (len >=1000)) {iret = -1; goto _END;}
     names[i] = (char*)calloc(len+1,sizeof(char));
     if(names[i] == NULL) {iret = -100; goto _END;}
     if(fread(names[i],sizeof(char),len,fp)<=0) {iret = -2; goto _END;}
     (names[i])[len] = (char)0;
    }

   if(ColNames ==NULL)
    {
     if(names != NULL)
      for(i=0;i<n_col;i++)
       free(names[i]);
      free(names); names = NULL;
    }
   if (ColTypes == NULL)
    {
      free(types); types = NULL;
    }

  _END:
   if(iret == 0)
    {
     if(NCols != NULL) *NCols = n_col;
     if(NRows != NULL) *NRows = n_row;
     if(ColNames != NULL) *ColNames = names;
     else
      {
       if(names != NULL)
        {
         for(i=0;i<n_col;i++) free(names[i]);
          free(names);
        }
      }
     if(ColTypes != NULL) *ColTypes = types;
     else free(types);
    }
   else
    {
     if (names != NULL)
      { for(i=0;i<n_col;i++) free(names[i]); }
     free(names); free(types);
     *NCols = 0;
     if(ColNames != NULL) *ColNames = NULL;
     if(ColTypes != NULL) *ColTypes = NULL;
    }
   return iret;
  }

/*************************************************************************
 *                                                                       *
 *                     Binary File Header   TS_V2                        *
 *                                                                       *
 *    Input parameters:                                                  *
 *                     FILE *fp - pointer to file (opened as rb)         *
 *                                                                       *
 *    Output parameters:                                                 *
 *                      int *NCols - number of coulumns                  *
 *                      long *NRows - number of rows (for future file    *
 *                                          formats                      *
 *                      char ***ColNames = Array of coulumn names        *
 *                                       ( must be freed in colling      *
 *                                         programm)                     *
 *                      int **ColTypes - Coulumn types                   *
 *                                       ( must be freed in colling      *
 *                                         programm)                     *
 *                                                                       *
 *    Return value:                                                      *
 *                   0 - sucsessfull exit                                *
 *                  -1 - wrong file type                                 *
 *                  -2 - error in reading                                *
 *                  -100 - unsufficient memory                           *
 *                                                                       *
 *  After this program exit pointer is set to the beginning of data      *
 *                                                                       *
 *************************************************************************/
 int BinaryTS_V2FileHeaderRead(FILE*fp, int *NCols, long *NRows,char ***ColNames,int**ColTypes,
	 double *time_interval, FILE_HEADER_TS_V2 *header)
 {
	 int iret,n_col,len,i;
	 long n_row;
	 char **names;
	 int *types;
	 char buffer[_FILE_INPUT_BUFFER_LENGTH+1];
	 short s;
	 long ss;
	 double t;
	 char c;
	 
	 iret = 0;
	 types = NULL; names = NULL;
	 n_col = 0; n_row = 0;
	 rewind(fp);
	 if(fread(buffer,sizeof(char),5,fp)<=0) { iret = -2; goto _END;}  /* Identifier */
	 buffer[5] = (char)0;
	 if(strcmp(buffer,"TS_V2") != 0) { iret = -1; goto _END;}
	 if(fread(&ss,sizeof(ss),1,fp)<=0){ iret = -2; goto _END;}    /* ColsNumber */
	 n_col = ss;
	 if((n_col<=0)||(n_col > 1000)) {iret = -1; goto _END;}
	 /* length of data in each col - double, type 4 */
	 types = (int*)calloc(n_col,sizeof(*types));
	 if(types == NULL) {iret = -100; goto _END;}
	 names = (char**)calloc(n_col,sizeof(*names));
	 if(names == NULL) {iret = -100; goto _END;}
	 if(fread(&ss,sizeof(ss),1,fp)<=0){ iret = -2; goto _END;}    /* ColsNumber */
	 n_row = ss;
	 if(n_row<=0) {iret = -1; goto _END;}
	 if(fread(&t,sizeof(t),1,fp)<=0){ iret = -2; goto _END;}    /* time_interval*/
	 
	 if(header)
	 {
		 header->cols_number = n_col;
		 header->rows_number = n_row;
		 header->time_interval = t;
		 if(!(header->ColHeaders = (COL_HEADER**)calloc(n_col,sizeof(*header->ColHeaders))))
			 for(i=0;i<n_col;i++)
			 {
				 if(!(header->ColHeaders[i] = (COL_HEADER*) calloc(1, sizeof(COL_HEADER))))
				 {
					 iret = -100; goto _END;
				 }
			 }
	 }
	 for(i=0;i<n_col;i++)
	 {
		 if(fread(&c,sizeof(c),1,fp)<=0) {iret = -2; goto _END;}
		 types[i] = (int)c;
		 if(header) header->ColHeaders[i]->type = types[i];
		 if(fread(&s,sizeof(s),1,fp)<=0) {iret = -2; goto _END;}
		 len = (int)s;
		 if( (len <0) || (len >=1000)) {iret = -1; goto _END;}
		 names[i] = (char*)calloc(len+1,sizeof(char));
		 
		 if(names[i] == NULL) {iret = -100; goto _END;}
		 if(fread(names[i],sizeof(char),len,fp)<=0) {iret = -2; goto _END;}
		 (names[i])[len] = (char)0;
		 if(header)
		 {
			 if(!(header->ColHeaders[i]->name = (char*)calloc(strlen(names[i])+1,sizeof(char))))
			 {
				 iret = -100; goto _END;
			 }
			 strcpy(header->ColHeaders[i]->name, names[i]);
		 }
		 
		 if(fread(&t,sizeof(t),1,fp)<=0){ iret = -2; goto _END;}    /* a_coeff */
		 
		 if(header) header->ColHeaders[i]->a = t;
		 
		 if(fread(&t,sizeof(t),1,fp)<=0){ iret = -2; goto _END;}    /* b_coeff */
		 if(header) header->ColHeaders[i]->b = t;
		 
		 if(fread(&c,sizeof(c),1,fp)<=0){ iret = -2; goto _END;}    /* chan_index */
		 if(header) header->ColHeaders[i]->chan_index = (int)c;
	 }
	 
	 if(ColNames ==NULL)
	 {
		 if(names != NULL)
			 for(i=0;i<n_col;i++)
				 free(names[i]);
			 free(names); names = NULL;
	 }
	 if (ColTypes == NULL)
	 {
		 free(types); types = NULL;
	 }
	 
_END:
	 if(iret == 0)
	 {
		 if(NCols != NULL) *NCols = n_col;
		 if(NRows != NULL) *NRows = n_row;
		 if(ColNames != NULL) *ColNames = names;
	 }
	 else
	 {
		 if (names != NULL)
		 { for(i=0;i<n_col;i++) free(names[i]); }
		 free(names); free(types);
		 *NCols = 0;
		 if(ColNames != NULL) *ColNames = NULL;
		 if(ColTypes != NULL) *ColTypes = NULL;
		 if(header)
			 if(header->ColHeaders)
			 {
				 for(i=0;i<n_col;i++) free(header->ColHeaders[i]);
				 free(header->ColHeaders);
			 }
	 }
	 return iret;
}
	 


/*************************************************************************
 *                                                                       *
 *              Numerical string translation to double array             *
 *  Numbers have to be separated by spaces                               *
 *                                                                       *
 *    Input parameters:                                                  *
 *                     char*string - numerical string                    *
 *                     int flag - 0 -don't output number_array           *
 *                                1 - output number array                *
 *                                                                       *
 *    Output parameters:                                                 *
 *                      double **number_array - array of numbers         *
 *                      (must be freed in colling procedure)             *
 *                      int *NCols - number of coulumns                  *
 *                                                                       *
 *    Return value:                                                      *
 *                      0 - sucsessfull exit                             *
 *                      -100 - unsufficient memory                       *
 *                                                                       *
 *************************************************************************/
int ReadNumString(char* String, int flag, double**number_array, int *NCols)
 {
  int i,n,iret;
  double aa,*dat;
  char *str;

  if(flag == 1) *number_array = NULL;
  dat = NULL;
  *NCols = 0;
  str = String;
  n=0; i = 0;
  iret = 0;

  if(String == NULL) return 0;


  for(;;)
   {
    str = str+n;
    if(sscanf(str,"%le%n",&aa,&n)<=0)break;
    if(flag == 1)
     {
      dat = (double*)realloc(dat,(i+1)*sizeof(*dat));
      if(dat == NULL) {iret = -100; goto _END;}
      dat[i] = aa;
     }
    i++;
   }
 _END:
  if(iret == 0)
   {
    if(flag == 1) *number_array = dat;
    if(NCols != NULL) *NCols = i;
   }
  return iret;
 }


/*************************************************************************
 *                                                                       *
 *              Data file read                                           *
 *  Numbers have to be separated by spaces                               *
 *                                                                       *
 *    Input parameters:                                                  *
 *                     char*filename - file name                         *
 *                     int type - 0 - ASCII                              *
 *                                1 - binary 666                            *
 *                     int ncol - number of coulumns                     *
 *                     long nrow - number of rows                        *
 *                                                                       *
 *    Output parameters:                                                 *
 *                      double       **data - data array                 *
 *                                                                       *
 *    Return value:                                                      *
 *                      0 - sucsessfull exit                             *
 *                      -100 - unsufficient memory                       *
 *                      -1 - file cannot be opened                       *
 *                      -2 - wrong coulumns number                       *
 *                      -3 - wrong rows number                           *
 *                      -4 - wrong binary file type                      *
 *                      -5 - error in file reading                       *
 *                                                                       *
 *************************************************************************/
int DataFileRead(char *filename, int type,  int ncol, long nrow, double **data)
 {
  int i,j,ii,iret;
  long l;
  FILE *fp;
  char *buffer;
  double *dat;
  int *DataTypes;
  char *binary_buf;
  int length;
  char *c_ptr;
  FILE_HEADER_TS_V2 header;
  void *col_buf;
  size_t size;


  dat = NULL; fp = NULL; binary_buf = NULL;
  DataTypes = NULL;
  col_buf = NULL; buffer = NULL;
  if(filename == NULL) return 0;

  iret = 0;

  switch(type)
   {
    case _ASCII_DATA_FILE:
     fp = fopen(filename,"r");
     if(fp == NULL) {iret = -1; goto _END;}
     l=0;
	 if(!(buffer = (char*)calloc(_MAX_FILE_INPUT_BUFFER_LENGTH+1,sizeof(*buffer)))) {iret = -100; goto _END;}
     while(fgets(buffer, _MAX_FILE_INPUT_BUFFER_LENGTH,fp) != NULL)
      {
       if(ReadNumString(buffer,1, &dat, &ii) <0 ) {iret = -100; goto _END;}
       if(ii != ncol)
        if((buffer[0] == '\n')||(buffer[0] == '\r')) continue;
        else
         {iret = -2; goto _END;}
       for(i=0;i<ncol;i++)
        (data[i])[l] = dat[i];
       free(dat); dat = NULL;
       l++;
      }
     if(l != nrow) { iret = -3; goto _END;}
    break;

    case _BINARY_666_DATA_FILE:
     fp = fopen(filename,"rb");
     if(fp == NULL) {iret = -1; goto _END;}
     iret = BinaryFileHeaderRead(fp, &ii, NULL ,NULL ,&DataTypes);
     switch(iret)
      {
       case 0: break;

       case -1: iret = -4; goto _END;
       case -2: iret = -5; goto _END;
       case -100: goto _END;
      }
     if(ii != ncol) {iret = -2; goto _END;}

     l=0;i=0;
     length=0;
     for(i=0;i<ncol;i++)
      {
       switch(DataTypes[i])
        {
         case 1:
          length += sizeof(short);
         break;
         case 2:
          length += sizeof(int);
         break;
         case 3:
          length += sizeof(float);
         break;
         case 4:
          length += sizeof(double);
         break;
        }
      }

     binary_buf = (char*)calloc(length*nrow,sizeof(*binary_buf));
     if(binary_buf == NULL) {iret = -100; goto _END;}
     if (fread(binary_buf,length*nrow,1,fp) <= 0) {iret = -3; goto _END;}
     c_ptr = binary_buf;
     for(l=0;l<nrow;l++)
      {
       for(i=0;i<ncol;i++)
        {
         switch(DataTypes[i])
          {
           case 1:
            (data[i])[l] = (double)(*((short*)c_ptr)); c_ptr += sizeof(short);
           break;
           case 2:
            (data[i])[l] = (double)(*((int*)c_ptr)); c_ptr += sizeof(int);
           break;
           case 3:
            (data[i])[l] = (double)(*((float*)c_ptr)); c_ptr += sizeof(float);
           break;
           case 4:
            (data[i])[l] = (double)(*((double*)c_ptr)); c_ptr += sizeof(double);
           break;
          }
        }
      }
    break;

	 case _BINARY_TS_V2_DATA_FILE:
	  iret = BinaryTS_V2FileHeaderRead(fp, NULL,NULL,NULL,NULL,NULL,&header);
      if(iret < 0) goto _END;
	  for(i=0;i<header.cols_number;i++)
	  {
	   size = 0;
	   switch(header.ColHeaders[i]->type)
	   {
	    case _1_BYTE_DATA: size = 1; break;
        case _2_BYTE_DATA: size = 2; break;
		case _4_BYTE_DATA: case _4_FLOAT_DATA: size = 4; break;
		case _8_FLOAT_DATA: size = 8; break;
	   }
	   size *= header.rows_number;

	   col_buf = realloc(col_buf, size);
       if (fread(col_buf,size,1,fp) <= 0) {iret = -3; goto _END;}
	   switch(header.ColHeaders[i]->type)
	   {
	    case _1_BYTE_DATA:
		 for(j=0;j<header.rows_number;j++)
		  (data[i])[j] = 
		  (double)((char*)col_buf)[j]*header.ColHeaders[i]->a+
          header.ColHeaders[i]->b;
		 break;
		 
        case _2_BYTE_DATA:
		 for(j=0;j<header.rows_number;j++)
		  (data[i])[j] = 
		  (double)((unsigned short*)col_buf)[j]*header.ColHeaders[i]->a+
          header.ColHeaders[i]->b;
		break;
		case _4_BYTE_DATA: 
		 for(j=0;j<header.rows_number;j++)
		  (data[i])[j] = 
		  (double)((unsigned long*)col_buf)[j]*header.ColHeaders[i]->a+
          header.ColHeaders[i]->b;
		break;
		case _4_FLOAT_DATA:
		 for(j=0;j<header.rows_number;j++)
		  (data[i])[j] = 
		  (double)((float*)col_buf)[j]*header.ColHeaders[i]->a+
          header.ColHeaders[i]->b;
			break;
		case _8_FLOAT_DATA: 
		 for(j=0;j<header.rows_number;j++)
		  (data[i])[j] = 
		  ((double*)col_buf)[j]*header.ColHeaders[i]->a+
          header.ColHeaders[i]->b;
		 break;
	   }
	  }
	 break;

   }
  _END:
   free(dat); free(binary_buf); free(buffer);
   if(fp != NULL) fclose(fp);
   free(DataTypes);
   return iret;
 }


int ReadInputDataArray(int *file_type, char *filename,double ***DataArray, int *NCols,
                        long *NRows, char ***ColNames, int **chan_index, double *time)
 {
  int iret,i;
  double **data;
  if((iret = DataFileInfo(filename, file_type, NCols, NRows,  ColNames, chan_index, time)) < 0) goto _END;
  data = (double**)calloc(*NCols,sizeof(*data));
  if(data == NULL) {iret = -100; goto _END;}
  for(i=0;i<*NCols;i++)
   {
    data[i]=(double*)calloc(*NRows,sizeof(**data));
    if(data[i] == NULL) {iret = -100; goto _END;}
   }
  iret = DataFileRead(filename,*file_type,*NCols, *NRows,data);
  if(iret < 0) goto _END;
  *DataArray = data;

  _END:
  if(iret < 0)
   {
    if(data != NULL)
     {
      for(i=0;i<*NCols;i++) free(data[i]);
      free(data);
      *DataArray = NULL;
     }
    if(*ColNames != NULL)
     {
      for(i=0;i<*NCols;i++) free((*ColNames)[i]);
      free(*ColNames);
      *ColNames = NULL;
     }
   }
  return iret;
 }

/***************************************************************
 *                                                             *
 *           Checking group item in record                     *
 *                                                             *
 *    Input parameters:                                        *
 *                      char *buffer - buffer with record      *
 *                      char *group_item - group item          *
 *                      int *no - <0 - read No of item         *
 *                                 >=0 - checking no of item   *
 *    Output parameter:                                        *
 *                      int *no - number of item               *
 *                                                             *
 *    Return value:                                            *
 *                   int  - 0 - item not found                 *
 *                          1 - item found                     *
 *                                                             *
 ***************************************************************/
int CheckForGroupItem(char *buffer, char *group_item, int *no)
{
  int iret, i;
  char *str;
  iret = 0;
  if(!strchr(buffer, '[')) goto _END;
  if(!(str = strstr(buffer,group_item))) goto _END; 
  str += strlen(group_item);
  str = RemoveBlanks(str,0);
  i = -1;
  sscanf(str,"%d",&i);
  if(*no >= 0)
  {
   if(i != *no) goto _END;
  }
  else
  {
   *no = i;
  }
  iret = 1;
_END:
  return iret;
}

/***************************************************************
 *                                                             *
 *           Finding group in file and stoping at this group   *
 *                                                             *
 *    Input parameters:                                        *
 *                      FILE *fp pointer to opened file        *
 *                      char *group_item - group item          *
 *                      int *no - <0 - read No of next item    *
 *                                 >=0 - finding item with no  *
 *    Output parameter:                                        *
 *                      int *no - number of item               *
 *                                                             *
 *    Return value:                                            *
 *                   int  - 0 - item not found                 *
 *                          1 - item found                     *
 *                                                             *
 ***************************************************************/

int GroupFind(FILE *fp, char *group_item, int *no)
{
 char *c;
 int iret;
 char buffer[_FILE_INPUT_BUFFER_LENGTH+1];

 iret = 0;
 while(c = fgets(buffer, _FILE_INPUT_BUFFER_LENGTH, fp))
 {
  iret = CheckForGroupItem(buffer, group_item, no);
  if(iret > 0)break;
 }
 return iret;
}


/***************************************************************
 *                                                             *
 *           Reading integer parameter from string             *
 *                                                             *
 *    Input parameters:                                        *
 *                      char *param_str - buffer with record   *
 *    Output parameter:                                        *
 *                      int *param - parameter                 *
 *                                                             *
 *    Return value:                                            *
 *                   int  - 0 - sucsess                        *
 *                         -1 - error                          *
 *                                                             *
 ***************************************************************/

int ReadIntParam(char *param_str, int *param)
{
 if(!*param_str) return -1;
 if(!sscanf(param_str,"%d",param)) return -1;
 return 0;
}

/***************************************************************
 *                                                             *
 *           Reading long parameter from string                *
 *                                                             *
 *    Input parameters:                                        *
 *                      char *param_str - buffer with record   *
 *    Output parameter:                                        *
 *                      long *param - parameter                *
 *                                                             *
 *    Return value:                                            *
 *                   int  - 0 - sucsess                        *
 *                         -1 - error                          *
 *                                                             *
 ***************************************************************/
int ReadLongParam(char *param_str, long *param)
{
 if(!*param_str) return -1;
 if(!sscanf(param_str,"%ld",param)) return -1;
 return 0;
}

/***************************************************************
 *                                                             *
 *           Reading long parameter from string                *
 *                                                             *
 *    Input parameters:                                        *
 *                      char *param_str - buffer with record   *
 *    Output parameter:                                        *
 *                      long *param - parameter                *
 *                                                             *
 *    Return value:                                            *
 *                   int  - 0 - sucsess                        *
 *                         -1 - error                          *
 *                                                             *
 ***************************************************************/
int ReadUlongParam(char *param_str, unsigned long *param)
{
 if(!*param_str) return -1;
 if(!sscanf(param_str,"%lu",param)) return -1;
 return 0;
}
/***************************************************************
 *                                                             *
 *           Reading double parameter from string              *
 *                                                             *
 *    Input parameters:                                        *
 *                      char *param_str - buffer with record   *
 *    Output parameter:                                        *
 *                      double *param - parameter              *
 *                                                             *
 *    Return value:                                            *
 *                   int  - 0 - sucsess                        *
 *                         -1 - error                          *
 *                                                             *
 ***************************************************************/
int ReadDoubleParam(char *param_str, double *param)
{
 if(!*param_str) return -1;
 if(!sscanf(param_str,"%le",param)) return -1;
 return 0;
}

/***************************************************************
 *                                                             *
 *           Reading string parameter from string              *
 *                                                             *
 *    Input parameters:                                        *
 *                      char *param_str - buffer with record   *
 *    Output parameter:                                        *
 *                      char* *param - parameter                 *
 *                                                             *
 *    Return value:                                            *
 *                   int  - 0 - sucsess                        *
 *                         -1 - error                          *
 *                                                             *
 ***************************************************************/
int ReadStringParam(char *param_str, char *param)
{
 char *c1;
 if(!*param_str) return -1;
 c1 = RemoveBlanks(param_str,0);
 strcpy(param,c1);
 return 0;
}


/***************************************************************
 *                                                             *
 *           Finding parameter item from the list, correspon-  *
 *           ding to the record and selecting string with      *
 *           parameter                                         *
 *                                                             *
 *    Input parameters:                                        *
 *                      char *buffer - buffer with record      *
 *                      char const **parameter_items - list    *
 *                                  with parameter items       *
 *                      int const number_of_parameter_items -  *
 *                          number of items in list            *
 *    Output parameter:                                        *
 *                      char* **param - string with parameter  *
 *                                                             *
 *    Return value:                                            *
 *                   int  - 0 - sucsess                        *
 *                         -1 - error                          *
 *                                                             *
 ***************************************************************/
int ParameterItem(char *buffer, char const **parameter_items,
				 int const number_of_parameter_items, char **param )
{
 unsigned i;
 int iret;
 char *str;

 iret = -1;
 str = RemoveBlanks(buffer,0);
 if(!*str) goto _END;
 for(i=0;i<strlen(str);i++)
 {
  if(str[i] == '=') break;
 }
 if(i == strlen(str)) goto _END;
 str[i] = (char)0;
 *param = &str[i+1];
 str = RemoveBlanks(str,0);
 for(i=0;i<strlen(str);i++) str[i]=toupper(str[i]);
 for(i=0;i<(unsigned)number_of_parameter_items;i++)
  if(!strcmp(str, parameter_items[i])) break;
 if(i == (unsigned)number_of_parameter_items) goto _END;
 iret = i;
 *param = RemoveBlanks(*param,0);
_END:
 return iret;
}


/***********************************************************************
 *                                                                     *
 *            Automatic file name                                      *
 *                                                                     *
 *   Input parameters:                                                 *
 *                    char *directory - directory                      *
 *                    char *prefix - file name prefix                  *
 *                    int num - number                                 *
 *                    char *ext - extension (witout '.')               *
 *                                                                     *
 *   Output parameters:                                                *
 *                    char *FileName - file name                       *
 *                                                                     *
 *   Return value:                                                     *
 *                 0 - sucsessfull exit                                *
 *                -1 - too big number                                  *
 *                                                                     *
/***********************************************************************/

int AutoFileName(char *prefix, int num, char *ext, char* FileName)
 {
  int iret;
  iret = 0;
  if ( (prefix == NULL) || (strlen(prefix) == 0)||(FileName == NULL)) {iret = 0; goto _END;}
  FileName[0] = (char)0;
   sprintf(&FileName[strlen(FileName)],"%s",prefix);
  sprintf(&FileName[strlen(FileName)],"_%d.%s",num,ext);
  _END:
   return iret;
 }


int AutoPCXFileName(char *prefix,int plot_num, int num, char *ext, char* FileName)
 {
  int iret;
  iret = 0;
  if ( (prefix == NULL) || (strlen(prefix) == 0)||(FileName == NULL)) {iret = 0; goto _END;}
  FileName[0] = (char)0;
  sprintf(&FileName[strlen(FileName)],"%s",prefix);
  sprintf(&FileName[strlen(FileName)],"_%d",plot_num);
  sprintf(&FileName[strlen(FileName)],"_%d.%s",num,ext);
  _END:
   return iret;
 }

/***********************************************************************
 *                                                                     *
 *            Full file name                                           *
 *                                                                     *
 *   Input parameters:                                                 *
 *                    char *directory - directory                      *
 *                    char *FileName - FileName                        *
 *                                                                     *
 *   Output parameters:                                                *
 *                    char *FullFileName - file name                   *
 *                                                                     *
/***********************************************************************/

void FullFileName(char *directory,char *FileName, char* FullFileName)
 {
  if((FileName == NULL) || (FileName[0] == 0) || (FullFileName == NULL)) return;
  FullFileName[0] = (char)0;
  if(directory != NULL)
   if(strlen(directory) > 0)
    {
     if(directory[strlen(directory)-1] == '\\')
      directory[strlen(directory)-1] = (char)0;
      if(directory[0] != (char)0) sprintf(FullFileName,"%s\\",directory);
    }
  sprintf(&FullFileName[strlen(FullFileName)],"%s",FileName);
  return;
 }



/***********************************************************************
 *                                                                     *
 *            Search last automatic file number                        *
 *                       in comment file                               *
 *                                                                     *
 *   Input parameters:                                                 *
 *                    char *comment file name - comment file name      *
 *                    char *keyword - keyword for data file name       *
 *                    char *prefix - data file name prefix             *
 *                    char *ext - extension                            *
 *                                                                     *
 *   Output parameters:                                                *
 *                    int *number - next data file number              *
 *                                                                     *
 *   Return value:  0 - sucsessfull exit                               *
 *                 -1 - wrong parameters                               *
 *                 -100 - unsufficient memory                          *
 *                                                                     *
/***********************************************************************/
 int LastAutoFile(char*CommentFileName, char*keyword, char*prefix,char *ext, int *number)
  {
   char*cp;
   FILE*fp;
   char *c; unsigned int ind,i;
   int num;
   int iret;
   char *pref;
   cp = NULL;
   iret = 0;
   fp = NULL;
   if (keyword == NULL) { iret = -1; goto _END; }
   if(CommentFileName == NULL) { *number = 0; goto _END;}
   cp = (char*)calloc(_FILE_INPUT_BUFFER_LENGTH+1, sizeof(*cp));
   if(cp == NULL) {iret = -100; goto _END;}

   fp = fopen(CommentFileName,"r");
   if(fp == NULL) { *number = 0; goto _END;}
   num = -1;
   while(fgets(cp,_FILE_INPUT_BUFFER_LENGTH,fp) != NULL)
    {
     if((c=strstr(cp,keyword)) == NULL) continue;
     else
      {
       for(i=strlen(cp)-1; i!=0;i--)
        {
         if((cp[i] == ' ')||(cp[i] =='\r')||(cp[i] =='\n')) cp[i] = (char)0;
         else break;
        }
       ind = (c - cp) + strlen(keyword);
       for(i=strlen(cp)-1; i>=ind; i--)
        {
         if((cp[i] == ' ') || (cp[i] == '=') ||(cp[i] ==':')||(cp[i] == '\\')) break;
        }
       pref = cp+i+1;
       c=strchr(pref,'.');
	   if(!strstr(pref, prefix)) {pref = NULL; continue;}
       if(c == NULL) { pref = NULL; continue;}
//       pref_length = c-pref-3;
//       if((pref_length != strlen(prefix))||(strncmp(pref,prefix,pref_length) != 0))
//        { pref = NULL; continue;}
       if(strcmp(c+1,ext) != 0) { pref = NULL; continue;}
	   c=strchr(pref,'_');
	   if(!c) continue;
       if(sscanf(c+1,"%d",&num) <=0) { pref = NULL; continue;}
      }
    }
   if(num == -1) *number = 0;
   else *number = num+1;

   _END:
   free(cp);
   if(fp != NULL) fclose(fp);
   return iret;
  }

void DateTime( char *dateString, char *timeString)
 {
  struct tm ts;
  time_t calTime; 
  time(&calTime);
  localtime_r( &calTime, &ts);
  if(dateString)
   sprintf(dateString,"%d.%d.%d",ts.tm_wday, ts.tm_mon, ts.tm_year);
  if(timeString)
   sprintf(timeString,"%d:%d:%d",ts.tm_hour, ts.tm_min,ts.tm_sec );
 }



 void ExtractDirectoryFromFileName(char *name)
 {
  char *c_ptr;
  if(!name) return;
  if(!(*name)) return;
  c_ptr = name;
  while(*c_ptr) c_ptr++;
  while((*c_ptr != '\\') && (c_ptr != name)) c_ptr--;
  *c_ptr = (char)0;
  return;
 }


 void ExtractDirectoryAndFileFromFullFileName(char *name, char **file_ptr)
 {
  char *c_ptr;
  if(!name) return;
  if(!(*name)) return;
  c_ptr = name;
  while(*c_ptr) c_ptr++;
  while((*c_ptr != '\\') && (c_ptr != name)) c_ptr--;
  if((unsigned)(c_ptr-name) < strlen(name)) *file_ptr = c_ptr+1;
  else *file_ptr = c_ptr;
  *c_ptr = (char)0;
  return;
 }
