
#define _FILE_INPUT_BUFFER_LENGTH 1000
#define _MAX_FILE_INPUT_BUFFER_LENGTH 1000000


#define _1_BYTE_DATA  0
#define _2_BYTE_DATA  1
#define _4_BYTE_DATA  2
#define _4_FLOAT_DATA 3
#define _8_FLOAT_DATA 4

#define _ASCII_DATA_FILE 0
#define _BINARY_666_DATA_FILE 1
#define _BINARY_TS_V2_DATA_FILE 2
#define _UNKNOWN_FILE           10

typedef struct COL_HEADER
{
  int type;
  char *name;
  double a;
  double b;
  int chan_index;
}COL_HEADER;

typedef struct FILE_HEADER_TS_V2
{
  long cols_number;
  long rows_number;
  double time_interval;
  COL_HEADER **ColHeaders;
}FILE_HEADER_TS_V2;


static int FILE_INPUT_BUFFER_LENGTH=_FILE_INPUT_BUFFER_LENGTH;

char *RemoveBlanks(char *,int );
int ASCII_DataSave(char*, int, long, double **);
int Binary_666_DataSave(char*FileName, int ColsNumber, long RowsNumber,
                            char **ColNames,double **data, int fast_flag);
int Binary_TS_V2_DataSave(char*FileName, long ColsNumber, long RowsNumber,
						double TimeInterval,int *DataTypes,
                        int *ChannelIndexes, double *a_coeffs, double *b_coeffs,
						char **ColNames,void **data);
int CheckFileStatus(char*);
int DataFileInfo(char *filename, int *type, int *NCols, long *NRows,  
				 char ***ColNames, int **ChannelIndexes, double *t);
 int BinaryFileHeaderRead(FILE*, int *,long*,char  ***,int**);
 int BinaryTS_V2FileHeaderRead(FILE*fp, int *NCols, long *NRows,char ***ColNames,int**ColTypes,
	 double *time_interval, FILE_HEADER_TS_V2 *header);
int ReadNumString(char*, int,double**, int *);
int DataFileRead(char *, int, int, long, double  **);
int ReadInputDataArray(int *file_type, char *filename,double ***DataArray, int *NCols,
                        long *NRows, char ***ColNames, int **chan_index, double *t);
int CheckForGroupItem(char *buffer, char *group_item, int *no);
int GroupFind(FILE *fp, char *group_item, int *no);
int ReadIntParam(char *param_str, int *param);
int ReadLongParam(char *param_str, long *param);
int ReadUlongParam(char *param_str, unsigned long *param);
int ReadDoubleParam(char *param_str, double *param);
int ReadStringParam(char *param_str, char *param);
int ParameterItem(char *buffer, char const **parameter_items,
				 int const number_of_parameter_items, char **param );
int AutoFileName(char *prefix, int num, char *ext, char* FileName);
int AutoPCXFileName(char *prefix,int plot_num, int num, char *ext, char* FileName);
void FullFileName(char *directory,char *FileName, char* FullFileName);
int LastAutoFile(char*CommentFileName, char*keyword, char*prefix,char *ext, int *number);
void DateTime( char *date, char *time);
void ExtractDirectoryFromFileName(char *name);
void ExtractDirectoryAndFileFromFullFileName(char *name, char **file_ptr);
