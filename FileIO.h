


typedef struct INPUT_DATA
{
 double xMin;
 double xMax;
 int nX;
 double yMin;
 double yMax;
 int  nY;
 double zMin;
 double zMax;
 int nZ; 
 char *inputFile;
 int filenameLength;
 char *outputFile;
 int *bands;
 int nBands;
 double eFermi;
 int nThreads;
} INPUT_DATA;

enum INPUT_KEYWORD_INDEX {
_Xmin,
_Xmax,
_Nx,
_Ymin,
_Ymax,
_Ny,
_Zmin,
_Zmax,
_Nz,
_InputFile,
_OutputFile,
_Bands,
_EFermi,
_Nthreads
};

#ifdef _FILE_IO
static const char *INPUT_KEYWORDS[] =
{
 "XMIN",
 "XMAX",
 "NX",
 "YMIN",
 "YMAX",
 "NY",
 "ZMIN",
 "ZMAX",
 "NZ",
 "INPUTFILE",
 "OUTPUTFILE",
 "BANDS",
 "EFERMI",
 "NTHREADS"
};

static size_t NUMBER_OF_INPUT_KEYWORDS =
      sizeof(INPUT_KEYWORDS)/sizeof(*INPUT_KEYWORDS);




static INPUT_DATA DEFAULT_DATA =
{
 DBL_MAX,       // _Xmin
 DBL_MAX,       // _Xmax
 0,             // _Nx
 DBL_MAX,       // _Ymin
 DBL_MAX,       // _Ymax
 0,             // _Ny
 DBL_MAX,       // _Zmin
 DBL_MAX,       // _Zmax
 0,             // _Nz
 NULL,          // _InputFile
 0,             // filenameLength
 NULL,          // _OutputFile,
 NULL,          // _Bands
 0,             // _nBands
 0.,		// _eFermi
 1              // _nThreads
};
#endif


 int SetupRead(char *filename, INPUT_DATA *data );
 int SetupSave(char *filename, INPUT_DATA *data );
 void ParceCommandString(int argc, char *argv[], INPUT_DATA *data);
 int VerifyInputData(INPUT_DATA *data);
