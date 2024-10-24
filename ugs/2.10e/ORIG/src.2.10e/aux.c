#include <stdlib.h>

#if ( defined(__LINUX_AOUT) || defined(__LINUX_ELF) || defined(__Darwin) )
void free_   ( idum )   void *idum ;  { free (idum) ; }
void malloc_ ( idum ) size_t *idum ;  { malloc (*idum) ; }
#endif

int  putenv_( string , length )
char  *string  ;
int   length   ;
{
  char *envirstr = malloc ( length+1 ) ;

/*printf ( " Input string is %i >%10s< \n" , length , string ) ;*/
  strncpy ( envirstr,  string, length ) ;
  envirstr[length] = '\0' ;
  return putenv ( envirstr ) ;
/*printf ( " Allocated length %i at %i is %i >%s<\n" ,
	  length , envirstr , strlen(envirstr) , envirstr ) ;*/
}
