
/* vax: *.dta polunkorjaus */

#include <stdio.h>

#ifndef vax
#include <fcntl.h>
#include <malloc.h>
#include <string.h>
#endif

#define false          0
#define true           1
#define NIL            NULL
#define _strlen      40
#define _no_of_files 30          
#define _maxint       1000
#define _minint	      -1000

   typedef char *string;

   typedef char _chstr[_strlen];

   typedef enum { _alist, _number ,_string } _nodetype;

   typedef int  _adjoff;
 
   typedef struct _nodebase 
   { char _ntype;
     union
     {   struct { struct _nodebase *_head, *_tail; _adjoff _End;} _alist_s;
         struct { int _value; _adjoff _End; } _number_s;
         struct { struct _nodebase *_smaller,*_bigger;
                  char _strstr[1];} _string_s ;
     } _nodeb ;
   } _nodecons;

   typedef _nodecons *_node;


#ifndef offsetof                                 

#define offsetof(t,m) sizeof(t) 

#endif                                           

   int          _ttychno;
   _node        _freenode;
   _node	_echo;
   char         _notplmstring;
   _node     	_oblist;
   int          _tabu;
   int          _wrs, _rds;
   struct { char  _f_free;FILE *_f_text;} 
     _files[_no_of_files];
   _node        _waste_bas;
   char         nxtch;
   char         _fratom;
   int          _linep;
   _node        _integers[_maxint-_minint];
   _node        T;
