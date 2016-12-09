#include "lispinit.h"

_node        cons(y, x) 
_node        x; 
_node        y; 
  {  _node    newnode;

   if (_freenode == NIL) 
   { 
    newnode = malloc(offsetof(_nodecons,_nodeb._alist_s._End));
    (*newnode)._ntype = _alist; 
    (*newnode)._nodeb._alist_s._head = x; 
    (*newnode)._nodeb._alist_s._tail =  y; 
    return newnode; 
    } 
   else 
   { 
    newnode = _freenode; 
    _freenode = (*_freenode)._nodeb._alist_s._tail; 
    (*newnode)._nodeb._alist_s._head = x; 
    (*newnode)._nodeb._alist_s._tail = y; 
    return newnode; 
    } 
   } 

  
  
_node        _makenumber(x) 
int          x; 
   {  _node    newnode;

    if ((x < 200) && (x > -100)) 
    return _integers[x+100]; 
    else 
    { 
     newnode = malloc(offsetof(_nodecons,_nodeb._number_s._End));
     (*newnode)._ntype = _number; 
     (*newnode)._nodeb._number_s._value = x; 
     return newnode; 
     } 
    } 

   
_node __newnode;

_node        _makestring(x) 
 char       *x; 

   { _node _makes2();
       _oblist = _makes2(_oblist,x);
     return __newnode; 
   } 
  
_node    _makes2(n, x) 
_node      n; 
 char      *x;
    { 
      if (n == NIL) 
      { 

       __newnode = malloc(strlen(x) + 2 +
              offsetof(_nodecons,_nodeb._string_s._strstr[0]));
       (*__newnode)._ntype = _string;
       strcpy((*__newnode)._nodeb._string_s._strstr,x); 
       (*__newnode)._nodeb._string_s._smaller = NIL; 
       (*__newnode)._nodeb._string_s._bigger = NIL; 
       return __newnode; 
	}
      else 
       if ((strcmp(x,(*n)._nodeb._string_s._strstr) == 0)) 
       { __newnode = n; return n; 
       } 
       else 

        if ((strcmp(x,(*n)._nodeb._string_s._strstr) > 0)) 
        { 
         ((*n)._nodeb._string_s._bigger) =
              _makes2((*n)._nodeb._string_s._bigger, x);
         return n; 
         } 
        else 
        { 
         (*n)._nodeb._string_s._smaller =
              _makes2((*n)._nodeb._string_s._smaller, x); 
         return n; 
         } 
     } 

  
_node        _dispose(x) 
_node        x; 
   { _nodetype    nodtyp;
   
     if (x != NIL) 
     { 
      nodtyp = (*x)._ntype; 

       if (nodtyp == _alist) 
       { 
        (*x)._nodeb._alist_s._head = T; 
        (*x)._nodeb._alist_s._tail = _freenode; 
        _freenode = x; 
        return T; 
        } 
	else return NIL;
     } 
	else return NIL;
    } 

   

   
_node        __makenumber(x) 
int          x; 
    {  _node   newnode;
    
     newnode = malloc(offsetof(_nodecons ,_nodeb._number_s._End));
     (*newnode)._ntype = _number; 
     (*newnode)._nodeb._number_s._value = x; 
     return newnode; 
     } 

    
   
void         _initint() 
   { int          i;
   
   _freenode = NIL; 
   _echo = NIL; 
   _notplmstring = true; 
   _oblist = NIL; 
   _tabu = 0; 
   _wrs = 0; 
   _rds = 0; 
   nxtch = 32; 
   _fratom = false; 
   _linep = 40; 
    for (  i = -100; i <= 200; i++) 
     _integers[i+100] = __makenumber(i); 
    T = _makestring("t"); 
    for (i = 1; i <= _no_of_files; i++) _files[i]._f_free = true; 
     _ttychno = 0; 
    } 

   
 
int          _numval(x) 
_node     x; 
  { 

   if (x != NIL) 
   return (*x)._nodeb._number_s._value; 
   else 
   return 0; 
   } 

  
  
char   *_strval(x) 
_node     x; 
  { 
     if (x != NIL) 
     if ((*x)._ntype == _string) return ((*x)._nodeb._string_s._strstr); 
     return ("ERROR");
    } 
  
 
_node        _tnil(x) 
 char         x; 
  { if (x) return T; 
    else return NIL; 
  } 

char       atom(x) 
_node        x; 
  { 
   if (x == NIL) 
    return 1; 
   else 
   if ((*x)._ntype == _alist) return 0; 
   else return 1; 
   } 

  
 
_node        car(x) 
_node        x; 
  { 
   if (atom(x)) 
   return NIL; 
   else 
   return (*x)._nodeb._alist_s._head; 
   } 

  
_node        cdr(x) 
_node        x; 
   { 
    if  (atom(x)) 
    return NIL; 
    else 
    return (*x)._nodeb._alist_s._tail; 
    } 

   
 
char        numberp(x) 
_node        x; 
  { 
    if (x != NIL) 
    if ((*x)._ntype == _number) return 1; 
    else return 0; 
    else return 0; 
   } 

  
  
char identp(x) 
_node        x; 
   { 
    if (x != NIL) 
    if ((*x)._ntype == _string) return 1; 
    else return 0; 
    else return 0; 
    } 

   
 
_node        rplaca(y,x) 
_node        x; 
_node        y; 
  { 
   if (!atom(x)) (*x)._nodeb._alist_s._head = y; 
   return x; 
   } 

  
_node        rplacd(y,x) 
_node        x; 
_node        y; 
   { 
    if (!atom(x)) (*x)._nodeb._alist_s._tail = y; 
    return x; 
    } 

   
 
_node        _getchar( y,x) 
_node     x; 
_node     y; 
  { char       *str;
    int          num;
  
    num = _numval(y); 
    str = & ((*x)._nodeb._string_s._strstr) [0]; 

    if (num <= strlen(str)) 
    return _makenumber(str[num-1]); 
    else 
    return _makenumber(0); 
   } 

  
  
char         numch(ch) 
char         ch; 
   { 
    return (ch > 47) && (ch < 58); 
    } 

   
  
char         __allnumbers(str) 
_chstr       str; 
   { int          i;

    if (((strlen(str) > 1) &&
	 ((str[0] == '-') ||
	 (str[0] == '+'))) ||
	numch(str[0])) 
     { 
     for (
      i = 1; i < strlen(str); i++) 
      { if (!(numch(str[i]))) return false; } 
      return true;
     } 
    else 
    return false; 
    } 

   
 
_node        retuident(str) 
_chstr       str; 
  { int          num;

   if (__allnumbers(str)) 
    {sscanf(str,"%d",&num);
      return _makenumber(num); 
    } 
   else 
   return _makestring(str); 
   } 

  
 
char         readc() 
  { char temp;
   temp = nxtch; 
   if (temp==13) nxtch=10;
   else if (26 != nxtch) 
       {
	 if (_rds==0) nxtch = getchar();
	   else nxtch = getc(_files[_rds]._f_text);
         if (nxtch==EOF) nxtch=26;
         if (nxtch==10) nxtch=13;
       } 
  
  return temp;
   } 

  
 
_node        _putch(ch) 
char         ch; 
  { 

    if (ch == 13) 
    { 

      if (_wrs == 0) 
      printf("\n"); 
      else 
      { 
       fprintf(_files[_wrs]._f_text,"\n"); 
       if (_echo != NIL) printf("\n"); 
       } 
      
     _tabu = 0; 
     } 
    else 

     if (ch != 10) 
     { 
      _tabu = _tabu + 1; 

       if (_wrs == 0) 
       printf("%c",ch); 
       else 
       { 
        fprintf(_files[_wrs]._f_text,"%c",ch); 
         if (_echo != NIL) printf("%c",ch); 
        } 
      } 
   return NIL; 
   } 

  
  
void         _putstr(str) 
_chstr       str; 
   { 
    _tabu = _tabu + strlen(str); 

     if (_wrs == 0) 
     printf("%s",str); 
     else 
     {
      fprintf(_files[_wrs]._f_text,"%s",str); 
      if (_echo != NIL) printf("%s",str); 
      } 
    } 

   
  
_node        _putint(x) 
int          x; 
   { _chstr       str;
    sprintf(str,"%d",x);
    _putstr(str); 
    return NIL; 
    } 

   
 
_node        tab(x) 
_node     x; 
  { 
    if (x == NIL) 
    return _makenumber(_tabu); 
    else 
    while (_numval(x) > _tabu) _putch(32); 
   } 

  
  
_node        readident(ch) 
char         ch; 
   { _chstr       str;char         too_long;int          i;
   
   str[0] = ch; 
   i = 1; 

    while ((nxtch != 40) && (nxtch != 41) && (nxtch > 32)) 
    { 

      if (i <= 40) 
       {str[i]= readc();
               i = i + 1;} 
      else 
       too_long = readc(); 

     } 
   str[i]=0; 
   return retuident(str); 
   } 


_node  _lisp_read() 
  { char         ch;
    _node _ihme_juttu();
      
   _fratom = true; 
   do { ch = readc(); }
   while (!((ch > 32) || (ch == 26) || (ch == 27))) ; 

    if (ch == 26) 
    return retuident("END-OF-FILE"); 
    else 
     
     if (ch == 40) 
     return _ihme_juttu(); 
     else 

      if (ch == 41) 
      return NIL; 
      else 
      return readident(ch); 
}

_node _ihme_juttu() 
   { _node        i;
     _node       temp;
     char         c1, c2;
   
     i = _lisp_read();
    

     if (_fratom && (i == NIL)) 
     { _fratom = false; return NIL; 
     } 
     else 
     { 
      temp = i; 
      while (nxtch < 33) c1 = readc(); 
      c2 = nxtch; 

      i = _ihme_juttu(); 

       if (c2 == 46) 
       return cons( car(cdr(i)),temp); 
       else 
       return cons( i, temp); 
      } 
}
  
  
   
    
   
char         plmchr(x) 
char         x; 
    { 
     return ((x > 47) && (x < 58))
 || ((x > 64) && (x < 91))
 || ((x > 96) && (x < 123))
 || (x == '_') || (x == '$'); 
     } 

    
  
_node        plmitem() 
   { char         ch;
    _chstr       str;
    char         too_long;
    int          i;
   
    do { 
     ch = readc(); 

     }  while (!((ch > 32) || (ch == 26))) ; 

     if (ch == 26) 
     return _makestring("END-OF-FILE"); 
     else 

      if (_notplmstring && (ch == '/') && (nxtch == '*')) 
      { 
       do { 

        }  while (!((readc() == '*') && (nxtch == '/'))) ; 
       ch = readc(); 
       return plmitem(); 
       } 
      else 
      { 
       if (ch == 39) _notplmstring = !(_notplmstring); 
       str[0] = toupper(ch); 
       i = 1; 

        if (plmchr(ch)) 

         while (plmchr(nxtch)) 
         { 

        if (i <= 40) 
            {str[i]= toupper(readc()); 
               i = i + 1;} 
         else 
          too_long = readc(); 
           } 
       str[i]=0; 
       return retuident(str); 
       } 
    } 

int          _find_free_file() 
  { int          i;
   i = 1; 
   while (!(_files[i]._f_free)) i = i + 1; 
   _files[i]._f_free = false; 
   return i; 
   } 

  
_node        _open_file(x) 
_node        x; 
   { int        i;
     FILE  	*h;
     _chstr 	name;
     strcpy(name,(*x)._nodeb._string_s._strstr);
     do {
       h= fopen(name,"r");
       if (h==NIL) {
       	  printf("\n\n*** There is no %s-file. Give a new name:",name);
	  gets(name);
          } }
     while (h==NIL);
    i = _find_free_file(); 
    _files[i]._f_text=h;
    return _makenumber(i); 
    } 
  
  
_node        _create_file(x) 
_node        x; 
   { int          i;
    i = _find_free_file(); 
    _files[i]._f_text= fopen((*x)._nodeb._string_s._strstr,"w");
    return _makenumber(i); 
    } 
  
  
_node        _close_file(x) 
_node        x; 
   { 
     fclose (_files[_numval(x)]._f_text);
     _files[_numval(x)]._f_free = true;
    return NIL; 
    } 

   
  
_node        _in_file(x) 
_node     x; 
   { 
     _notplmstring=true;
     if (x != NIL) 
     { 
      _rds = _numval(x); 
      nxtch = 32; 
      return NIL; 
      } 
     else 
     return _makenumber(_rds); 
    } 

  
_node        out(x) 
_node     x; 
   { 

     if (x != NIL) 
     { _wrs = _numval(x); return NIL; 
     } 
     else 
     return _makenumber(_wrs); 
    } 

   
 
_node        compress(x) 
_node        x; 
  { _chstr       str;
     int i = 0;  
   strcpy(str, "?"); 
   while (x != NIL) 
    { 
     str[i] =_numval (car (x));
     i++;
     x = cdr(x); 
     } 
   str[i]=0;
   return retuident(str); 
   } 

  
   
_node        __explo_2(str, i) 
_chstr       str; 
int          i; 
    {
      if (i >= strlen(str)) 
      return NIL; 
      else 
      return cons( __explo_2(str, i + 1),_makenumber(str[i]%256)); 
     } 
  
_node        explode(x) 
_node     x; 
   { _chstr       tempstr;

     if (identp(x)) 
     return __explo_2((*x)._nodeb._string_s._strstr, 0); 
     else 
      if (numberp(x)) 
      { 
       sprintf(tempstr,"%d",(*x)._nodeb._number_s._value);
       return __explo_2(tempstr, 0); 
       } 
      else 
      return __explo_2("Explode-error", 0); 
    } ;


_node _arg(argc,argv,i)
 int argc,i;char *argv[];
 {   if (i<argc)
    return retuident(argv[i]);
   else return NIL;
 }

  int _list_length(x)
  _node
       x;
  { { int
         _y_5;
    (_y_5= 0);
    while(NIL!=x)
      { (_y_5=_y_5+1);
       (x=cdr(x));
       }
     return _y_5;
     }
   }

  _node nthcdr(y,x)
  _node  x;
  _node  y;
  { int x2;
    x2=_numval(x);
    while(x2>0) {
      (y= cdr(y));
      x2=x2-1; }
   return y;
   }

  