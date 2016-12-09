
#include "lispinit.h"

#include "ihme.inc"

#ifndef vax 
#include "lispinit.dec"

#endif

  _node ihme()
  { { 
         _node __1L_;
    (__1L_= reverse(NIL,explode(_lisp_read())));
    { 
           _node __3L_;
      (__3L_= __1L_);
      print(compress(print(__3L_)));
       }
     }
   return _putch(13);
   }
_initconst () 
 {_initint () ; 

 __saatana=NIL;
;}

main (argc , argv) 
int argc ; char *argv[] ; 
{ _initconst () ; 
ihme();
return (0) ; } 
