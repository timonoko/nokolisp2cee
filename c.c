
#include "lispinit.h"

#include "c.inc"

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
      return print(compress(print(__3L_)));
       }
     }
   }
_initconst () 
 {_initint () ; 
_in_file(_open_file(_makestring("c.dta")));

 __saatana=NIL;
{ _close_file(_in_file(NIL));
  _in_file(_makenumber(0));
  };}

main (argc , argv) 
int argc ; char *argv[] ; 
{ _initconst () ; 
ihme();
return (0) ; } 
