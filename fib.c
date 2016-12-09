
#include "lispinit.h"

#include "fib.inc"

#ifndef vax 
#include "lispinit.dec"

#endif

  _node fib(x)
  
       _node x;
  { if(_numval(x)<2)
    return x;
   else return _makenumber((_numval(fib(_makenumber((_numval(x)-
     1))))+_numval(fib(_makenumber((_numval(x)-2))))));
   }
_initconst () 
 {_initint () ; 
_in_file(_open_file(_makestring("fib.dta")));

{ _close_file(_in_file(NIL));
  _in_file(_makenumber(0));
  };}

main (argc , argv) 
int argc ; char *argv[] ; 
{ _initconst () ; 
print(fib(_arg (argc , argv , 1) ));
return (0) ; } 
