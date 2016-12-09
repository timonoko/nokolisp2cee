
#include "lispinit.h"

#include "rb.inc"

#ifndef vax 
#include "lispinit.dec"

#endif

  _node __F2(msb,x)
  
       _node *x;
       _node *msb;
  {
        _node rb();
{ { 
         _node __1L_;
    (__1L_= _makenumber((_numval(*x)%_numval(*msb))));
    return rb(_makenumber((_numval(*msb)/2)),__1L_);
     }
   }};



  _node rb(msb,x)
  
       _node x;
       _node msb;
  { if(0==_numval(msb))
    return _makenumber(0);
   else return _makenumber(((_numval(x)/_numval(msb))+
     (2*_numval(__F2(&msb,&x)))));
   }
_initconst () 
 {_initint () ; 
_in_file(_open_file(_makestring("rb.dta")));

 __saatana=NIL;
{ _close_file(_in_file(NIL));
  _in_file(_makenumber(0));
  };}

main (argc , argv) 
int argc ; char *argv[] ; 
{ _initconst () ; 
rb(_arg (argc , argv , 2) ,_arg (argc , argv , 1) );
return (0) ; } 
