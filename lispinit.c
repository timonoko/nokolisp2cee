
#include "lispinit.inc"


  _node equal(y,x)
  
       _node x;
       _node y;
  { if(x==y)
    return T;
   else if(NIL!=x)
    if(NIL!=y)
     if(numberp(x))
      if(numberp(y))
       return _tnil(_numval(x)==_numval(y));
      else return NIL;
     else if(numberp(y))
      return NIL;
     else if(identp(x))
      return NIL;
     else if(identp(y))
      return NIL;
     else if(NIL!=equal(car(y),car(x)))
      return equal(cdr(y),cdr(x));
     else return NIL;
    else return NIL;
   else return NIL;
   }


  _node member(y,x)
  
       _node x;
       _node y;
  { if(NIL!=y)
    if(NIL!=equal(car(y),x))
     return y;
    else return member(cdr(y),x);
   else return NIL;
   }


  _node assoc(y,x)
  
       _node x;
       _node y;
  { if(NIL!=y)
    if(NIL!=equal(car(car(y)),x))
     return car(y);
    else return assoc(cdr(y),x);
   else return NIL;
   }


  _node print(x)
  
       _node x;
  { if(NIL!=x)
    if(numberp(x))
     _putint (_numval (x)) ;
    else if(identp(x))
     _putstr (_strval (x)) ;
    else { _putch(40);
     { 
           _node xL_;
      (xL_= x);
      while(NIL!=xL_)
        { { 
               _node _wpL_;
          (_wpL_= car(xL_));
          (xL_= cdr(xL_));
           print(_wpL_);
           }
         if(NIL!=xL_)
          if(atom(xL_))
           { _putch(32);
            _putch(46);
            _putch(32);
            print(xL_);
            (xL_= NIL);
            }
          else if(_numval(tab(NIL))>70)
           { _putch(13);
            tab(_makenumber(8));
            }
          else _putch(32);
         }
       }
     _putch(41);
     }
   else { _putch(40);
    _putch(41);
    }
   return x;
   }


  _node reverse(y,x)
  
       _node x;
       _node y;
  { if(NIL!=x)
    { 
          _node __1L_;
     (__1L_= cdr(x));
     return reverse(cons(y,car(x)),__1L_);
      }
   else return y;
   }


  _node append(y,x)
  
       _node x;
       _node y;
  { if(NIL!=x)
    { 
          _node __1L_;
     (__1L_= car(x));
     return cons(append(y,cdr(x)),__1L_);
      }
   else return y;
   }


  _node last(x)
  
       _node x;
  { while(NIL!=cdr(x))
    { 
          _node _wpL_;
     (_wpL_= car(x));
     (x= cdr(x));
      }
   return x;
   }


  _node nconc(y,x)
  
       _node x;
       _node y;
  { { 
         _node __1L_;
    (__1L_= last(x));
    rplacd(y,__1L_);
     }
   return x;
   }


  _node delete(y,x)
  
       _node x;
       _node y;
  { if(atom(y))
    return y;
   else if(NIL!=y)
    if(NIL!=equal(x,car(y)))
     return delete(cdr(y),x);
    else { 
          _node __5L_;
     (__5L_= delete(car(y),x));
     return cons(delete(cdr(y),x),__5L_);
      }
   else return y;
   }


  _node subst(z,y,x)
  
       _node x;
       _node y;
       _node z;
  { if(NIL!=z)
    if(NIL!=equal(z,x))
     return y;
    else if(atom(z))
     return z;
    else { 
          _node __3L_;
     (__3L_= subst(car(z),y,x));
     return cons(subst(cdr(z),y,x),__3L_);
      }
   else return z;
   }


  _node nth(y,x)
  
       _node x;
       _node y;
  { return car(nthcdr(y,x));
   }

