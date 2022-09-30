(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)
(*
*  Because there is no  pointer (maybe) in cool languages, so I decide to use the linked stack
* First,to construct the stacknode
* Second, use the stacknode we built to construct the Stack and some operations
* Third, to deal with the Main()
* Because ,in cool kanguage,a class may inherit only from a single class,so I copy the functions from atoi.cl in order to use i2a,a2i in my main fuction
*)
 class Stacknode{
   --define operations on empty stacknode
   isNil() : Bool { true };
   head()  : String { { abort(); ""; } };
   tail()  : Stacknode { { abort(); self; } };
   cons(i : String) : Stacknode {
       (new Stacknode1).init(i, self)
   };
};

class Stacknode1 inherits Stacknode{
   --define nonempty stacknode
   item : String;
   next : Stacknode;
   isNil() : Bool { false };
   head()  : String { item };
   tail()  : Stacknode { next };
   init(i : String, rest : Stacknode) : Stacknode {
       {
       item <- i;
       next <- rest;
       self;
       }
   };
};

class Stack inherits IO{
   --define operations on stack
   top : Stacknode;
   count : Int;
   temp : Stacknode1;
   p : Stacknode;
   initStack() : Stack{
       {
       top <- new Stacknode;
       count <- 0;
       self;
       }
   };
   stackLength() : Int{count};
   getTop() : String{top.head()};
   getTopNode():Stacknode{top};
   push(i:String) : Stack{
       {
       temp <- new Stacknode1;
       temp.init(i,getTopNode());
       top <- temp;
       count <- (count +1);
       self;
       }
   };
   pop(i:String) : String{
       if (count=0) then {
           abort();
           "";
        }
        else{
           count <- (count-1);
           i <- getTop();
           top <- top.tail();
           i;
        }
        fi
   };
   -- traverse the stack and print the elements
   stackTraverse(): Object{
       {
       p <- getTopNode();
       out_string("\n");
       while(not p.isNil()) loop
       {
           out_string(p.head());
           out_string("\n");
           p<-p.tail();
       }
       pool;}
   };
};

class Main inherits IO{
   mylist : Stack;
   parametera : String;
   parameterb : String;
   parameterc : String;
   instring : String;
   (*
   c2i   Converts a 1-character string to an integer.  Aborts
         if the string is not "0" through "9"
   *)
   c2i(char : String) : Int {
       if char = "0" then 0 else
       if char = "1" then 1 else
       if char = "2" then 2 else
           if char = "3" then 3 else
           if char = "4" then 4 else
           if char = "5" then 5 else
           if char = "6" then 6 else
           if char = "7" then 7 else
           if char = "8" then 8 else
           if char = "9" then 9 else
           { abort(); 0; }  -- the 0 is needed to satisfy the typchecker
           fi fi fi fi fi fi fi fi fi fi
        };
   
   (*
      i2c is the inverse of c2i.
   *)
        i2c(i : Int) : String {
       if i = 0 then "0" else
       if i = 1 then "1" else
       if i = 2 then "2" else
       if i = 3 then "3" else
       if i = 4 then "4" else
       if i = 5 then "5" else
       if i = 6 then "6" else
       if i = 7 then "7" else
       if i = 8 then "8" else
       if i = 9 then "9" else
       { abort(); ""; }  -- the "" is needed to satisfy the typchecker
           fi fi fi fi fi fi fi fi fi fi
        };
   
   (*
      a2i converts an ASCII string into an integer.  The empty string
   is converted to 0.  Signed and unsigned strings are handled.  The
   method aborts if the string does not represent an integer.  Very
   long strings of digits produce strange answers because of arithmetic 
   overflow.
   
   *)
        a2i(s : String) : Int {
           if s.length() = 0 then 0 else
       if s.substr(0,1) = "-" then ~a2i_aux(s.substr(1,s.length()-1)) else
           if s.substr(0,1) = "+" then a2i_aux(s.substr(1,s.length()-1)) else
              a2i_aux(s)
           fi fi fi
        };
   
   (*
     a2i_aux converts the usigned portion of the string.  As a programming
   example, this method is written iteratively.
   *)
        a2i_aux(s : String) : Int {
       (let int : Int <- 0 in	
              {	
                  (let j : Int <- s.length() in
                 (let i : Int <- 0 in
               while i < j loop
               {
                   int <- int * 10 + c2i(s.substr(i,1));
                   i <- i + 1;
               }
               pool
             )
              );
                 int;
           }
           )
        };
   
   (*
       i2a converts an integer to a string.  Positive and negative 
   numbers are handled correctly.  
   *)
       i2a(i : Int) : String {
       if i = 0 then "0" else 
           if 0 < i then i2a_aux(i) else
             "-".concat(i2a_aux(i * ~1)) 
           fi fi
       };
       
   (*
       i2a_aux is an example using recursion.
   *)		
       i2a_aux(i : Int) : String {
           if i = 0 then "" else 
           (let next : Int <- i / 10 in
           i2a_aux(next).concat(i2c(i - next * 10))
           )
           fi
       };
   --function to cope with the input except "e"
   excutmethod(stack : Stack,instring : String): Object {
       {
       if (instring="d")then
           stack.stackTraverse()
       else
              if(instring="x")then{
                  out_string("\n");
                  out_string("stop");
                  out_string("\n");
                  abort();}
              else
                  if(instring="e")then
                      specialexcute(stack)
                   else
                       stack.push(instring)
                   fi
               fi
       fi;
       }
   };
    
   --The behavior of the ‘e’ command depends on the contents of the stack when ‘e’ is issued
   specialexcute(stack : Stack) : Object {
       {
           if(stack.stackLength() = 0) then 0
           else{
           parameterc <- stack.pop(parameterc) ;
       if(parameterc="+")then{
           parametera <- stack.pop(parametera);
           parameterb <- stack.pop(parameterb);
           parameterc <- i2a(a2i(parametera)+a2i(parameterb));
           stack.push(parameterc);
       }else{
       if(parameterc="s")then{
           parametera <- stack.pop(parametera);
           parameterb <- stack.pop(parameterb);
           stack.push(parametera);
           stack.push(parameterb);
       }else{
        if(parameterc=i2a(a2i(parameterc)))then 0
       else{
           out_string("\n");
           out_string("Error");
           abort();
       }
       fi;
       }
       fi;
       }
       fi;
       }
       fi;
   }
};

main() : Object{
    {
      mylist <- new Stack.initStack();
       while (true) loop
       {
          (new IO).out_string(">");
          instring <- (new IO).in_string();
          excutmethod(mylist , instring);
       }
       pool;
    }
   };
};
