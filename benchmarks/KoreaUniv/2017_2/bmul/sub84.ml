(* problem 7*)
type digit = ZERO | ONE
type bin = digit list;;
      
let rec length:bin->int
 =fun b->
 match b with
 |[]->0
 |hd::tl->1+length tl;;
             
 let rec toint:digit->int
 =fun d->
 match d with
 |ZERO->0
 |ONE->1;;
                   
                    
 let rec todec:bin->int->int
  =fun b l->
 match b with
 |[]->0
 |hd::tl->toint hd*(fastexpt 2 l)+(todec tl (l-1));;
                          
let rec tobin:int->bin
=fun n->
if n=1 then [ONE]
else if n=0 then [ZERO]
else if (n mod 2)=0 then (tobin (n/2))@[ZERO]
else (tobin (n/2))@[ONE];;
                                 
                                  
let bmul:bin->bin->bin
=fun b1 b2->
tobin (todec b1 ((length b1)-1)*todec b2 ((length b2)-1));;

