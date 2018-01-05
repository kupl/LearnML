type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp

| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp

exception FreeVariable

let rec galc_helper : exp * float -> float = fun(aa,bb) ->
match aa with
| X -> raise (FreeVariable)
| INT a -> float_of_int a
| REAL a -> a
| ADD(a,b) -> ( match a,b with
               | X,X -> if bb = 0.0001 then raise (FreeVariable)
                        else bb +. bb
               | X,_ ->  if bb = 0.0001 then raise (FreeVariable)
                        else bb +. galc_helper(b,bb)
               | _,X ->  if bb = 0.0001 then raise (FreeVariable)
                        else galc_helper(a,bb) +. bb
               | _,_ -> galc_helper(a,bb) +. galc_helper(b,bb)               
              )
| SUB(a,b) -> ( match a,b with
               | X,X -> if bb = 0.0001 then raise (FreeVariable)
                        else bb -. bb
               | X,_ -> if bb = 0.0001 then raise (FreeVariable)
                        else bb -. galc_helper(b,bb)
               | _,X -> if bb = 0.0001 then raise (FreeVariable)
                        else galc_helper(a,bb) -. bb
               | _,_ -> galc_helper(a,bb) -. galc_helper(b,bb)               
              )
| MUL(a,b) -> ( match a,b with
               | X,X -> if bb = 0.0001 then raise (FreeVariable)
                        else bb *. bb
               | X,_ -> if bb = 0.0001 then raise (FreeVariable)
                        else bb *. galc_helper(b,bb)
               | _,X -> if bb = 0.0001 then raise (FreeVariable)
                        else galc_helper(a,bb) *. bb
               | _,_ -> galc_helper(a,bb) *. galc_helper(b,bb)               
              )
| DIV(a,b) -> ( match a,b with
               | X,X -> if bb = 0.0001 then raise (FreeVariable)
                        else bb /. bb
               | X,_ -> if bb = 0.0001 then raise (FreeVariable)
                        else bb /. galc_helper(b,bb)
               | _,X -> if bb = 0.0001 then raise (FreeVariable)
                        else galc_helper(a,bb) /. bb
               | _,_ -> galc_helper(a,bb) /. galc_helper(b,bb)               
              )

| SIGMA(a,b,f) -> if galc_helper(a,0.0001) > galc_helper(b,0.0001) then 0.0
                  else 
                     let bb_int = int_of_float(galc_helper(a,0.0001)) in
                     let bb = float_of_int(bb_int) in
                     galc_helper( f , bb) +. galc_helper( SIGMA( ADD(a,REAL 1.0) ,b,f) , 0.0)


| INTEGRAL(a,b,f) ->  if galc_helper(a,0.0001) > galc_helper(b,0.0001) then (-.galc_helper(INTEGRAL(b,a,f),bb))
                  else if (galc_helper(b,0.0001) -. galc_helper(a,0.0001)) < 0.1 then 0.0
                  else 
                       let bb = galc_helper(a, 0.0001) in
                       galc_helper( MUL(REAL (galc_helper(f, bb)), REAL 0.1) , 0.0) +. galc_helper( INTEGRAL( ADD(a,REAL 0.1) ,b,f) , 0.0 )
                   
let rec galculator : exp -> float = fun ab -> 
match ab with         
| x -> galc_helper(x,0.0001)     


