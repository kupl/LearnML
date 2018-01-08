type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal | Italy | Germany | Sweden | England | Croatia | Argentina;; 
type tourna = LEAF of team | NODE of tourna * tourna;; 

type tournaPair = SINGLE of tourna * int | PAIR of tourna * tourna;; 

let pptree t = 
let findmax a b = if (a > b) then a else b in 
let rec depth t = match t with LEAF(a) -> 0 | NODE(a,b) -> 1 + (findmax (depth a) (depth b)) in 
let rec ppspace x = if ( x <= 0 ) then print_string("") else (print_string(" "); ppspace (x-1) ) in 
let rec ppminus x = if ( x <= 0 ) then Printf.printf("") else (Printf.printf("-"); ppminus (x-1) ) in 
let convert t = match t with 
LEAF(a) -> SINGLE(t, 0) 
| NODE(a,b) -> PAIR(a,b) in 
let rec pptreeline li x = 
match li with 
h::tail -> (match h with 
SINGLE(t, i) -> (match t with 
LEAF(a) -> (ppspace (int_of_float (2. ** (float_of_int (x+i+1))) - 1); print_string("|"); ppspace (int_of_float (2. ** (float_of_int (x+i+1)))); [SINGLE(t,i+1)]@(pptreeline tail x) ) 
| NODE(a,b) -> (ppspace (int_of_float (2. ** (float_of_int x)) - 1); Printf.printf("|"); ppspace (int_of_float (2. ** (float_of_int x))); [PAIR(a,b)]@(pptreeline tail x) ) 
) 
| PAIR(t1,t2) -> ( ppspace (int_of_float (2. ** (float_of_int x)) - 1); Printf.printf("|"); ppminus (int_of_float (2. ** (float_of_int (x + 1))) - 1); Printf.printf("|"); ppspace (int_of_float(2. ** (float_of_int x))); 
(convert t1)::(convert t2)::(pptreeline tail x) 
) 
) 
| _ -> (Printf.printf("\n"); []) 
in 
let rec pptreein l x = 
if ( x >= 0 ) then ( 
match l with 
h::tail -> (pptreein (pptreeline l x) (x - 1)) 
| _ -> Printf.printf("\n") 
) else () 
in 
pptreein ([SINGLE(t,0)]) (depth t);; 
