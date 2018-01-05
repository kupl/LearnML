exception FreeVariable
type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp
let rec delfrontzero a =
  match a with
    | hd::tl -> if hd = 0. then delfrontzero tl else a
    | [] -> []

let rec listlast a =
  let b = List.tl a in
  match b with
    | hd::tl -> listlast b
    | [] -> List.hd a

let getval (a:int) (b:float list) =
  let rec getvalrec a b n =
    if (List.length b) == 0 then 0. else ((float_of_int a)**(float_of_int n)) *. (listlast b) +. (getvalrec a (List.rev (List.tl (List.rev b))) (n+1)) in
  getvalrec a b 0

let getvalfloat (a:float) (b:float list) =
  let rec getvalrec a b n =
    if (List.length b) == 0 then 0. else (a**(float_of_int n)) *. (listlast b) +. (getvalrec a (List.rev (List.tl (List.rev b))) (n+1)) in
  getvalrec a b 0
  

let rec getsigmarec (a,b,c) =
  if a = b then getval a c else if a < b then (getval a c) +. getsigmarec(a+1,b,c) else 0.

let getsigma (a,b,c) =
  getsigmarec (int_of_float a,int_of_float b,c)

let m a b = a*.b
let shift a n =
  let rec mkzero (n:int) =
    if n = 0 then [] else 0.::(mkzero (n-1)) in
  List.append a (mkzero n)
let sum a b =
let rec sumiter a b =
  match (a,b) with
    | ([],[]) -> []
    | (hd::tl,[]) -> hd::(sumiter tl [])
    | ([],hd::tl) -> hd::(sumiter [] tl)
    | (h1::t1,h2::t2) -> (h1+.h2)::(sumiter t1 t2) in
  delfrontzero (List.rev (sumiter (List.rev a) (List.rev b)))
  
let sub a b =
let rec subiter a b =
  match (a,b) with
    | ([],[]) -> []
    | (hd::tl,[]) -> hd::(subiter tl [])
    | ([],hd::tl) -> (0. -. hd)::(subiter [] tl)
    | (h1::t1,h2::t2) -> (h1-.h2)::(subiter t1 t2) in
  delfrontzero (List.rev (subiter (List.rev a) (List.rev b)))
  
let rec mul a b =
  match b with
    | hd::tl -> sum (shift (List.map (m hd) a) (List.length tl)) (mul a tl)
    | [] -> [0.]
  
let rec div a b =
  match a with
    | hd::tl -> if (List.length a) >= (List.length b) then sum (shift [hd/.(List.hd b)] ((List.length a) - (List.length b))) (div (sub tl (List.tl (mul b (shift [hd/.(List.hd b)] ((List.length a) - (List.length b)))))) b) else [0.]
    | [] -> [0.]
let getintegral (a,b,c) =
  let num = if b > a then int_of_float ((b-.a) /. 0.1) else int_of_float ((a-.b) /. 0.1) in
let rec getintegralrec (a,b,c) n =
  if n=0 then 0. else (0.1 *. (getvalfloat a c)) +. (getintegralrec ((a +. 0.1),b,c) (n-1)) in
if a = b then 0. else if a > b then -.(getintegralrec (b,a,c) num) else getintegralrec (a,b,c) num

let rec galculator p : float =
  let rec getpoly e : float list =
  match e with
    | X -> [1.;0.]
    | INT a -> [float_of_int a]
    | REAL a -> [a]
    | ADD (a,b) -> sum (getpoly a) (getpoly b)
    | SUB (a,b) -> sub (getpoly a) (getpoly b)
    | MUL (a,b) -> mul (getpoly a) (getpoly b)
    | DIV (a,b) -> div (getpoly a) (getpoly b)
    | SIGMA (a,b,c) -> [(galculator e)]
    | INTEGRAL (a,b,c) -> [(galculator e)] in
match p with
  | X -> raise FreeVariable
  | INT a -> float_of_int a
  | REAL a -> a
  | ADD (a,b) -> (galculator a) +. (galculator b)
  | SUB (a,b) -> (galculator a) -. (galculator b)
  | MUL (a,b) -> (galculator a) *. (galculator b)
  | DIV (a,b) -> (galculator a) /. (galculator b)
  | SIGMA (a,b,c) -> getsigma(galculator a,galculator b,getpoly c)
  | INTEGRAL (a,b,c) -> getintegral(galculator a,galculator b,getpoly c)