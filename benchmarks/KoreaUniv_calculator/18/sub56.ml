type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec cal_SIGMA = fun st fi exp sum->
  let rec cal = fun exp y ->
    match exp with
      |X -> y
      |INT a' -> a'
      |ADD (a',b')-> (cal a' y + cal b' y)
      |SUB (a',b')-> (cal a' y - cal b' y)
      |MUL (a',b')-> (cal a' y * cal b' y)
      |DIV (a',b')-> (cal a' y / cal b' y)
      |SIGMA(a',b',c') ->cal_SIGMA (cal a' y) (cal b' y) c' 0
      in if st<=fi then cal_SIGMA (st+1) fi exp (sum+ cal exp st) else sum;; 
    

let rec calculator : exp -> int
= fun exp -> 
  match exp with
    |X->0
    |INT a' -> a'
    |ADD (a',b')-> calculator a' + calculator b'
    |SUB (a',b')-> calculator a' - calculator b'
    |MUL (a',b')-> calculator a' * calculator b'
    |DIV (a',b')-> calculator a' / calculator b'
    |SIGMA (a',b',c')-> cal_SIGMA (calculator a') (calculator b') c' 0;; 
    
