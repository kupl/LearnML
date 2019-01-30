type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> 
  let rec sigma (n1, n2, f) = 
    if(n1>n2) then 0
    else (f n1) + sigma(n1+1, n2, f)
    in
  let rec tofun e = match e with
    X->(fun x->x)
    |INT n -> (fun x->n)
    |ADD (e,f) -> (fun x-> ((tofun e) x)+((tofun f) x))
    |SUB (e,f) -> (fun x-> ((tofun e) x)-((tofun f) x))
    |MUL (e,f) -> (fun x-> ((tofun e) x)*((tofun f) x))
    |DIV (e,f) -> (fun x-> if ((tofun f) x)=0 then raise (Failure "error: division by ZERO") else ((tofun e) x)/((tofun f) x))
    |SIGMA (e,f,g) -> (fun x-> sigma((tofun e) x,(tofun f) x,(tofun g)))
    in
  match exp with
    X-> raise (Failure "error : free variable")
    |INT n -> n
    |ADD (e, f) -> let ve = (calculator e) in let vf = (calculator f) in (ve+vf)
    |SUB (e, f) -> let ve = (calculator e) in let vf = (calculator f) in (ve-vf)
    |MUL (e, f) -> let ve = (calculator e) in let vf = (calculator f) in (ve*vf)
    |DIV (e, f) -> let ve = (calculator e) in let vf = (calculator f) in if vf =0 then raise (Failure "error: division by ZERO") else (ve/vf) 
    |SIGMA (k, l, x) -> let ck = (calculator k) in let cl = (calculator l) in let tx = (tofun x) in
    sigma(ck,cl,tx);;
    
    

      