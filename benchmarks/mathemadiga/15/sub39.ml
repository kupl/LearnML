type exp = X
| INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp;;
exception FreeVariable;;


let galculator: exp -> float = fun e ->
  let rec galdo : exp * float list -> float = fun (ex,fb) ->
    match ex with
    | X -> (match fb with
           | [] -> raise FreeVariable
           | hd::tl -> hd)
    | INT(i)  -> (float_of_int i)
    | REAL(f) -> f 
    | ADD(ex1,ex2) -> (galdo (ex1,fb))+.(galdo (ex2,fb))
    | SUB(ex1,ex2) -> (galdo (ex1,fb))-.(galdo (ex2,fb))
    | MUL(ex1,ex2) -> (galdo (ex1,fb))*.(galdo (ex2,fb))
    | DIV(ex1,ex2) -> (galdo (ex1,fb))/.(galdo (ex2,fb))
    | SIGMA(ex1,ex2,ex3) -> 
      let start = (int_of_float (galdo (ex1,fb)))
      in
      let ed = (int_of_float (galdo (ex2,fb)))
      in
      if start>ed then (float_of_int 0)
      else
        (galdo(ex3,(float_of_int start)::[]))+.galdo(SIGMA(INT(start+1),INT(ed),ex3),fb)
    
    | INTEGRAL(ex1,ex2,ex3) -> (
      let start=galdo(ex1,fb)
      in
      let ed=galdo(ex2,fb)
      in
      if start>ed then -1.0*.(galdo(INTEGRAL(ex2,ex1,ex3),fb))
      else
        if ed<(0.1+.start) then 0.0
        else galdo(ex3,start::[])+.galdo(INTEGRAL(REAL(start+.0.1),REAL(ed),ex3),fb)

    )
  in
  (galdo (e,[]))
;;


let check (f: unit -> bool): unit =
    print_endline (if f () then "O" else "X")

let n1 = INT 3
let n2 = REAL 1.2
let n3 = INT (-2)
let n4 = REAL 0.8

let x1 = ADD (X, INT 1)
let x2 = MUL (X, (MUL (INT 2, X)))
let x3 = SUB (MUL (X, X), INT 1)

let s1 = SIGMA (INT 0, INT 1, X)
let s2 = SIGMA (INT 0, X, MUL (MUL (X, X), INT 3))
let s3 = SIGMA (s1, INT 10, s2)

let check_tight : float -> float -> bool =
    fun a b ->
          a -. b < 0.00001 &&  b -. a < 0.00001

let check_loose : float -> float -> bool =
    fun a b ->
          a -. b < 0.5 && b -. a < 0.5

          (* Arithmatic *)
let _ = check (fun () -> check_tight (galculator n1) 3.0)
let _ = check (fun () -> check_tight (galculator n2) 1.2)
let _ = check (fun () -> check_tight (galculator n3) (-2.0))
let _ = check (fun () -> check_tight (galculator n4) 0.8)
let _ = check (fun () -> check_tight (galculator (ADD (n1, n2))) 4.2)
let _ = check (fun () -> check_tight (galculator (ADD (ADD (n1, n2), n3))) 2.2)
let _ = check (fun () -> check_tight (galculator (ADD (ADD (n1, n2), n4))) 5.0)
let _ = check (fun () -> check_tight (galculator (SUB (n1, n2))) 1.8)
let _ = check (fun () -> check_tight (galculator (SUB (n4, n3))) 2.8)
let _ = check (fun () -> check_tight (galculator (SUB (SUB (n4, n3), n3))) 4.8)
let _ = check (fun () -> check_tight (galculator (MUL (n1, n2))) 3.6)
let _ = check (fun () -> check_tight (galculator (MUL (ADD (n3, n4), n2))) (-1.44))
let _ = check (fun () -> check_tight (galculator (MUL (n1, (SUB (INT 0, n2))))) (-3.6))
let _ = check (fun () -> check_tight (galculator (DIV (n1, n2))) 2.5)
let _ = check (fun () -> check_tight (galculator (DIV (n4, n3))) (-0.4))
let _ = check (fun () ->
    try check_tight (galculator X) 123.0 with FreeVariable -> true | _ -> false)

(* Sigma *)
let _ = check (fun () -> check_tight (galculator (SIGMA (INT 1, INT 10, REAL 0.5))) 5.0)
let _ = check (fun () -> check_tight (galculator (SIGMA (INT 1, INT 10, X))) 55.0)
let _ = check (fun () -> check_tight (galculator (SIGMA (REAL 1.0, INT 100, x1))) 5150.0)
let _ = check (fun () -> check_tight (galculator (SIGMA (REAL 1.0, REAL 10.1, x2))) 770.0)
let _ = check (fun () -> check_tight (galculator (SIGMA (INT 4, INT 12, MUL ((SUB (X, REAL 1.0)), x1)))) 627.0)
let _ = check (fun () -> check_tight (galculator (SIGMA (INT 4, INT 12, x3))) 627.0)
let _ = check (fun () -> check_tight (galculator s3) 3630.0)
let _ = check (fun () ->
    check_tight (galculator
      (SIGMA
          (SUB (INT 3, REAL 1.0),
              SIGMA (INT 1, INT 3, X),
                  SIGMA (X, ADD (X, X), SUB (MUL (INT 2, MUL (X, X)), MUL (REAL 3.0, X))))))
      2015.0)
let _ = check (fun () ->
    check_tight (galculator
      (SIGMA (SIGMA (INT 2, INT 1, X), INT 10,
          (SIGMA (SIGMA (INT (-1), INT 1, X), X,
                (SIGMA (INT 0, X, MUL (X, X))))))))
      3289.0)
let _ = check (fun () ->
    try check_tight (galculator (SIGMA (INT 0, X, X))) 0.0 with FreeVariable -> true | _ -> false)

(* Integral *)
let _ = check (fun () ->
    check_loose (galculator (INTEGRAL (INT 2, REAL 2.05, ADD (X, X)))) 0.0)
let _ = check (fun () ->
    check_loose (galculator (INTEGRAL (REAL (-2.0), REAL (-2.05), DIV (X, X)))) 0.0)
let _ = check (fun () ->
    check_loose (galculator (INTEGRAL (INT 0, INT 2, REAL 0.5))) 1.0)
let _ = check (fun () ->
    check_loose (galculator (INTEGRAL (INT 2, INT 0, REAL (-1.0)))) 2.0)
let _ = check (fun () ->
    check_loose (galculator (INTEGRAL (INT 0, INT 2, MUL (X, INT 2)))) 3.8)
let _ = check (fun () ->
    check_loose (galculator (INTEGRAL (REAL 1.55, REAL 1.37, X))) (-0.137))
let _ = check (fun () ->
    check_loose (galculator (INTEGRAL (INT 2, INT 0, MUL (X, X)))) (-2.47))
let _ = check (fun () ->
    check_loose (galculator (INTEGRAL (REAL 0.1, INT 1, DIV (INT 100, X)))) 282.896)
let _ = check (fun () ->
    check_loose (galculator (INTEGRAL (REAL 10.0, REAL 1.0, SUB(MUL(X, X), INT 1)))) (-319.065))
let _ = check (fun () ->
    check_loose (galculator (INTEGRAL (INT 1, INT (-1), MUL (MUL (X, X), X)))) 0.1)
let _ = check (fun () ->
    check_loose (galculator (INTEGRAL (INT 1, INT 0, MUL (INT 3, MUL (X, X))))) (-0.855))
let _ = check (fun () ->
    try check_loose (galculator (INTEGRAL (INT 0, MUL (INT 1, X), X))) 0.0 with FreeVariable -> true | _ -> false)
let _ = check (fun () ->
    try check_loose
        (galculator
              (INTEGRAL
                      (ADD (X, REAL 1.0),
                              SIGMA (REAL 1.0, REAL 10.0, SUB (MUL(X, X), INT 1)),
                                      SUB(MUL(X, X), X))))
            0.0 with FreeVariable -> true | _ -> false)
let _ = check (fun () ->
    try check_loose
        (galculator
              (SIGMA
                      (INTEGRAL (INT 0, MUL (INT 1, INT 0), X),
                              INTEGRAL (SUB (INT 1, MUL (INT 2, X)), INT 30, REAL 10.5),
                                      MUL (X, X))))
            0.0 with FreeVariable -> true | _ -> false)
