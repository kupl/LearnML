(* hw2.1 *)



type crazy2 = NIL 
            | ZERO of crazy2 
            | ONE of crazy2 
            | MONE of crazy2


(*crazy2val: crazy2 -> int*)



let rec crazy2len (c: crazy2) : int = 
  match c with
    | NIL -> 0
    | ZERO a -> 1 + (crazy2len a)
    | ONE a -> 1 + (crazy2len a)
    | MONE a -> 1 + (crazy2len a)


let _ = crazy2len (ZERO(ONE(MONE NIL)))

let rec crazy2power (p: int) : int = 
  match p with
    | 0 -> 1
    | _ -> 2 * crazy2power (p - 1)

let _ = crazy2power 3


let rec crazy2val_aux (c: crazy2) (p: int) : int = 
  match c with
    | NIL -> 0
    | ZERO c_sub -> (crazy2val_aux c_sub (p+1))
    | ONE c_sub -> (crazy2val_aux c_sub (p+1)) + (crazy2power p)
    | MONE c_sub -> (crazy2val_aux c_sub (p+1)) - (crazy2power p) 


let crazy2val (c: crazy2) : int =
  crazy2val_aux c 0



(* testcase *)


(*
let _ = crazy2val (ZERO(ONE(MONE NIL)))




let _= 
let print_bool x = print_endline (string_of_bool x) in 
print_bool (-1  = (crazy2val (MONE NIL))); 
print_bool (1   = (crazy2val (ONE (ZERO (ZERO (ZERO NIL)))))); 
print_bool (1   = (crazy2val (ONE NIL))); 
print_bool (9   = (crazy2val (MONE (MONE (ONE (ONE (ZERO NIL))))))); 
print_bool (-13 = (crazy2val (MONE (ZERO (ONE (ZERO (ONE (MONE NIL)))))))) 
;; 
*)
