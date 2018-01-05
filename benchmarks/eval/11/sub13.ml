(* PL HW2-3, "계산해줘!"
   2007-11738
   알렉산더 *)

(* Type defenition *)
type expr = NUM of int
          | PLUS of expr * expr
          | MINUS of expr * expr
          | MULT of expr * expr
          | DIVIDE of expr * expr
          | MAX of expr list

(* Main function *)
(* eval: expr -> int *)
let rec eval e = 
    
    (* get Max number from list *)
    let maxNum (eList) =
        let rec getMax (num, eL) =
            match eL with
                [] -> num
              | h::t -> (if (eval num < eval h) then getMax (h, t)
                         else getMax (num, t))
        in
        match eList with
              [] -> NUM 0
            | h::t -> getMax (h, t)
    in
    
    (* eval main function *)
    match e with
         NUM i -> i
       | PLUS (e1, e2) -> (eval e1) + (eval e2)
       | MINUS (e1, e2) -> (eval e1) - (eval e2)
       | MULT (e1, e2) -> (eval e1) * (eval e2)
       | DIVIDE (e1, e2) -> (eval e1) / (eval e2)
       | MAX expList -> eval (maxNum (expList))
