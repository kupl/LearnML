(* 1. You can modify the given function specifications as recursive. *)
(* 2. However, do not modify the function names or types.            *)
(* 3. It is free to define any helper functions.                     *)

(***********************************)
(**            Problem 1          **)
(***********************************)

module Problem1 = struct
  type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  
  let balanced : mobile -> bool
  =fun (lb,rb) ->
     in let point
    | CompoundBranch ->
    | SimpleBranch -> 
  
end

(***********************************)
(**            Problem 2          **)
(***********************************)

module Problem2 = struct
  type exp = V of var
           | P of var * exp 
           | C of exp * exp
  and var = string
  
  let check ex=
    let rec excheck y x_list =
      match m with
        | V n -> List.mem n n_list
        | P (x,y1) -> excheck m1 (x::x_list)
        | C (x1, y2) -> excheck m1 x_list && excheck y2 x_list
  in
  excheck ex []
end

