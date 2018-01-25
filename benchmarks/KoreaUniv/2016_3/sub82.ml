(*
 *   1. You can modify the given function specifications as recursive.
 *     2. Do not modify the function names or types.
 *       3. It is free to define any helper functions.
 *       *)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
      type aexp =
            | Const of int
              | Var of string
                | Power of string * int
                  | Times of aexp list
                    | Sum of aexp list

                      let rec diff : aexp * string -> aexp
                        = fun (exp, var) ->
                              match exp with
                                | Const n -> Const 0
                                  | Var x -> if x = var then Const 1 else Const 0
                                    | Power (x, n) -> if x = var then Times[Const n; Power(x, n - 1)] else Const 0
                                      | Times [] -> Const 0
                                        | Times (hd::tl) -> Sum[Times(diff(hd, var):: tl); Times[hd; diff(Times tl, var)]]
                                          | Sum [] -> Const 0
                                            | Sum (hd::tl) -> Sum[diff(hd, var); diff(Sum tl, var)]
end
(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
      type mobile = branch * branch
        and branch =
              | SimpleBranch of length * weight
                | CompoundBranch of length * mobile
                  and length = int
                    and weight = int

                      let rec addAllWeight
                        = fun mb ->
                              match mb with
                                | SimpleBranch(length, weight) -> weight
                                  | CompoundBranch(length, (sub_mb_left, sub_mb_right))
                                    -> (addAllWeight sub_mb_left) + (addAllWeight sub_mb_right)
                                      let rec multiply
                                        = fun mb ->
                                              match mb with
                                                | SimpleBranch(length,weight) -> length * weight
                                                  | CompoundBranch(length, (sub_mb_left, sub_mb_right)) -> length * (addAllWeight mb)


                                                    let rec balanced : mobile -> bool
                                                      = fun mob ->
                                                            match mob with
                                                              | (left, right) ->
                                                                            match left with
                                                                                  | SimpleBranch(length_simple_left, weight_simple_left) ->
                                                                                                    begin
                                                                                                                  match right with
                                                                                                                            | SimpleBranch(length_simple_right, weight_simple_right) -> if (multiply left) = (multiply right) then true else false
                                                                                                                                      | CompoundBranch(length_compound_right, (branch_left, branch_right)) -> if (balanced (branch_left, branch_right))  && (multiply left) = (multiply right) then true else false
                                                                                                                                                end
                                                                                                          | CompoundBranch(length_compound_left, (branch_one, branch_two)) ->
                                                                                                                            begin
                                                                                                                                        match right with
                                                                                                                                                  | SimpleBranch(length_simple_right, weight_simple_left) -> if (balanced (branch_one, branch_two)) && (multiply left) = (multiply right) then true else false
                                                                                                                                                            | CompoundBranch(length_compound_right, (branch_left, branch_right)) -> if (balanced (branch_one, branch_two)) && (balanced (branch_left,branch_right)) && (multiply left) = (multiply right) then true
                                                                                                                                                                      else false
                                                                                                                                                                                end

end


(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
      type exp =
            | X
              | INT of int
                | ADD of exp * exp
                  | SUB of exp * exp
                    | MUL of exp * exp
                      | DIV of exp * exp
                        | SIGMA of exp * exp * exp


                          let rec calculator : exp -> int
                            = fun exp ->
                                  match exp with
                                    | X -> raise(Failure "Wrong input")
                                      | INT m -> m
                                        | ADD (m, n) -> calculator m + calculator n
                                          | SUB (m, n) -> calculator m - calculator n
                                            | MUL (m, n) -> calculator m * calculator n
                                              | DIV (m, n) -> calculator m / calculator n
                                                | SIGMA(start, last, polynomial) ->
                                                          let s = calculator start in
                                                              let l = calculator last in
                                                                  if s = l then
                                                                          begin
                                                                                  let rec helper = fun p la ->
                                                                                          match p with
                                                                                              | X -> helper la la
                                                                                                  | INT n -> n
                                                                                                      | ADD (m, n) -> (helper m la) + (helper n la)
                                                                                                          | SUB (m, n) -> (helper m la) - (helper n la)
                                                                                                              | MUL (m, n) -> (helper m la) * (helper n la)
                                                                                                                  | DIV (m, n) -> (helper m la) / (helper n la)
                                                                                                                      | SIGMA (st, las, pol) -> calculator p
                                                                                                                          in helper polynomial last
                                                                                                                              end
                                                                             else
                                                                                   calculator (SIGMA (start, SUB(last, INT 1), polynomial)) + calculator (SIGMA(last, last, polynomial))
end


(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
      type exp =
            | V of var
              | P of var * exp
                | C of exp * exp
                  and var = string

                    let check : exp -> bool
                      = fun exp ->
                            let rec find = fun e env ->
                                  match e with
                                    | V n ->
                                                      begin
                                                                    match env with
                                                                              | [] -> false
                                                                                        | hd::tl -> if (n = hd) then true else (find e tl)
                                                                                                  end
                                                        | P (v, f) -> find f ([v] @ env)
                                                          | C (f1, f2) -> if (find f1 env) && (find f2 env) then true else false
                                                            in find exp []
end

