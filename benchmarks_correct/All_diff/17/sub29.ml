(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) ->
match e with
|Const c -> Const 0
|Var v-> if(v=x) then Const 1 else Const 0
|Power (a,b) -> if((a=x)&&(b<>0)) then Times[Const b;Power (x, (b-1))] else Const 0
|Times e -> begin match e with
            |[] -> Const 0
            | h::t -> Sum[ Times ([diff (h, x)]@t) ; Times [ h ; diff (Times t, x)]]
end
|Sum l -> begin match l with
          |[]->Const 0
          |h::t->Sum [(diff (h,x));(diff (Sum t,x))]
end