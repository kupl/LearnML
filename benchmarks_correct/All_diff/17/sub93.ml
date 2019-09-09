(* problem 4 *)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
|Const n->Const 0
|Var a -> if a=x then Const 1 else Const 0
|Power (a, n) -> if a=x then Times[Const n ; Power (a, (n-1))] else Const 0
|Times li ->(match li with
            |[] -> Const 0
            |hd::[]->diff (hd, x)
            |hd::tl ->(match hd with
                      |Const n -> Times [hd; diff (Times tl, x)]
                      |_->Sum ([Times ([diff (hd,x)]@tl)]@[Times ([hd]@[diff ((Times tl),x)])])))
|Sum li-> (match li with
          |[]->Const 0
          |hd::[] -> diff(hd, x)
          |hd::tl-> (match hd with 
                    |Const n -> diff (Sum tl, x)
                    |_->Sum ([diff (hd, x)]@[diff((Sum tl), x)])));;