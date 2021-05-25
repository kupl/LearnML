(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e,x) -> (* TODO *)
  match e with
	|Const c -> Const 0
	|Var y -> if y=x then Const 1 else Const 0
	|Power (z,a) -> (if z=x then Times [Const a; Power (z,a-1)] else Const 0)
  |Sum sl ->( 
        match sl with
        | [] -> Sum []
        | hd::tl -> 
        (
          let newt = diff (Sum tl, x) in 
            (match newt with 
              |Sum ntl -> Sum (diff (hd,x)::ntl)
              |_-> Sum [diff (hd,x)]
            )
        ))
  |Times l ->(
        match l with 
        |[]-> Const 0
        |hd::tl -> (Sum [(Times ([diff (hd,x)]@tl)); (Times [hd;diff (Times tl,x)])])
        )
