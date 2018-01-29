
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list;;

let rec diff : aexp * string -> aexp
= fun (aexp, x) -> (match aexp with
  Const n -> Const 0
| Var v -> if v=x then Const 1 else Const 0
| Power (s,n) -> (if s=x then
								(match n with
									  0 -> Const 0
									| 1 -> Const 1
									| 2 -> Times [Const 2 ; Var s]
									| _ -> Times [Const n ; Power (s,n-1)])
								else Const 0)
| Times lst -> (match lst with 
								  [] -> Const 1
								| hd :: [] -> diff (hd,x)
								| hd :: tl -> match hd with
															  Const 0 -> Const 0
															| Const 1 -> diff (Times tl,x)
															| Const n -> Times[Const n ; diff (Times tl,x)]
															| _ -> Sum[Times (diff (hd,x)::tl);Times[hd ; diff (Times tl, x)]])
| Sum lst -> (match lst with
							  [] -> Const 0
							| hd :: [] -> diff (hd,x)
							| hd :: tl -> Sum [diff (hd,x) ; diff (Sum tl,x)]));;