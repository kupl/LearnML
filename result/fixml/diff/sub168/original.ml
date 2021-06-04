type aexp =
  | Const of int
  | Var of string
  | Power of (string * int)
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp =
 fun (exp, x) ->
  match (exp, x) with
  | Const a, x -> Const 0
  | Var a, x -> if a = x then Const 1 else Var a
  | Power (a, b), x ->
      if a = x then Times [ Const b; Power (a, b - 1) ] else Power (a, b)
  | Times li, x -> Sum (diff_time li x)
  | Sum li, x -> Sum (diff_sum li x)


and diff_time : aexp list -> string -> aexp list =
 fun li x ->
  match li with
  | [] -> []
  | h :: t ->
      if t != [] then
        [ Times ([ diff (h, x) ] @ t) ] @ [ Times ([ h ] @ diff_time t x) ]
      else [ Times ([ diff (h, x) ] @ t) ]


and diff_sum : aexp list -> string -> aexp list =
 fun li x -> match li with [] -> [] | h :: t -> [ diff (h, x) ] @ diff_sum t x
