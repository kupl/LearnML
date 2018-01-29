
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
  = fun (exp, var) -> match exp with
		|Const (i) -> Const (0)
		|Var (s) ->  if s=var then Const (1) else Const (0)
		|Power (s, i) ->if s=var then  Times [Const (i); Power (s, i-1)]
										else Const (0)
		|Times (aexp_list) ->
			let rec times_list_differentiate lst = match lst with
			|[] -> [Const (0)]
			|h::[] -> [diff (h, var)]
			|h::t -> [Times [diff (h, var); Times (t)]; Times [h; diff (Times (t), var)]]
			in
			Sum (times_list_differentiate (aexp_list))
		|Sum (aexp_list) ->
					  let rec sum_list_differentiate lst = match lst with
						|[] -> [Const (0)]
						|h::[] -> [diff (h, var)]
						|h::t -> diff (h, var) :: sum_list_differentiate (t)
						in
						Sum (sum_list_differentiate (aexp_list))