type aexp = Const of int
  |Var of string
  |Power of string * int
  |Times of aexp list
  |Sum of aexp list
exception InvalidArgument
let rec diff (aexp,dx) =
  match aexp with
  |Const a -> Const 0
  |Var a -> if a=dx then Const 1 else Const 0
  |Power (a,b) ->
      if a=dx then 
	(if b=0 then Const 0
	else Times [Const b;Power (a,b-1)])
	else Const 0
  |Times lst -> (match lst with
    |[] -> raise InvalidArgument
    |[one] -> diff (one,dx)
    |_ -> Sum [Times ((diff (List.hd lst, dx))::List.tl lst); Times ([(List.hd lst);(diff (Times (List.tl lst),dx))])])
  |Sum lst -> if []=lst then raise InvalidArgument
  else Sum (List.map (fun a -> diff (a,dx)) lst)
