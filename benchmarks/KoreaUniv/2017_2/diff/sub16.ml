(*Problem 4*)
type aexp =
|Const of int
|Var of string
|Power of string * int
|Times of aexp list
|Sum of aexp list


let rec diff : aexp * string -> aexp
= fun (e,x) -> match e with
|Sum (lst) -> (match lst with
  |[]->Const 0
  |hd::tl -> if(tl = []) then diff (hd,x) else Sum([diff (hd,x)] @ [diff (Sum tl,x)])
  )
|Const int -> Const 0
|Var string -> if(string = x) then Const 1 else Const 0
|Power(string, int) -> if(string = x) then Times [Const int; Power(x, (int-1))] 
else Const 0
|Times (lst) -> match lst with
  |[] -> Const 0
  |hd::tl -> Times([hd] @ [diff (Sum tl,x)])
