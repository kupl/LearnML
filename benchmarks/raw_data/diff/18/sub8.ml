type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list;;

let rec diff : aexp * string -> aexp
= fun (exp, x) -> 
  match exp with
      Const integer -> Const 0
    | Var str -> 
      if (str = x) then Const 1 
      else Var str
    | Power (str, integer) -> 
      if (str <> x) then Power (str, integer)
      else Times ([Const integer] @ [Power (str,integer-1)])
    | Times list1 ->
    ( match list1 with
          [] -> Const 0
        | hd :: tl -> Sum(Times(diff(hd, x) :: tl) :: [Times (hd::[diff(Times tl, x)])])
    ) 
    | Sum list2 -> 
      match list2 with
          [] -> Const 0
        | hd :: tl -> 
          Sum ((diff (hd, x)) :: [diff (Sum tl, x)]);;
        
        
diff(Times [Const 2; Var "x"; Const 2; Var "x"] ,"x");;

(*
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, x) -> exp (*TODO*);;

*)