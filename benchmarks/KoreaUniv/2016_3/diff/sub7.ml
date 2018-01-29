
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diffhelp: (aexp * string) -> aexp
  = fun (exp, string) ->
  match exp with
  | Const i -> Const 0
  | Var str -> if (str = string) then Const 1
               else exp
  | Power (str, i) -> if (str = string) then Times [Const i; Power (string, i-1)]
                      else exp
  | Times [Const i; xp] -> Times [Const i; diffhelp (xp, string)]
  | Times [Power (str1,i1); Power (str2,i2)] ->
                      if (str1 = str2) 
                      then diffhelp (Power (str1, i1 + i2), string)
                      else Times [diffhelp ((Power (str1, i1), string)); 
                      diffhelp ((Power (str2, i2), string))]
  |_ -> exp ;;

  let rec clean: aexp -> aexp
  = fun exp ->
  match exp with
  | Times [Const 1; xp] -> xp
  | Times [Const i; Const 1] -> Const i
  | Times [Const i; Power (str, 1)] -> Times [Const i; Var str]
  | Times [Const i1; Times [Const i2; xp]] -> clean (Times [Const (i1 * i2); xp])
  |_ -> exp;;

 let rec listofdiff: (aexp list * string ) -> (aexp list)
 =fun (list, string) ->
 match list with
 | [] -> []
 | hd::tl -> if ((diffhelp (hd, string)) = Const 0) then listofdiff (tl, string) 
             else  [clean (diffhelp (hd, string))] @  listofdiff (tl, string);;

  let diff : aexp * string -> aexp
  = fun (exp, var) -> 
  match exp with
  | Sum l -> Sum (listofdiff (l, var))
  |_ -> clean (diffhelp (exp, var));; (* TODO *)