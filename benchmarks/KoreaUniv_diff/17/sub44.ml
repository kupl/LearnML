(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) ->
  let rec ae f isfactor =
    match f with
    |Sum a -> let rec sumhelp b =
                match b with
                |[] -> []
                |hd::tl -> (ae hd false)::(sumhelp tl)
              in Sum (sumhelp a)
    |Times a -> let isx = 
                  let rec xsearch term = 
                    match term with
                    |[] -> false
                    |hd::tl -> match hd with
                              |Var a -> if a = x then true
                                        else xsearch tl
                              |Power (a,b) -> if a = x then
                                              (if b = 0 then xsearch tl
                                                else true)
                                              else xsearch tl
                              |_ -> xsearch tl
                  in xsearch a
                in let rec timeshelp b =
                    match b with
                    |[] -> []
                    |hd::tl -> (ae hd isx)::(timeshelp tl)
                  in Times (timeshelp a)
    |Power (a,b) -> if a <> x then if isfactor then Power (a,b) else Const 0
                  else if b = 1 then Const 1
                  else if b = 0 then if isfactor then Const 1 else Const 0
                  else Times [(Const b); (Power (a,(b-1)))]
    |Var a -> if a = x then Const 1
              else if isfactor then Var a 
              else Const 0
    |Const a -> if isfactor then Const a 
                else Const 0
    in ae e false;;