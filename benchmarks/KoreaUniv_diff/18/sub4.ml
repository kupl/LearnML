type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
  
(*let rec listToelement : 'a list -> 'a list -> 'a list
= fun l1 l2 -> *) (*l1은 [Const 1]로 시작*)
 (*)match l2 with
    | [] -> l1
    | hd::tl -> (l1 @ [hd]) @ listToelement l1 tl;;*)

    
let listToelement2 : 'a list -> aexp
= fun lst ->
  match lst with
    | hd::tl -> match hd with
                  | Times l -> Times l
                  | Sum l -> Sum l
                  | Const (x) -> Const x
                  | Var (x) -> Var x
                  | Power (s, n) -> Power (s, n)

let rec diff : aexp * string -> aexp
= fun (aexp, x) ->
  match aexp with
    | Const (n) -> Const 0
    | Var (s) -> if s = x then Const 1 else Var s
    | Power (s, n) -> if s = x && n > 2 then Times [Const n ; Power (s, n - 1)] 
                      else if s = x && n = 2 then Times [Const n ; Var x]
                      else Power (s, n) 
    | Times l -> diff_Times l x
    | Sum l -> diff_Sum l x

and diff_Times : aexp list -> string -> aexp
= fun l x ->
  match l with
    | hd::tl -> Sum [Times [diff(hd, x) ; (listToelement2 tl)] ; Times [hd ; diff(listToelement2 tl, x)]]
      
and diff_Sum : aexp list -> string -> aexp
= fun l x ->
  match l with
    | hd::tl -> Sum [diff(hd, x) ; diff(listToelement2 tl, x)];;


    (*| Times l -> match l with
                  | hd::tl -> Sum ((Times (diff (hd, x) :: tl)) @ (hd :: Times (diff (Times tl, x)))) 
    | Sum l -> match l with
    
                |hd::tl -> Sum (diff(hd, x) :: diff(tl, x));;*)
 
 
 
diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1], "x");;



