(*probem 4*)
  type aexp=
  | Const of int
  | Var of string
  | Power of string *int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp*string -> aexp
  =fun (e,x) -> 
      match e with
      
      |Const a -> Const 0
      |Var b -> if b = x then Const 1 else Var x 
      |Power (s,i) -> if  s= x then Times [ Const i; Power(s,i-1)] else Power (s,i)
      |Times a,_ -> raise(Failure("nothing to differentiate"))
      |Times l -> if l = [] then Const 0 else 
                  begin 
                  match l with 
                  |h::t-> Sum [Times[diff(h,"x")];diff(h,"x")]
                  end
      |Sum n, []-> raise(Failure("nothing to differentiate"))
      |Sum m -> if m=[] then Const 0 else
                match m with 
                |h::t -> Sum [diff(h,"x"); diff(h,"x")]
               ;; 
