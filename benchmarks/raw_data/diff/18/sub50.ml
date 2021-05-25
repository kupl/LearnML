type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list
  
let rec num_x = fun lst s-> (*Time안에 변수가 몇 개 들어 있는지 알기 위해*)
  if lst = [] then 0 else
    match List.hd lst with
      |Times a' -> num_x a' s + num_x (List.tl lst) s
      |Power(a',b') -> if a' = s && b'<> 0 then b' + num_x (List.tl lst) s else 0 +num_x (List.tl lst) s
      |Var a' -> if a'=s then 1 + num_x (List.tl lst) s else 0 +num_x (List.tl lst) s
      |Const a' -> if a'=0 then 0 else 0 +num_x (List.tl lst) s;;
      
let rec dif_times_lst = fun lst x ->
  match List.hd lst with
    |Const x' -> [Const x']@dif_times_lst (List.tl lst) x
    |Var x' -> if x' = x then (List.tl lst) else [Var x']@dif_times_lst (List.tl lst) x
    |_->[];;

let rec dif_Times = fun lst x ->
  match num_x lst x with 
    |0-> [Const 0]
    |1-> dif_times_lst lst x
    |_->[Const (num_x lst x)]@dif_times_lst lst x;;
  
let rec dif_sums_lst = fun lst x ->
  match lst with 
    |[]->[]
    |_-> match List.hd lst with
          |Const x' -> [Const 0]@dif_sums_lst (List.tl lst) x
          |Var x' -> if x'= x then [Const 1]@ (List.tl lst) else [Const 0]@dif_sums_lst (List.tl lst) x
          |Sum a' -> if num_x a' x =0 then [Const 0] else dif_sums_lst a' x @ dif_sums_lst (List.tl lst) x
          |Power(a',b')-> if a' = x then match b' with
                                          |1->[Const 1]@dif_sums_lst (List.tl lst) x
                                          |_->[Times[Const b';Power(a',b'-1)]]@dif_sums_lst (List.tl lst) x
                          else [Const 0];;
          
let rec dif_all_lst = fun lst x ->
  match lst with
    |[]->[]
    |_->match List.hd lst with
      |Const x' -> [Const 0]@dif_all_lst (List.tl lst) x
      |Var x' -> if x'= x then [Const 1]@ (List.tl lst) else [Const 0]@dif_all_lst (List.tl lst) x
      |Sum a' -> if num_x a' x =0 then [Const 0] else dif_sums_lst a' x @ dif_all_lst (List.tl lst) x
      |Power(a',b')-> if a' = x then match b' with
                                      |1->[Const 1]@dif_all_lst (List.tl lst) x
                                      |_->[Times[Const b';Power(a',b'-1)]]@dif_all_lst (List.tl lst) x
                      else [Const 0]
      |Times a' -> [Times(dif_Times a' x)]


let rec diff : aexp * string -> aexp
= fun (exp, x) ->
  match exp with 
    |Const x' -> Const 0 
    |Var x' -> if x'= x then Const 1 else Const 0
    |Sum a' -> Sum (dif_all_lst a' x) 
    |Power(a',b')-> if a' = x then match b' with
                                    |1->Const 1
                                    |_->Times[Const b';Power(a',b'-1)]
                    else Const 0
    |Times a' -> Times (dif_all_lst a' x);;
    
diff (Sum[Const 2;Times[Const 2;Var "x";Var "y"]],"x");;(*2+2x^2 -> 4x*)
diff (Power("x",4),"x");;
diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1],"x");;(*x^2+2x+1 -> 2x+2*)
diff (Sum [Power ("x", 4); Times [Const 2; Var "x"]; Const 1],"x");;(*x^4+2x+1 -> 4x^3+2*)
diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1],"x");;(*x^2+2x+1 -> 2x+2*)
