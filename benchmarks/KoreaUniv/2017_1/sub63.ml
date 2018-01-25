(*2015170311_배성진*)

(*p1*)

let rec fastexpt :int->int->int
  =fun b n -> if (n=1) then b
  else if (n mod 2)=0 then (fastexpt b (n/2))*(fastexpt b (n/2))
  else b*(fastexpt b (n-1));;

(*For instance*)
(*
# fastexpt 2 3;;
- : int = 8
*)

(*p2*)

let smallest_divisor: int->int
= fun n-> let rec f i =
if (n<i*i) then n
else if ((n mod i)=0) then i
else f (i+1)
in f 2;;

(*For instance*)
(*
# smallest_divisor 15;;
- : int = 3
# smallest_divisor 121;;
- : int = 11
# smallest_divisor 141;;
- : int = 3
# smallest_divisor 199;;
- : int = 199
*)


(*p3*)

let rec iter : int*(int->int)->(int->int)
=fun (n,f)-> if (n=0) then (fun x->x)
else fun x->f( (iter((n-1),f)) x);;

(*For instance*)
(*
# iter(10,fun x->(2+x))0;;
- : int = 20
*)


(*p4*)

let rec product : (int->int)->int->int->int
=fun f a b-> if (a=b) then (f a)
else (f b)*(product f a (b-1));;

(*For instance*)
(*
# product (fun x ->(2*x)) 1 3;;
- : int = 48
*)

(*p5*)

let rec dfact:int->int
=fun n-> if (n=1) then 1
else if (n=2) then 2
else n*(dfact (n-2));;
 

(*For instance*)
(*
# dfact 7;;
- : int = 105
*)


(*p6*)

let rec drop : 'a list->int->'a list
=fun l n -> if n=0 then l
else drop (match l with |[]->[]| h::t->t) (n-1);;

(*For instance*)
(*
# drop [1;2;3;4;5] 2;;
- : int list = [3; 4; 5]
# drop [1;2] 3;;
- : int list = []
# drop ["C";"Java";"OCaml"] 2;;
- : string list = ["OCaml"]
*)

(*p7*)

let rec unzip : ('a*'b) list -> 'a list * 'b list
= fun lst-> match lst with
|[]->([],[])
|[('a,'b)]->(['a],['b])
|('a,'b)::t-> let (l1,l2) = unzip t in (['a],['b]);;


(*For instance*)
(*

*)

(*p8*)

let change : int list->int->int
=fun coins amount-> let rec reverse coins1=
match coins1 wuth
|[]->[]
|h::t->(reverse t)@[h] in coins1=coins in coins=reverse coins1
if (*총합에서 잔돈중 최고 큰 것을 최대한으로 뺍니다.(빼는 잔돈의 양보다 작아질때 까지)->더이상 뺄 수 없다면 남은 총합에서 그 다음크기의 잔돈을 최대한으로 뺍니다.-> 이것을 반복합니다.-> 가장 작은 잔돈이 더이상 뺄 수 없을 때 남은 총합이 0이라면 카운팅하고 0이 아니라면 카운팅하지 않습니다.->*)


(*For instance*)
(*

*)


