(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n ->
  match n with
  |0 -> 1
  |_ ->
    if (n mod 2)=1 then b * (fastexpt b (n-1))
    else (fastexpt b (n/2))*(fastexpt b (n/2)) 



(* problem 2*)
let rec ans : int -> int -> int
= fun i n ->
  if i*i >n then -1
  else begin
    if ((n mod i) = 0) then i
    else ans (i+2) n
  end

let smallest_divisor : int -> int
= fun n ->
  if n mod 2 = 0 then 2
  else begin
    let answer  = ans 3 n in
    if answer = -1 then n
    else answer
  end

(* problem 3*)

let compose f g = fun x -> f(g(x))


let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) ->
  match n with
  |1 -> f
  |_ -> let asdf = iter(n-1,f) in
        compose f asdf


(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
  match b-a with
  |0 -> f(b) 
  |_ -> 
    let num  = f(a) in
    let num2 = product f (a+1) b in
    num*num2

(* problem 5*)

let rec dfact : int -> int
= fun n -> 
  match n with
  |2 -> 2
  |1 -> 1
  |_ -> n*(dfact(n-2))

(* problem 6*)

let rec drop : 'a list -> int -> 'a list
= fun l n ->
  match l with
  | [] -> []
  | hd::tl ->
    if n = 1 then tl
    else drop tl (n-1)
(* problem 7*)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> 
  match lst with
  | [] -> ([],[])
  | hd::tl ->
    let (a,b) = hd in
    let (a_list,b_list) = unzip(tl) in
    ([a]@a_list,[b]@b_list)

(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount ->
  (match amount with
  | 0 -> 1
  | _ ->
    (match coins with
    | [] -> 0
    | hd::tl ->
      if amount < 0 then 0
      else  (change coins (amount-hd))+(change tl amount)
    )
  )

(*let _ = 
  let a11 = fastexpt 3 3 in
  let a12 = fastexpt 3 2 in
  let a21 = smallest_divisor 15 in
  let a22 = smallest_divisor 121 in
  let a23 = smallest_divisor 8 in
  let a24 = smallest_divisor 199 in
  let a3 = iter(120,fun x-> 2+x) 0 in
  let a41 = product (fun x -> x) 1 5 in
  let a42 = product (fun x -> x) 4 6 in
  let a6 = drop ["C";"Java";"Ocaml"] 2 in
  let (a7,b7) = unzip [(1,"one");(2,"two");(3,"three")] in
  let a81 = change [1;5;10] 12 in
  let a82 = change [1;5;10;25;50] 100 in
  let a83 = change [1;5;10] 0 in
  let a84 = change [1;5;10] (-5) in
  let a85 = change [] 10 in
  print_int a11;
  print_string "\n";
  print_int a12;
  print_string "\n";
  print_int a21;
  print_string "\n";
  print_int a22;
  print_string "\n";
  print_int a23;
  print_string "\n";
  print_int a24;
  print_string "\n";
  print_int a3;
  print_string "\n";
  print_int a41;
  print_string "\n";
  print_int a42;
  print_string "\n";
  print_int a81;
  print_string "\n";
  print_int a82;
  print_string "\n";
  print_int a83;
  print_string "\n";
  print_int a84;
  print_string "\n";
  print_int a85;
  List.iter (fun x -> print_string x) a6;
  List.iter (fun x -> print_int x) a7;
  List.iter (fun x -> print_string x) b7;*)
