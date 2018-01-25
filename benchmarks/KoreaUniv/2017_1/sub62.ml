(* your code goes here *)
(* problem 1*)
let fastexpt : int -> int -> int
= fun b n -> (let n = )
 if n = 0 then 1
 else if(n % 2 == 0) (x * x, n / 2);
 else return x * exponentiate(x * x, (n - 1) / 2);
 




(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let rec aux n i = 
         if i > (n / 2) then n 
         else if (n mod i == 0) then i else aux n (i + 1) in aux n 2;;


(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n,0) -> v
 |     n f = f((2+x),n)

fun iter(f,v) 0 = v
 |  iter(f,v) n = f(iter(f,v) (x+2), n);;
 




iter function of the List module ?
 a module which is bundled as part of the Ocaml standard library.
 This function takes two parameters: 
the first is another function that accepts a single parameter, 
the second is a list.




(* problem 4*)

let product : (int -> int) -> int -> int -> int

#let rec factorial n = if n=1 then 1 else n*(factorial(n-1));;
factorial : int -> int = <fun>


# let rec factorial x =
 if x <= 0 then 0 else
 if x = 1 then 1 else
 x*factorial (x - 1);;
val factorial : int -> int = <fun>




(* problem 5*)

let dfact : int -> int
= fun n -> (*TODO*)
  if n % 2 == 1:
    k = (n+1)/2
   else if fact(2*k) / (2**k * fact(k))
  else 2**k * fact(k)




(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> (*TODO*)

# let drop list n =
    let rec aux i = function
      | [] -> []
      | h :: t -> if i = n then aux 1 t else h :: aux (i+1) t  in
    aux 1 list;;
val drop : 'a list -> int -> 'a list = <fun>




(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> (*TODO*)

let rec unzip l =
 match l with [] -> ([],[])
 | (a,b) :: rest ->
 (match unzip rest with (flist, slist) -> (a::flist, b::slist));;



(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> (*TODO*)

let rec generate lst = match lst with
  | []   -> []
  | h::t -> [h] :: (List.map (fun x -> h::x) (generate t)) @ (generate t)
  |_ when (amount < 0) -> [] 
  |_ -> match coinlist with
  |[] -> [] 

(*h::f -> something, using [25;10;5;1] aka all change combinations...*)

let print_the_coin_matrix_for_all_our_joy enter_the_matrix =
print_endline (join "\n" (List.map array_to_string enter_the_matrix));;





