(*********************)
(*     Problem 1     *)
(*********************)
1. let max2 lst =
2. List.fold_left
3. (fun a b ->
4. if a > b then
5. a
6. else
7. b)
8. (List.hd lst)
9. lst
10. ;;
11. ?
12. Printf.printf "max is %d\n" (max2 [1;2;3;4;5]);
13. Printf.printf "max is %d\n" (max2 [5;4;3;2;1]);
14. Printf.printf "max is %d\n" (max2 [5;4;1;2;3]);
15. Printf.printf "max is %d\n" (max2 [-5;-4;-1;-2;-3]);

1. let min2 lst =
2. List.fold_left
3. (fun a b ->
4. if a < b then
5. a
6. else
7. b)
8. (List.hd lst)
9. lst
10. ;;
11. ?
12. Printf.printf "least is %d\n" (min2 [1;2;3;4;5]);
13. Printf.printf "least is %d\n" (min2 [5;4;3;2;1]);
14. Printf.printf "least is %d\n" (min2 [5;4;1;2;3]);
15. Printf.printf "least is %d\n" (min2 [-5;-4;-1;-2;-3]);

(*********************)
(*     Problem 2     *)
(*********************)
1. let filter f l =
2. List.fold_right (fun x a -> if f x then x :: a else a) l []
3. ?
4. ?
5. open Printf
6. let output1 = filter (fun x -> x mod 2 = 0) [1;2;3;4;5];;
7. Printf.printf "output 1 is: \n";;
8. let () = List.iter (printf "%d ") output1;;
9. Printf.printf "\n";;
10. ?
11. open Printf
12. let output2 = filter (fun x -> x > 0) [5;-1;0;2;-9];;
13. Printf.printf "output 2 is: \n";;
14. let () = List.iter (printf "%d ") output2;;
15. Printf.printf "\n";;
16. ?
17. open Printf
18. let output3 = filter (fun x -> x * x > 25) [1;2;3;4;5;6;7;8];;
19. Printf.printf "output 3 is: \n";;
20. let () = List.iter (printf "%d ") output3;;
21. Printf.printf "\n";;

(*********************)
(*     Problem 3     *)
(*********************)
1. (* this fucnction 'twice' takes a function f and a value x. 
2. * It applies function f on x, and then applies f on f(x) *)
3. let twice f x = 
4. f (f x)
5. ?
6. (* test cases *)
7. let output1 = twice (fun x -> x + 1) 1;;
8. (* example expected output is (1+1) + 1 =3 *)
9. Printf.printf "output is %d\n" (output1);;
10. ?
11. let output2 = twice (fun x -> x * 2) 3;;
12. Printf.printf "output is %d\n" (output2);;
13. ?
14. let output3 = twice (fun x -> (x * 2) + 1) 4;;
15. Printf.printf "output is %d\n" (output3);;

(*********************)
(*     Problem 4     *)
(*********************)
1. (* Trees of integers *)
2. ?
3. type inttree = Empty | Node of node
4. and node = { value : int; left : inttree; right : inttree }
5. ?
6. (* Return true if the tree contains x. *)
7. let rec search (t : inttree) (x : int) : bool =
8. match t with
9. | Empty -> false
10. | Node {value=v; left=l; right=r} ->
11. v = x || search l x || search r x
12. ?
13. let tree1 =
14. Node {value=2; left=Node {value=1; left=Empty; right=Empty};
15. right=Node {value=3; left=Empty; right=Empty}}
16. ?
17. let z = search tree1 3;;
18. Printf.printf "output is %s\n" (string_of_bool z);;
19. ?
20. let x = search tree1 18;;
21. Printf.printf "output is %s\n" (string_of_bool x);;

(*********************)
(*     Problem 5     *)
(*********************)
type nat = Zero | Succ of nat;;

let rec add n1 n2 = 
  match n1 with
    Zero -> n2
  | Succ n1_minus_1 -> (add n1_minus_1 (Succ n2))
  
let rec multiply n1 n2 =
  match n1 with
    Zero -> Zero
  | Succ n1MinusOne -> add n2 (mul n1MinusOne n2 )
  
let output1 = add(four,four);
Printf.printf "Output is %d\n" (output1);;

let output2 = add(two,four);
Printf.printf "Output is %d\n" (output2);;

(*********************)
(*     Problem 6     *)
(*********************)
type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

let rec eval : formula -> bool
= fun f -> true (* TODO *)


