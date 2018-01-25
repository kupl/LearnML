(* problem 1*)
let fastexpt : int -> int -> int
= fun b n -> let rec a b n = if n=0 then 1 
                                    else if (n mod 2 = 0) then a (b*b) (n/2)
                                                  else (a b (n-1)) * b
  in a b n

(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let rec a n r = if r*r > n then n
                                      else if (n mod r = 0) then r
                                           else a n (r+1)
  in a n 2

(* problem 3*)

let iter : int * (int -> int) -> (int -> int)
= fun (n, f) -> if n = 0 then f else
                let rec a n f x= if n>0 then f ( a (n-1) f x) 
                                        else x
               in a n f

(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b -> let rec func n = if n=b then f n
                                       else f n * (func (n+1))
  in func a

(* problem 5*)

let dfact : int -> int
= fun n -> let rec func n = if n=1 || n=2 then n
                                          else n * func (n-2)
  in func n

(* problem 6*)

let drop : 'a list -> int -> 'a list
= fun l n -> let rec func l n = if n=0 then l
                                       else match l with
                                       hd::tl -> func tl (n-1)
                                       | [] -> []
  in func l n

(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> let rec func lst a b = match lst with
                                [] -> (List.rev(a),List.rev(b))
                                |hd::tl -> (match hd with
                                          (fr,ba) ->  func tl (fr::a) (ba::b))
                                          
  in func lst [] []

(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> let rec func lst c = if c = 0 then 1
                                          else if c < 0 then 0 
                                           else match lst with
                                              [] -> 0
                                              |hd::tl -> (match tl with
                                                          [] -> func lst (c-hd)
                                                          | _ -> (func lst (c-hd)) + (func tl c))
  in func coins amount
