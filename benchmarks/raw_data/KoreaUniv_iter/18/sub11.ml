let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> match n with
    | 0 -> let g : int -> int
           = fun c -> c in g 
    | _ -> fun c -> let doit x = f c in iter(n-1, f) (doit c);;


iter(9, fun x -> 2 + x) 0;;

(*



let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f)c -> if n = 0 then c else (iter(n-1, f))(f(c));;

let f x = x + 2;;
iter (2, f)0;;


let doit : (int->int)*(int->int) -> (int->int)
            = fun a b -> match a with
              | 
let g : int->int
= fun c -> c;;
let rec iter (n,f) = if n = 0 then g else f(iter(n-1, f));;
let f x = 2 + x;;

f(f(f (g 0)));;*)