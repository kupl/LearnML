(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
   if a < 0 then raise (Failure "a cannot be negative") (*have to exclude the case when the starting point a is negative*)
   else
      if a > b then raise (Failure "b has to be equal to or be larger than a") (*have to exclude the case when the end point b is smaller than the starting point a*)
      else
         if a = b then f b (*when a = b, just return the value of function f of applying the argument b. It is expressed as f b, but f a will be fine as well*)
	 else (f b) * (product f a (b - 1));; (*when b > a, call the function "product" recursively while timing f b, until b becomes eqauls to a*)
