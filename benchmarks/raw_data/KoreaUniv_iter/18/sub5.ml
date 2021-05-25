let rec realIter
= fun (n, f, f_original) -> match n with
  |0 -> fun x->x
  |1 -> f
  |_ -> realIter(n-1, (fun x->f(f_original x)) , f_original);;

let rec iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> realIter (n, f, f);;
