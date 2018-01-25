let rec zipper : int list * int list -> int list
=fun (a,b) -> [] 
(let rec zip (xs : int list) (ys : int list) : (int * int) list option =
  match (xs,ys) with
    ([], []) -> Some []
  | (x::xtail,[]) -> None
  | ([],y::ytail) -> None
  | (x::xtail,y::ytail) -> 
      (match zip xtail ytail with
         None -> None
       | Some zs -> Some ((x,y) :: zs))
;;
)