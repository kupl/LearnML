type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank heap = 
    match heap with 
    | EMPTY -> -1
    | NODE (r, _, _, _) -> r

let shake (x, lh, rh) = if (rank lh) >= (rank rh)
                        then NODE (rank rh+1, x, lh, rh)
                        else NODE (rank lh+1, x, rh, lh)

let rec merge (lh, rh) =
    match lh with
    | EMPTY -> rh
    | NODE (_, lx, llh, lrh) ->
            (match rh with
            | EMPTY -> lh
            | NODE (_, rx, rlh, rrh) ->
                    if lx <= rx then shake (lx, llh, merge (lrh, rh))
                    else shake (rx, rlh, merge (lh, rrh))
			)

let insert (x, h) = merge (h, NODE (0, x, EMPTY, EMPTY))

let findMin heap =
    match heap with
    | EMPTY -> raise EmptyHeap
    | NODE(_, x, _, _) -> x

let deleteMin heap =
    match heap with
    | EMPTY -> raise EmptyHeap
    | NODE (_, x, lh, rh) -> merge (lh, rh)

(*

let rec string_of_heap h = match h with
  | EMPTY -> ""
  | NODE(r,x,EMPTY,EMPTY) -> "rank("^string_of_int r^")"^string_of_int x
  | NODE(r,x,l,EMPTY) -> "rank("^string_of_int r^")"^"NODE("^string_of_heap l^"/ "^string_of_int x^")"
  | NODE(ra,x,EMPTY,r) -> "rank("^string_of_int ra^")"^"NODE("^string_of_int x^" \\"^string_of_heap r^")"
  | NODE(ra,x,l,r) -> "rank("^string_of_int ra^")"^"NODE("^string_of_heap l^"/ "^string_of_int x^" \\"^string_of_heap r^")"

let insert_n n h = insert(n,h)
let (@@) f a = f a

let _ =
  let (a,b) =
  let h = insert_n 33 @@ insert_n 30 @@ insert_n 34 @@ insert_n 25 @@ insert_n 17 @@ insert_n 21 @@ insert_n 14 @@ insert_n 15 @@ insert_n 9 @@ insert_n 4 @@ EMPTY in (print_string (string_of_heap h));(findMin h, rank h)
  in
  print_int a;
  print_string ",";
  print_int b;
  print_newline()
  

let _ =
  let (a,b) =
let h = insert_n 7 @@ insert_n 18 @@ insert_n 17 @@ insert_n 3 @@ insert_n 11 @@ insert_n 13 @@ insert_n 8 @@ insert_n 4 @@ insert_n 5 @@ insert_n 21 @@ EMPTY in (findMin h, rank h)
  in
  print_int a;
  print_string ",";
  print_int b;
  print_newline()

let _ =
  let (a,b) =
let h = insert_n 36 @@ insert_n 9 @@ insert_n 31 @@ insert_n 30 @@ insert_n 14 @@ insert_n 23 @@ insert_n 4 @@ insert_n 18 @@ insert_n 27 @@ insert_n 38 @@ EMPTY in (findMin h, rank h)
  in
  print_int a;
  print_string ",";
  print_int b;
  print_newline()

let _ =
  let (a,b) =
let h = insert_n 13 @@ insert_n 29 @@ insert_n 5 @@ insert_n 22 @@ insert_n 17 @@ insert_n 10 @@ insert_n 14 @@ insert_n 2 @@ insert_n 27 @@ insert_n 30 @@ EMPTY in (findMin h, rank h)
  in
  print_int a;
  print_string ",";
  print_int b;
  print_newline()

let _ =
  let (a,b) =
let h = insert_n 28 @@ insert_n 13 @@ insert_n 26 @@ insert_n 24 @@ insert_n 23 @@ insert_n 8 @@ insert_n 21 @@ insert_n 18 @@ insert_n 5 @@ insert_n 31 @@ EMPTY in (findMin h, rank h)
  in
  print_int a;
  print_string ",";
  print_int b;
  print_newline()
*)
