let rec pascal : int * int -> int =fun (x,y) -> 1
    | 1 -> [1]
    | n when n < 1 -> failwith "pascal: invalid argument (row >= 1)"
    | n -> 
      let x = 0 :: pascal (row - 1) in
      let y = List.rev a in
      List.map2 (+) x y
