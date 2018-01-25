(* problem 7*)

let unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst -> let rec func lst a b = match lst with
                                [] -> (List.rev(a),List.rev(b))
                                |hd::tl -> (match hd with
                                          (fr,ba) ->  func tl (fr::a) (ba::b))
                                          
  in func lst [] []