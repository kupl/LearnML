(*컴퓨터공학부 2014-16775 김민지
programming language hw 2-6*)

module type Queue = 
  sig
    type element
    type queue
    exception EMPTY_Q
    val emptyQ: queue
    val enQ: queue * element -> queue
    val deQ: queue -> element * queue
  end

module IntListQ = 
  struct
    type element = int list
    type queue = (element list) * (element list)
    exception EMPTY_Q
    let emptyQ: queue = ([], [])
    let enQ ((q: queue), (x:int list)): queue = 
      match q with
      |([], []) -> ([], [x])
      |(hx::tx, hy::ty) -> ((x::(hx::tx)), hy::ty)
      |([], hy::ty) -> ([x], hy::ty)
      |(hx::tx, []) -> ([x], (List.rev (hx::tx)))
    let deQ (q: queue): (element * queue) = 
      match q with
      |([], []) -> raise EMPTY_Q
      |(hx::tx, hy::ty) -> (hy, (hx::tx, ty))
      |([], hy::ty) -> (hy, ([], ty))
      |(hx::tx, []) -> 
        ((List.hd (List.rev (hx::tx))), ([], List.tl (List.rev (hx::tx))))
  end

