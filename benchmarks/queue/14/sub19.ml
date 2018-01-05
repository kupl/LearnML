module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyQ: queue
  val enQ: queue * element -> queue
  val deQ: queue -> element * queue
end

module IntListQ : Queue with type element = int list = 
struct 
  type element = int list
  type queue = element list * element list 
  exception EMPTY_Q

  let emptyQ =
    ([], [])

  let enQ: queue * element -> queue = 
    fun (q, elem) ->
    let (lQueue, rQueue) = q in
      ((List.append [elem] lQueue), rQueue) (* L list의 맨 앞에 enqueue *) 
        
  let split l i k =
    let rec take n l =
      match l with
      | [] -> []
      | h::t -> if n = 0 then []
                else h::(take (n-1) t)
    in
    let rec drop n l =
       match l with
       | [] -> []
       | h::t as li -> if n = 0 then li
                       else (drop (n-1) t)
    in
    take (k - i + 1) (drop i l)

  let deQ: queue -> element * queue = 
    fun q  ->
      let (lQueue, rQueue) = q in
	if q = emptyQ then
	   raise EMPTY_Q
	else
	let lenL = (List.length lQueue) in
	let lenR = (List.length rQueue) in
	   if rQueue = [] then 
 	      let deqElem = (List.nth lQueue (lenL-1)) in
	         (deqElem, ((split lQueue 0 (lenL-2)), rQueue))
	   else
	      let deqElem = (List.hd rQueue) in
	         (deqElem, (lQueue, (split rQueue 1 (lenR-1))))
	  
end
