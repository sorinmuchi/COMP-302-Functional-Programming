module TreeCont : TREE = 
struct

  type 'a tree = Node of 'a * ('a tree) list 

  let find p t = match t with
	Node(x, children) -> if p x then (Some x) else let rec rec_find p t_list c = match t_list with
		| [] -> c ()
		| Node(y, yl) :: tl -> if p y then (Some y) else (rec_find p yl (fun () -> rec_find p tl c))
		in rec_find p children (fun () -> None) 
end
