module List_Library : TAILREC_LIST_LIBRARY = 
  struct
    (* We reimplement some of OCaml's list library functions tail-recursively. As a
     result, these functions will be able to run on large lists and we will not
     get a stack overflow.
     
     *)
    
    exception NotImplemented
		
    (* partition list *)
    (* partition : ('a -> bool) -> 'a list -> 'a list * 'a list
     
     partition p l returns a pair of lists (l1, l2), where l1 is the list of all the elements of l that satisfy the predicate p, and l2 is the list of all the elements of l that donot satisfy p. The order of the elements in the input list is preserved.
     
     *)

	let partition p list =
		let rec rec_part p l c = match l with
			| [] -> c ([], [])
			| hd::tl -> if (p hd) then rec_part p tl (fun (l1, l2) -> c(hd::l1, l2)) else rec_part p tl (fun (l1, l2) -> c(l1, hd::l2))
		in rec_part p list (fun (l1, l2) -> (l1, l2))    

  end
