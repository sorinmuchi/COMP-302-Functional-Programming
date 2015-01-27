type suit = Clubs | Spades | Hearts | Diamonds

type rank =  Six | Seven | Eight | Nine | Ten | 
             Jack | Queen | King | Ace

type card = rank * suit

type hand = Empty | Hand of card * hand

(* dom : suit -> suit -> bool

   dom(s1,s2) = true iff suit s1 beats or is equal to suit s2
                relative to the ordering S > H > D > C         
   Invariants: none
   Effects: none
*)

let dom s1 s2 = match s1, s2 with
  | Spades, _        -> true
  | Hearts, Diamonds -> true
  | Hearts, Clubs    -> true
  | Diamonds, Clubs  -> true
  | s1, s2           -> s1 = s2


let dom_rank r1 r2 = match r1, r2 with
  | Ace, _ -> true
  | _ , Ace -> false
  | King, _ -> true
  | _, King -> false
  | Queen, _ -> true
  | _, Queen -> false
  | Jack, _ -> true
  | _, Jack -> false
  | Ten, _ -> true
  | _, Ten -> false
  | Nine, _ -> true
  | _, Nine -> false
  | Eight, _ -> true
  | _, Eight -> false
  | Seven, _ -> true
  | _, Seven -> false
  | Six, _ -> false
    

let greater (r1, s1) (r2, s2) = 
  if s1 = s2 then dom_rank r1 r2 else dom s1 s2

let rec insert c h = 
  match h with
    | Empty -> Hand(c , Empty)
    | Hand(x ,h') -> if (greater c x) then Hand(x, insert c h') else Hand(c,Hand(x,h'))
  

let rec ins_sort h =
  match h with 
    | Empty -> Empty
    | Hand(oneCard, h') -> insert oneCard (ins_sort h')



