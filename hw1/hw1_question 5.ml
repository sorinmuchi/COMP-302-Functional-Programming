type prop = 
  | Atom of string
  | Neg of prop
  | Conj of prop * prop
  | Disj of prop * prop
  | Impl of prop * prop


let rec nnf p = match p with 
  | Neg(Neg (a)) -> nnf a
  | Neg(Conj(a,b)) -> Disj((nnf (Neg(a))),(nnf (Neg(b))))
  | Neg(Disj(a, b)) -> Conj((nnf (Neg(a))),(nnf (Neg(b))))
  | Impl(a,b) -> Disj((nnf (Neg(a))),(nnf b))
  | Atom(a) -> Atom(a)
  | Neg(a) -> Neg(a)
  | p -> p 

let f1 = Neg (Conj (Atom "p", Disj (Atom "q", Atom "r")));;
let f2 = Neg (Conj (Neg (Atom "p"), Disj (Atom "q", Atom "r")))
