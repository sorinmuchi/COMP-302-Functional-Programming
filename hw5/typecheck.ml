open Type
module M = Minml
open Unify
  
type context = Ctx of (M.name * tp) list

let empty = Ctx []

exception Unimplemented

exception NotFound
let rec assoc x y = match y with
  | [] -> raise NotFound
  | (y, r)::rest -> if x = y then r else assoc x rest

let rec lookup ctx x = let Ctx list = ctx in assoc x list
let extend ctx (x, v) = let Ctx list = ctx in Ctx ((x,v)::list)

let rec extend_list ctx l = match l with
  | [] -> ctx
  | (x,y) :: pairs -> extend_list (extend ctx (x, y)) pairs

exception TypeError of string

let fail message = raise (TypeError message)

let primopType p = match p with
  | M.Equals   -> Arrow(Product[Int; Int], Bool)
  | M.LessThan -> Arrow(Product[Int; Int], Bool)
  | M.Plus     -> Arrow(Product[Int; Int], Int)
  | M.Minus    -> Arrow(Product[Int; Int], Int)
  | M.Times    -> Arrow(Product[Int; Int], Int)
  | M.Negate   -> Arrow(Int, Int)

(* Question 3. *)

(* infer : context -> M.exp -> tp  *)
let rec infer ctx exp = match exp with
  | M.Var y -> lookup ctx y
  | M.Int (x) -> Int
  | M.Bool (x) -> Bool
  | M.Primop (op, el) -> primopType op
  | M.If (e, e1, e2) -> (match (infer ctx e) with
    | Bool -> let typeOne = infer ctx e1 in 
        let typeTwo = infer ctx e2 in
         if typeOne = typeTwo then typeOne else (fail "Types of e1 and e2 do not match")
    | _ -> fail "e is not a boolean")

  | M.Fn (x, Some(t), e) -> let returnType = infer (extend ctx (x, t)) e in 
    Arrow (t, returnType)

  | M.Tuple (el) -> Product (List.map (infer ctx) el)

  | M.Rec (f, Some(t), e) -> let tPrime = (infer (extend ctx (f, t)) e) in 
    if t = tPrime then t else fail "T not equal to T\'"

  | M.Apply (e1, e2) -> (let typeE1 = (infer ctx e1) in
    let typeE2 = (infer ctx e2) in
    match typeE1 with 
      | Arrow(t, tPrime) -> if t = typeE2 then tPrime else fail "e2 is not of type T"
      | _ -> fail "e1 is not of type Arrow")

  | M.Let (d, e) -> (
    let decsType dec = match dec with
      | M.Val (e, x) ->  (x, (infer ctx e))  
      | M.ByName (e, x) -> (x, (infer ctx e))
    in let tPrime = List.map decsType d 
    in infer (extend_list ctx tPrime) e 
  )

  | M.Anno (e, t) -> if (infer ctx e) = t then t else fail "T and T\' do not match"
