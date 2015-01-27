

(* unify: typ * typ -> unit 

   unify(T1, T2) = () 
   
   succeeds if T1 and T2 are unifiable, fails otherwise.

   Side Effect: Type variables in T1 and T2 will have been
    updated, s.t. if T1 and T2 are unifiable AFTER unification
    they will denote the "same" type.
*)

open Type
module M = Minml

exception Error of string
exception Unimplemented

let freshVar () = TVar (ref None)

let rec occurs s t = match s, t with
  | _, Int -> false
  | _, Bool -> false
  | _, Arrow (t1, t2) ->
      (occurs s t1) || (occurs s t2)
  | _, Product ts ->
      List.exists (occurs s) ts
  | _, TVar r ->
   match !r with
    | None -> (s == r)
    | Some t' -> (s == r) || (occurs s t')

(* Question 4. *)
let rec unify s t = match s, t with
  | Int, Int -> ()
  | Bool, Bool -> ()
  | Arrow(s1, s2), Arrow(t1, t2) -> unify s1 t1; unify s2 t2
  | Product(ss), Product(ts) -> List.inter2 unify ss ts
  | y, TVar(x) 
  | TVar(x), y -> if occurs x y then raise (Error "Variable occurs inside the other") 
    else (if !x = None then x := (Some y) 
      else (if !x = (Some y) then () 
        else raise (Error "Doesn't match")))
  | TVar(s0), TVar(t0) -> if s0 = t0 then () 
    else (if !s0 = None then s0 := !t0 
      else (if !t0 = None then t0 := !s0 
        else raise (Error "TVar not compatible")))
  | _, _ -> raise (Error "Does not match a valid case in unify!") 

let unifiable (t, t') = 
  try
    let _ = unify t t' in true
  with Error s -> false
