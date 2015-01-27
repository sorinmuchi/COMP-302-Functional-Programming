type nat = int list (* increasing list of weights, each a power of two *)

(* For example: 

5  = [1,4]
13 = [1,4,8]

*)

(* ------------------------------------------------------------------- *)
(* Q4.1 : Incrementing a binary number (10 points)                     *)
(* ------------------------------------------------------------------- *)

let inc ws = if ws = [0] then [1] else
  let rec carry n ws = match ws with
    | [] -> n :: []
    | w :: ws -> if w = n then carry (2*n) ws else n :: w :: ws
  in carry 1 ws
    

(* ------------------------------------------------------------------- *)
(* Q4.2 : Decrementing a sparse binary number  (10 points)             *)
(* ------------------------------------------------------------------- *)
exception InvalidInput
let dec ws = match ws  with
  | [] -> raise InvalidInput
  | w :: ws  -> if w = 1 then ws else
      let rec dec_rec w ws = if w = 1 then ws else dec_rec (w/2) ((w/2)::ws)
      in dec_rec w ws
      

(* ------------------------------------------------------------------- *)
(* Q4.3 : Adding sparse binary numbers  (10 points)                    *)
(* ------------------------------------------------------------------- *)

let rec add m n  = match m,n with
  | [], _ -> n
  | m,n -> add (dec m) (inc n)

(* ------------------------------------------------------------------- *)
(* Q4.3 : Converting to integer - tail recursively  (10 points)        *)
(* ------------------------------------------------------------------- *)
let sbinToInt n = 
  let rec sbinToInt_rec n acc = match n with
    | [] -> acc
    | x :: n -> sbinToInt_rec n (x + acc)
  in sbinToInt_rec n 0
