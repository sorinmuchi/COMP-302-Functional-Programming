(* Using modules for Working with different metrics *)

module type METRIC = 
sig 
  type t 
  val unit : t 
  val plus : t -> t -> t 
  val prod : float -> t -> t 
  val toString : t -> string
  val toFloat  : t -> float
  val fromFloat : float -> t
end;;

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
(* Question 1.1 *)
(* Define a module Float which provides an implementation of 
   the signature METRIC; 

   We then want use the module Float to create different representations
   for Meter, KM, Feet, Miles, Celsius, and Fahrenheit, Hour 
*)
(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

module Float = 
struct 
  type t = float 
  let unit = 1.0 
  let plus = (+.) 
  let prod = ( *. ) 
  let toString  x = string_of_float x
  let toFloat x = x
  let fromFloat x = x
end;;

module Meter = (Float : METRIC);; 
module KM = (Float : METRIC);; 
module Feet = (Float : METRIC);;
module Miles = (Float : METRIC);;
module Celsius = (Float : METRIC);;
module Fahrenheit = (Float : METRIC);;
module Hour = (Float : METRIC);;

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
(* Question 1.2 *)
(* Define a functor Speed which implements the module type SPEED. We 
   want to be able to compute the speed km per hour as well as 
   miles per hour. 

   The functor Speed must therefore be parameterized.
*)
(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
 
module type SPEED =  
sig
  type s
  type distance 
  val speed :  distance -> Hour.t -> s 
  val average : s list -> s 
end;;

module Speed (M : METRIC) : (SPEED with type distance = M.t) = 
struct 
  type s = float 
  type distance = M.t

  let speed m (h:Hour.t) = 
    (M.toFloat m) /. (Hour.toFloat h)

  let toFloat s = s 
  let average s = 
    let l = List.length s in 
    let rec sum s = match s with
      | [] -> 0.0 
      | s::xs -> s +. (sum xs)
    in 
    (sum s) /. (float l)
end;;
(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
(* Question 1.3 *)
(* Show how to use the functor Speed to obtain an implementations
   for computing miles per hour in the module MilesPerHour and
   and implementation computing kilometers per hour in the module
   KMPerHour
*)
(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

module MilesPerHour = Speed (Miles);; 
module KMPerHour = Speed (KM);; 

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
(* Question 1.4 *)
(* It is useful to convert between different metrics.

   Define a module type CONVERSION which specifies the following
   conversion functions:
   - feet2meter          meter = feet * 0.3048
   - fahrenheit2celsius  celsius = (fahrenheit - 32) / 1.8
   - miles2KM            km = miles * 1.60934
   - MilesPerHour2KMPerHour 

   Then implement a module which provides these conversion functions.
*)
(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

module type CONVERSION = 
sig
  val feet2meter : Feet.t -> Meter.t 
  val fahrenheit2celsius : Fahrenheit.t -> Celsius.t 
  val miles2KM : Miles.t -> KM.t 
  val meter2KM : Meter.t -> KM.t
end;;

module Conversion : CONVERSION = 
struct
  let feet2meter (x:Feet.t) = 
   (* Meter.fromFloat*)
    let f = ((Feet.toFloat x) *. 0.3048) in
    Meter.fromFloat f
    
  let fahrenheit2celsius (x:Fahrenheit.t) = 
    let c = ((Fahrenheit.toFloat x) -. 32.0) /. 1.8 in 
      Celsius.fromFloat c

  let miles2KM (x:Miles.t) = 
    let k = (Miles.toFloat x) *. 1.60934 in 
      KM.fromFloat (k)

  let meter2KM (x:Meter.t) = 
    KM.fromFloat ((Meter.toFloat x) /. 100.0)

end;;
