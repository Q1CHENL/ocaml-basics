(* In this exercise, we will implement monoids as modules in OCaml. A monoid groups together
   a type t, an associative binary operation plus, and an identity element zero. That means
   that:
   plus (plus x y) z is equal to plus x (plus y z)
   plus zero x and plus x zero both return x *)

(* a direct module definition *)
module IntTripleMonoid = struct
  (* polymorphism like type 'a t = 'a list is less common in direct 
     module definition, because in a direct module def, a specific 
     implementation is often required *)
  type triple = Triple of int * int * int

  let zero = Triple (0, 0, 0)
  let plus t1 t2 = 
    let (Triple (a, b, c)) = t1 in 
    let (Triple (d, e, f)) = t2 in
    Triple (a + d, b + e, c + f)

end

(* This module can be used to hide the function plus of module IntTripleMonoid by 
   not defining it. Except for plus, the other parts of it must be conforming to IntTripleMonoid *)
module type ZeroTripleMonoid = sig
  type triple = Triple of int * int * int
  val zero: triple
end

(* Here, a module ZeroTripleMonoidModule is defined conforming to module type
   ZeroTripleMonoid, with a implementation of IntTripleMonoid, which must conform to
   ZeroTripleMonoid as well *)
module ZeroTripleMonoidModule: ZeroTripleMonoid = IntTripleMonoid

(* Usage examples: *)
(* ZeroTripleMonoidModule.zero is accessible *)
let zero_t = ZeroTripleMonoidModule.zero
(*  The constructor Triple of type truple is accessible *)
let zero_triple = ZeroTripleMonoidModule.Triple (0, 0, 0)

type triple = ZeroTripleMonoidModule.triple 

(* ZeroTripleMonoidModule.plus is not accessible *)
(* let sum = ZeroTripleMonoidModule.plus (Triple (0, 0, 0)) (Triple (1, 1, 1)) *)

(* a module type definition Monoid: *)
module type Monoid = sig
  (* The reason why we declare such a type is to define the types of other values below,
    and further to define the behaviour of the module conforming to this, otherwise the 
    def of the type can just be omitted *)
  type 'a t

  val zero : 'a t
  val plus : 'a t -> 'a t -> 'a t
end

(* Notice: similar to functor type, a module type cannot be implemented by another module type,
   but only a module (implementation) *)
(* module type M: Monoid = sig ... end ===> cannot complile *)

(* Here, "with type 'a t = 'a list" is mandatory as well as "type 'a t = 'a list"
   inside the module definition. 

   Actually, it still compiles without external type specification "with type 'a t = 'a list",
   but error will occur when used e.g "example_listmonoid" below
   *)
module ListMonoid : Monoid with type 'a t = 'a list = struct
  type 'a t = 'a list

  let zero = []
  let plus l1 l2 = l1 @ l2
end

(* Error occurs without "with type 'a t = 'a list":
   [ "a"; "b" ] has type 'a list but an expression was expected of type
   'b ListMonoid.t 

   'a list is because that's the function sig defined internally
   'b ListMonoid.t is because the type ListMonoid.t is not "officially" defined externally
   so there is conflict
   *)
let example_listmonoid = ListMonoid.plus [ "a"; "b" ] [ "c"; "d"; "e" ]

module FunctionMonoid : Monoid with type 'a t = 'a -> 'a = struct
  type 'a t = 'a -> 'a

  let zero x = x
  let plus f g x = f (g x)
end

let ex_fun_1 = FunctionMonoid.zero "a"
let ex_fun_2 = FunctionMonoid.plus (fun x -> "f " ^ x) (fun x -> "g " ^ x) "y"

module PairListFlippedListMonoid : Monoid with type 'a t = 'a list * 'a list =
struct
  type 'a t = 'a list * 'a list

  let zero = ([], [])
  let plus pair1 pair2 = (fst pair2 @ fst pair1, snd pair1 @ snd pair2)
end

let ex_plf =
  PairListFlippedListMonoid.plus ([ "a" ], [ "x"; "y" ]) ([ "b"; "c" ], [ "z" ])
