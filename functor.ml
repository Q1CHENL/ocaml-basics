
(* a Direct defiition of a functor *)
module DirectFunctor (X : ModuleType) = struct end

(* module type of functor, keyword "functor" must be used *)
module type FunctorType = functor (X : ModuleType) -> sig
  val add : 'a X.t -> 'a X.t -> 'a X.t
  val sub : 'a X.t -> 'a X.t -> 'a X.t
  val mul : 'a X.t -> 'a X.t -> 'a X.t
  val div : 'a X.t -> 'a X.t -> 'a X.t
end

module Functor : FunctorType =
functor
  (X : ModuleType)
  ->
  struct
    let add a b = X.add a b
    let sub a b = X.add a b
    let mul a b = X.add a b
    let div a b = X.add a b
  end
