(* a direct definition of a module without 
   conforming to a certain module type *)
module DirectModule = struct
  type 'a t = float
  let add a b = a +. b
  let sub a b = a -. b

  let mul a b = a *. b
  let div a b = a /. b
end

(* module type of a regular module *)
(* it can be used to hide certains parts of a module *)
(* here we will hide functtion sub in module DirectModule *)
module type ModuleType = sig 
  type 'a t
  val add: 'a t -> 'a t -> 'a t
  val sub: 'a t -> 'a t -> 'a t
  val mul: 'a t -> 'a t -> 'a t
  val div: 'a t -> 'a t -> 'a t
end

(* here we let DirectMoudleHide conforms to the ModuleType, which only has a
   function sig add, and let also have the same implementation of DirectModule.
   This way we essentially hide sub in DirectModule *)
module DirectModuleHide: ModuleType = DirectModule

(* a module conforms to the module type ModuleType *)
module Module: ModuleType = struct
  type 'a t = int 
  let add a b = a + b
  let sub a b = a - b
  let mul a b = a * b
  let div a b = a / b
end 

(* module type of functor, keyword "functor" must be used *)
module type FunctorType = functor (X: ModuleType) -> sig
  val add: 'a X.t -> 'a X.t -> 'a X.t
  val sub: 'a X.t -> 'a X.t -> 'a X.t
  val mul: 'a X.t -> 'a X.t -> 'a X.t
  
  val div: 'a X.t -> 'a X.t -> 'a X.t
end

module Functor: FunctorType = functor(X: ModuleType) -> struct
  let add a b = X.add a b
  let sub a b = X.add a b
  let mul a b = X.add a b
  let div a b = X.add a b

end

(* a Direct defiition of a functor *)
module DirectFunctor (X: ModuleType) = struct

end