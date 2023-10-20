(* a direct definition of a module without *)
(* note that expressions like "type 'a t = float" should not be used , just
   write type t = int if needed, since 'a t means t is meant to be parameterised by 'a *)
module DirectModule = struct
  type 'a pair = 'a * 'a

  let paired a b = (a, b)
  let first (a, b) = a
  let second (a, b) = b
end

(* a polymorphic module type of a regular module *)
(* it can be used to hide certains parts of a module definition*)
(* here we will hide function second in module DirectModule *)
module type ModuleType = sig
  type 'a pair

  val paired : 'a -> 'a -> 'a * 'a
  val first : 'a * 'a -> 'a
  (* val second : 'a * 'a -> 'a *)
end

(* here we make DirectMoudleHide conforms to the ModuleType, which only has a
   function first, and make it also have the same implementation of DirectModule.
   This way we essentially hide second in DirectModule. In other words, the ModuleType
   provides an another view of DirectModule *)

(* DirectModule must conform to ModuleType, that is to have type 'a pair and function first defined,
   since it will be the implementation of DirectModuoleHide, which conforms to ModuleType *)
module DirectModuleHide : ModuleType = DirectModule

(* Usage example: *)
let mypair = DirectModule.paired 1 2
let second = DirectModule.second mypair

(* accessible *)
(* not accessible *)

(* let second = DirectModuleHide.second mypair *)

(* a module conforming to the module type ModuleType *)
module Module : ModuleType = struct
  type 'a pair = int list * int list

  let paired a b = (a, b)
  let first (a, b) = a

  (* feel free to add more contents beyond ModuleType it conforms to *)
  let concat (a, b) = a @ b
end

(* The difference between t and 'a t is:
      type <=> Type
   'a type <=> Type<'a>
*)

(* Usage of Abstract Type t *)
module type StackType = sig
  (* what this type t mean:
     1. any module that conforms to this signature must specify the type t
     2. since the vals and functions are defined using type t, the functions
     of the module conforming to this will also operate on its own defined t
     as long as the functions' parameters' types are specified as t, rather
     than some polymorphic functions
  *)
  type t

  val empty : t list
  val push : t -> t list -> t list
  val pop : t list -> (t * t list) option
end

(* if I want a stack storing int *)
(* "with type t = int" is not mandatory if "type t = int" is specified inside *)
module IntStack : StackType with type t = int = struct
  type t = int

  let empty = []

  (* push's sig will be int -> int list -> int list when used, because type t = int is
     specified and push'sig was t -> t list -> t list in the module type *)
  let push x s = x :: s
  let pop = function [] -> None | x :: xs -> Some (x, xs)
end

module StringStack : StackType with type t = string = struct
  type t = string

  let empty = []
  let push x s = x :: s
  let pop = function [] -> None | x :: xs -> Some (x, xs)
end

(* though it works, it's tedious to use type t in this case, type 'a t would be better *)
module Stack = struct
  type 'a t = 'a list

  let empty = []
  let push x s = x :: s
  let pop = function [] -> None | x :: xs -> Some (x, xs)
end

(* type x = y is just a alias acrossing the module scope, it does not automatically mean that
   e.g function in the module also accepts a parameter of type x *)
module IntStack = struct
  type ilst = int list
  type slst = string list

  let empty = []
  let push x s = x :: s
  let pop = function [] -> None | x :: xs -> Some (x, xs)
end

(* push and pop remain polymorphic, because their parameter type were not defined in the
   module even though some type ilst and slst were defined *)
let istack = IntStack.push 2 [ 3 ]
let sstack = IntStack.push "s" [ "s" ]

module PolyStack = struct
  type 'a t = 'a list 
  (* "TODO: ask when its useful to actually define a type though its not really used" *)

  let empty = []
  let push x s = x :: s
  let pop = function [] -> None | x :: xs -> Some (x, xs)
end

