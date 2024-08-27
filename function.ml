(* Functions in OCaml *)
(* A function has a value, which corresponds to a certain type, e.g int -> int
   which means it takes an it and returns an int *)

(* Examples: *)
(* f is a function takes an int and returns the incremented int *)
let f x = x + 1

(* But when we write f x, its type becomes int, not int -> int anymore,
   because it's applied to x, so we can annotate this way: *)
let f (x : int) = x + 1

(* or f x as a whole is of type int *)
let f x : int = x + 1

(* or both *)
let f (x : int) : int = x + 1

(* All the syntax above are equivalent to this using fun keyword *)
let f = fun x -> x + 1

(* Thus we can annotate the type of f with int -> int *)
let f : int -> int = fun x -> x + 1

(* Of course we can also annotate type on the right hand side *)
let f x : int = (x : int) + 1
let f : int -> int = fun x : int -> (x : int) + 1

(* Note that we cannot only annotate f when parameters are defined right after it.
   This does not compile *)
(* let (f: int -> int) x = x + 1 *)

(* Also note g is not equivalent to f *)
(* g x: int -> int means g is a function taking an argument x of any type, since not
   annotated, and another int argument y, and returns a funtion of type int -> int,
   or we can say g x as a whole is of type int -> int *)
let g x : int -> int = fun y -> y + 1
