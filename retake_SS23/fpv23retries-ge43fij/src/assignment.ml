module type Trie = sig
  (* (<is_word>, <children_list>) *)
  type trie = Trie of (bool * (char * trie) list)

  val empty : trie
  val insert : trie -> char list -> trie
  val remove : trie -> char list -> trie
  val contains : trie -> char list -> bool
end

module type Trie_db = functor (Trie : Trie) -> sig
  type t

  val create : unit -> t
  val insert : t -> char list -> unit
  val remove : t -> char list -> unit
  val contains : t -> char list -> bool
end

module type Helpers = sig
  val string_to_char_list : string -> char list
  val char_list_to_string : char list -> string
end

(** Some helper functions you may use in your code or during testing.
    You can use them where you see fit or fully implement your own solutions. *)
module Helpers : Helpers = struct
  (** Converts a [string] to a [char list]. *)
  let string_to_char_list string =
    List.init (String.length string) (String.get string)

  (** Converts a [char list] to a [string]. *)
  let char_list_to_string list = String.concat "" (List.map Char.escaped list)
end

module Trie : Trie = struct
  (* (<is_word>, <children_list>) *)
  type trie = Trie of (bool * (char * trie) list)

  let empty = Trie (false, [])
  let contains trie word = failwith "TODO: contains"
  let insert trie word = failwith "TODO: insert"
  let remove trie word = failwith "TODO: remove"
end

module Trie_db : Trie_db =
functor
  (Trie : Trie)
  ->
  struct
    open Trie

    type command =
      | Insert of char list
      | Remove of char list
      | Contains of (char list * bool Event.channel)

    type t = command Event.channel

    let create () = failwith "TODO: create"
    let insert trie_server word = failwith "TODO: insert"
    let remove trie_server word = failwith "TODO: remove"
    let contains trie_server word = failwith "TODO: contains"
  end

(* The following tries each contain the words "hey", "hi" and "hi!".
   These should help you understand how the modules and types should be used. *)

(* Usage of the type *)
let example_trie_direct () =
  let open Trie in
  Trie
    ( false,
      [
        ( 'h',
          Trie
            ( false,
              [
                ('e', Trie (false, [ ('y', Trie (true, [])) ]));
                ('i', Trie (true, [ ('!', Trie (true, [])) ]));
              ] ) );
      ] )

(* Usage of the Trie module *)
let example_trie_insert () =
  let trie = Trie.empty in
  let trie = Trie.insert trie (Helpers.string_to_char_list "hey") in
  let trie = Trie.insert trie (Helpers.string_to_char_list "hi") in
  let trie = Trie.insert trie (Helpers.string_to_char_list "hi!") in
  trie

(* Usage of the Trie database module *)
let example_trie_insert_db () =
  let module Trie_db = Trie_db (Trie) in
  let trie = Trie_db.create () in
  Trie_db.insert trie (Helpers.string_to_char_list "hey");
  Trie_db.insert trie (Helpers.string_to_char_list "hi");
  Trie_db.insert trie (Helpers.string_to_char_list "hi!");
  trie
