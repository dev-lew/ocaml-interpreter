
open Printf

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

type 'a parser = char list -> ('a * char list) option

let (<|>) (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
    fun ls ->
      match p1 ls with
        None -> p2 ls
      |
        Some (x, rest) -> Some (x, rest)

let (>>=) : 'a parser -> ('a -> 'b parser) -> 'b parser =
  fun pa ->
  fun f ->
  fun ls -> match pa ls with
      None -> None
    |
      Some (a, rest) -> f a rest


let interpreter (s : string) : string list * int = failwith "undefined"


let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

let runfile (file : string) : string list * int =
  let s = readlines file in
  interpreter s
