
open Printf

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

type 'a parser = char list -> ('a * char list) option

(* Grammar definition
   The grammar also supports the primitive types of
   int, bool, and string *)
type const =
  | Int of int
  | Bool of bool
  | String of string
  | Name of string
  | Unit

type command =
  | Push of const
  | Pop
  | Log
  | Swap
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Neg

(* Disjunction operator: attempts to parse with p1, and
   if it fails, parse using p2 *)
let (<|>) (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
    fun ls ->
      match p1 ls with
        None -> p2 ls
      |
        Some (x, rest) -> Some (x, rest)

(* Bind operator: combine the results of pa and pb with
   the function f *)
let (>>=) : 'a parser -> ('a -> 'b parser) -> 'b parser =
  fun pa ->
  fun f ->
  fun ls -> match pa ls with
      None -> None
    |
      Some (a, rest) -> f a rest

(* Does not consume the input string
   Returns c *)
let return (a: 'a) : 'a parser =
  fun ls -> Some (a, ls)

(* Always fails *)
let zero : 'a parser =
  fun _ -> None

(* Run a parser *)
let parse (p: 'a parser) (str: string) : ('a * char list) option =
  p (explode str)

(* Applies p zero or more times until it fails
   Returns a list of each result of p on ls *)
let rec many (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
    None -> Some ([], ls)
  |
    Some (x, ls) -> begin
    match many p ls with
      None -> Some (x::[], ls)
    |
      Some (xs, ls) -> Some (x::xs, ls)
  end

(* Applies p 1 or more times until it fails
   Returns a list of each result of on ls *)
let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
    Some (x, ls) -> begin
      match many p ls with
        Some (xs, ls) -> Some (x::xs, ls)
      |
        None -> Some ([x], ls)
    end
  |
    None -> None

(* Parses the first character of the input *)
let charp : char parser =
  fun ls ->
  match ls with
  | x::ls -> Some (x, ls)
  | _ -> None

(* Takes char predicate f and if f is true, parse the character *)
let sat (f: char -> bool) : char parser =
  charp >>= fun x ->
  if f x then return x else zero

(* Takes a char, and parses that char, fail otherwise if it's not in
   the input *)
let satc (c: char) : char parser =
  sat (fun x -> if x = c then true else false)

(* Takes a char, and parses anything but that char *)
let nsatc (c: char) : char parser =
  sat (fun x -> if x = c then false else true)

(* Takes a char list and parses that char list, fail otherwise if it's not in
   the input *)
let clistp : char list -> char list parser =
  fun inp ->
  fun ls ->
  let rec aux inp ls =
    match inp, ls with
      [], _ -> Some ([], ls)
    |
      c::inp_rest, c'::ls_rest ->
      if c = c' then
        match aux inp_rest ls_rest with
          Some (_, rest) -> Some (inp, rest)
        |
          None -> None
      else None
    |
     _ -> None
  in aux inp ls

(* Takes a string and parses that string, fail otherwise if it's not in
   the input *)
let sats (str : string) : string parser =
  clistp (explode str) >>= fun x -> return (implode x)

(* Parses a string and stops after parsing a quote
   Fails if there is no quote
   Helper function for stringp *)
let failquote : const parser =
  many (nsatc '\"') >>= fun x ->
  satc '\"' >>= fun _ ->
  return (String ((implode x)))


(* Parses a string as defined in the grammar *)
let stringp : const parser =
  satc '\"' >>= fun _ -> failquote

(* Parses whitespace character *)
let wsc : char parser =
  satc ' ' <|> satc '\n' <|> satc '\t' <|> satc '\r'

(* Parses arbitrary amounts of whitespace *)
let wsp = many1 wsc

(* Parses a digit represented as a character *)
let digitp : char parser =
  charp >>= fun x ->
  if '0' <= x && x <= '9'
  then return x
  else zero

(* Parses a letter, defined in the grammar as a-z | A-Z *)
let letterp : char parser =
  charp >>= fun x ->
  if ('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z')
  then return x
  else zero

(* Parses a bool *)
let boolp : const parser =
  (sats "true" >>= fun _ ->
  return (Bool true))
  <|>
  (sats "false" >>= fun _ ->
  return ( Bool false))

(* Parses natural numbers *)
let nat : const parser =
  many1 digitp >>= fun x ->
  return (Int ((int_of_string (implode x))))

(* Parses negative numbers *)
let neg : const parser =
  satc '-' >>= fun _ ->
  many1 digitp >>= fun x ->
  return (Int ((-1 * (int_of_string (implode x)))))

(* Parses an integer *)
let intp : const parser =
  nat <|> neg

(* Parses a unit *)
let unitp : const parser =
  satc '(' >>= fun _ ->
  satc ')' >>= fun _ ->
  return Unit

(* Parses a name as defined in the grammar
   Merlin gives warnings here but the function works ? *)
let namep : const parser =
  letterp >>= fun x ->
  many (letterp <|> digitp <|> satc '_' <|> satc '\'') >>= fun rest ->
  return (Name (implode (x::rest)))

(* Parses a const as defined in the grammar *)
let constp : const parser =
  (intp <|> boolp <|> stringp <|> namep <|> unitp)

(* Parses a Push command, ignores whitespace *)
let pushp : command parser =
  sats "Push" >>= fun _ ->
  wsp >>= fun _ ->
  intp >>= fun x ->
  satc ';' >>= fun _ ->
  return (Push x)

(* Parses a Pop command *)
let popp : command parser =
  sats "Pop" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Pop

(* Parses a Log command *)
let logp : command parser =
  sats "Log" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Log

(* Parses a Swap command *)
let swapp : command parser =
  sats "Swap" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Swap

(* Parses an Add command *)
let addp : command parser =
  sats "Add" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Add

(* Parses a Sub  command *)
let subp : command parser =
  sats "Sub" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Sub

(* Parses a Mul command *)
let mulp : command parser =
  sats "Mul" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Mul

(* Parses a Div command *)
let divp : command parser =
  sats "Div" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Div

(* Parses a Rem command *)
let remp : command parser =
  sats "Rem" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Rem

(* Parses a Neg command *)
let negp : command parser =
  sats "Neg" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Neg

(* Parses a no argument command *)
let noarg : command parser =
  popp <|> logp <|> swapp <|> addp <|> subp <|> mulp <|> divp <|> remp <|> negp

let commandp : command parser =


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
