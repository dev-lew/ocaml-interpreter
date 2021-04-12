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
  | Cat
  | And
  | Or
  | Not
  | Eq
  | Lte
  | Lt
  | Gte
  | Gt
  | Let
  | Ask
  (* The following commands will be enforced by parsers only:
     Begin commands End
     If commands Else commands End*)

type prog =
  command list

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
let wsp = many wsc

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
  (sats "<true>" >>= fun _ ->
  return (Bool true))
  <|>
  (sats "<false>" >>= fun _ ->
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
  sats "<unit>" >>= fun _ ->
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
  constp >>= fun x ->
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

let catp : command parser =
  sats "Cat" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Cat

let addp : command parser =
  sats "And" >>= fun _ ->
  satc ';' >>= fun _ ->
  return And

(* Parses a no argument command *)
let noarg : command parser =
  popp <|> logp <|> swapp <|> addp <|> subp <|> mulp <|> divp <|> remp <|> negp <|>
  catp

(* Parses a command and ignores whitespace after
   the semicolon (helps with parsing lists of commands) *)
let commandp : command parser =
  (pushp <|> noarg) >>= fun x ->
  wsp >>= fun _ ->
  return x

(* Parses a list of commands, ignoring whitespace *)
let commandsp : prog parser =
  many1 commandp

(* Converts const to string representations consistent with
   the output files *)
let string_of_const (inp : const) : string =
  match inp with
    Int i -> string_of_int i
  |
    Bool b -> "<" ^ string_of_bool b ^ ">"
  |
    String s -> "\"" ^ s ^ "\""
  |
    Name n -> n
  |
    Unit -> "<unit>"



(* Evaluates a list of commands, using configuration
   (p/s) -> (p'/s') with p being a prog and s being a stack
   Throws defined error codes 0,1,2,3
   Returns an output list * error code
   Only a Log command may change the output list *)
let rec eval (prog : prog) (s : const list ) (acc: string list) : (string list * int) =
  match prog with
    (Push v)::prog' -> eval prog' (v::s) acc
  |
    Pop::prog' -> begin
      match s with
        [] -> acc, 2
      |
        v::s' -> eval prog' s' acc
    end
  |
    Log::prog' -> begin
      match s with
        [] -> acc, 2
      |
        v::s' -> eval prog' s' ((string_of_const v)::acc)
    end
  |
    Swap::prog' -> begin
      match s with
        [] -> acc, 2
      |
        v::[] -> acc, 2
      |
        v1::v2::s' -> eval prog' (v2::v1::s') acc
    end
  |
    Add::prog' -> begin
      match s with
        [] -> acc, 2
      |
        v::[] -> acc, 2
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            (Int x, Int y) -> eval prog' (Int (x + y)::s') acc
          |
            _ -> acc, 1
        end
    end
  |
    Sub::prog' -> begin
      match s with
        [] -> acc, 2
      |
        v::[] -> acc, 2
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            (Int x, Int y) -> eval prog' (Int (x - y)::s') acc
          |
            _ -> acc, 1
        end
    end
  |
    Mul::prog' -> begin
      match s with
        [] -> acc, 2
      |
        v::[] -> acc, 2
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            (Int x, Int y) -> eval prog' (Int (x * y)::s') acc
          |
            _ -> acc, 1
        end
    end
  |
    Div::prog' -> begin
      match s with
        [] -> acc, 2
      |
        v::[] -> acc, 2
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            (Int x, Int y) -> begin
              match y with
                0 -> acc, 3
              |
                _ -> eval prog' (Int (x / y)::s') acc
              end
          |
            _ -> acc, 1
        end
    end
  |
    Rem::prog' -> begin
      match s with
        [] -> acc, 2
      |
        v::[] -> acc, 2
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            (Int x, Int y) -> begin
              match y with
                0 -> acc, 3
              |
                _ -> eval prog' (Int (x mod y)::s') acc
            end
          |
            _ -> acc, 1
        end
    end
  |
    Neg::prog' -> begin
      match s with
        [] -> acc, 2
      |
        v::s' -> begin
          match v with
            Int x -> eval prog' (Int (-1 * x)::s') acc
          |
            _ -> acc, 1
          end
    end
  |
    Cat::prog' -> begin
      match s with
        [] -> acc, 2
      |
        v::[] -> acc, 2
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            String x, String y -> eval prog' (String (y ^ x)::s') acc
          |
            _ -> acc, 1
          end
      end
  |
    And::prog' -> begin
      match s with
        [] -> acc, 2
      |
        v::[] -> acc, 2
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            Bool x, Bool y -> eval prog' (Bool (x && y)::s') acc
          |
            _ -> acc, 1
          end
      end
  |
    [] -> acc, 0

(* Wrapper for evaluate
   Takes a prog calls evaluate on it
   A successfully parsed program returns an empty list as the second member
   in the tuple *)
let interpreter (s : string) : string list * int =
  match parse commandsp s with
    Some (commands, []) -> begin
    match eval commands [] [] with
        ls, err -> (List.rev ls), err
  end
  |
    _ -> failwith "unimplemented"

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
