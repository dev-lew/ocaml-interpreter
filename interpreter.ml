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
  | BeginEnd of command list
  | IfElseEnd of (command list * command list) (* true block, false block *)
  | DefFun of (const * const * command list) (* function name, arg, commands *)
  | Call
  | Throw
  | TryCatchEnd of (command list * command list) (* try block, catch block *)

(* Program type *)
type prog =
  command list

(* Type for the memory *)
type value =
  | I_val of int
  | B_val of bool
  | S_val of string
  | N_val of string
  | F_val of ((string * value) list * const * const * command list) (* mem, name, name, coms *)
  | U_val

(* Simple key value based memory for Let *)
type mem =
  (string * value) list

(* Converts values to string representations consistent with
   the output files *)
let string_of_value (inp : value) : string =
  match inp with
    I_val i -> string_of_int i
  |
    B_val b -> "<" ^ string_of_bool b ^ ">"
  |
    S_val s -> "\"" ^ s ^ "\""
  |
    N_val n -> n
  |
    F_val _ -> "<fun>"
  |
    U_val -> "<unit>"

(* Converts consts to values  *)
let value_of_const (inp: const) : value =
  match inp with
    Int i -> I_val i
  |
    Bool b -> B_val b
  |
    String s -> S_val s
  |
    Name n -> N_val n
  |
    Unit -> U_val

(* Retrieve a value by searching with key name *)
let fetch (m : mem) (name : string) : value option =
  List.assoc_opt name m

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

(* Returns the results of both p1 and p2 *)
let (+++) (p1 : 'a parser) (p2 : 'b parser) : ('a * 'b) parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (y, ls) -> Some ((x, y), ls)
     | None -> None)
  | None -> None

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

(* Useful for mutually recursive parsers,
   requires the parser to take in a unit *)
let rec many' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

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

let andp : command parser =
  sats "And" >>= fun _ ->
  satc ';' >>= fun _ ->
  return And

let orp : command parser =
  sats "Or" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Or

let notp : command parser =
  sats "Not" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Not

let eqp : command parser =
  sats "Eq" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Eq

let ltep : command parser =
  sats "Lte" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Lte

let ltp : command parser =
  sats "Lt" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Lt

let gtep : command parser =
  sats "Gte" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Gte

let gtp : command parser =
  sats "Gt" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Gt

let letp : command parser =
  sats "Let" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Let

let askp : command parser =
  sats "Ask" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Ask

let callp : command parser =
  sats "Call" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Call

let throwp : command parser =
  sats "Throw" >>= fun _ ->
  satc ';' >>= fun _ ->
  return Throw

(* Parses a no argument command *)
let noarg : command parser =
  popp <|> logp <|> swapp <|> addp <|> subp <|> mulp <|> divp <|> remp <|> negp <|>
  catp <|> andp <|> orp <|> notp <|> eqp <|> ltep <|> ltp <|> gtep <|> gtp <|> letp
  <|> askp <|> callp <|> throwp

(* Parses a command and ignores whitespace after
   the semicolon (helps with parsing lists of commands) *)
let rec commandp () : command parser =
  (pushp <|> noarg <|> beginendp () <|> ifelseendp () <|> deffunp () <|> trycatchendp ()) >>= fun x ->
  wsp >>= fun _ ->
  return x

and beginendp () : command parser =
  sats "Begin" >>= fun _ ->
  wsp >>= fun _ ->
  many' (fun () -> commandp ()) >>= fun coms ->
  sats "End;" >>= fun _ ->
  wsp >>= fun _ ->
  return (BeginEnd (coms))

and ifelseendp () : command parser =
  sats "If" >>= fun _ ->
  wsp >>= fun _ ->
  many' (fun () -> commandp ()) >>= fun truecoms ->
  sats "Else" >>= fun _ ->
  wsp >>= fun _ ->
  many' (fun () -> commandp ()) >>= fun falsecoms ->
  sats "End;" >>= fun _ ->
  wsp >>= fun _ ->
  return (IfElseEnd (truecoms, falsecoms))

and deffunp () : command parser =
  sats "DefFun" >>= fun _ ->
  wsp >>= fun _ ->
  namep >>= fun funcname ->
  wsp >>= fun _ ->
  namep >>= fun arg ->
  wsp >>= fun _ ->
  many' (fun () -> commandp ()) >>= fun coms ->
  sats "End;" >>= fun _ ->
  wsp >>= fun _ ->
  return (DefFun (funcname, arg, coms))

and trycatchendp () : command parser =
  sats "Try" >>= fun _ ->
  wsp >>= fun _ ->
  many' (fun () -> commandp ()) >>= fun trycoms ->
  sats "Catch" >>= fun _ ->
  wsp >>= fun _ ->
  many' (fun () -> commandp ()) >>= fun catchcoms ->
  sats "End;" >>= fun _ ->
  wsp >>= fun _ ->
  return (TryCatchEnd (trycoms, catchcoms))

(* Parses a list of commands, ignoring whitespace *)
let commandsp () : prog parser  =
  many1 (commandp ())


(* Evaluates a list of commands, using configuration
   (p/s) -> (p'/s') with p being a prog and s being a stack
   Throws defined error codes 0,1,2,3
   Returns an output list * error code * copy of stack for BeginEnd
   Only a Log command may change the output list
   m is a simple memory for the let and ask commands
   since begin end must push the top value of the inner stack to the outer one *)
let rec eval (prog : prog) (s : value list ) (acc: string list) (m : mem) : (string list * int * value list)  =
  match prog with
    (Push v)::prog' -> eval prog' (value_of_const v::s) acc m
  |
    Pop::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::s' -> eval prog' s' acc m
    end
  |
    Log::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::s' -> eval prog' s' ((string_of_value v)::acc) m
    end
  |
    Swap::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::[] -> acc, 2, s
      |
        v1::v2::s' -> eval prog' (v2::v1::s') acc m
    end
  |
    Add::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::[] -> acc, 2, s
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            (I_val x, I_val y) -> eval prog' (I_val (x + y)::s') acc m
          |
            _ -> acc, 1, s
        end
    end
  |
    Sub::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::[] -> acc, 2, s
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            (I_val x, I_val y) -> eval prog' (I_val (x - y)::s') acc m
          |
            _ -> acc, 1, s
        end
    end
  |
    Mul::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::[] -> acc, 2, s
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            (I_val x, I_val y) -> eval prog' (I_val (x * y)::s') acc m
          |
            _ -> acc, 1, s
        end
    end
  |
    Div::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::[] -> acc, 2, s
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            (I_val x, I_val y) -> begin
              match y with
                0 -> acc, 3, s
              |
                _ -> eval prog' (I_val (x / y)::s') acc m
              end
          |
            _ -> acc, 1, s
        end
    end
  |
    Rem::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::[] -> acc, 2, s
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            (I_val x, I_val y) -> begin
              match y with
                0 -> acc, 3, s
              |
                _ -> eval prog' (I_val (x mod y)::s') acc m
            end
          |
            _ -> acc, 1, s
        end
    end
  |
    Neg::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::s' -> begin
          match v with
            I_val x -> eval prog' (I_val (-1 * x)::s') acc m
          |
            _ -> acc, 1, s
          end
    end
  |
    Cat::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::[] -> acc, 2, s
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            S_val x, S_val y -> eval prog' (S_val (x ^ y)::s') acc m
          |
            _ -> acc, 1, s
          end
      end
  |
    And::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::[] -> acc, 2, s
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            B_val x, B_val y -> eval prog' (B_val (x && y)::s') acc m
          |
            _ -> acc, 1, s
          end
      end
  |
    Or::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::[] -> acc, 2, s
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            B_val x, B_val y -> eval prog' (B_val (x || y)::s') acc m
          |
            _ -> acc, 1, s
        end
    end
  |
    Not::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::s' -> begin
          match v with
            B_val x -> eval prog' (B_val (not x)::s') acc m
          |
            _ -> acc, 1, s
        end
    end
  |
    Eq::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::[] -> acc, 2, s
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            (I_val x, I_val y) -> eval prog' (B_val (x = y)::s') acc m
          |
            _ -> acc, 1, s
        end
    end
  |
    Lte::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::[] -> acc, 2, s
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            (I_val x, I_val y) -> eval prog' (B_val (x <= y)::s') acc m
          |
            _ -> acc, 1, s
        end
    end
  |
    Lt::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::[] -> acc, 2, s
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            (I_val x, I_val y) -> eval prog' (B_val (x < y)::s') acc m
          |
            _ -> acc, 1, s
        end
    end
  |
    Gte::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::[] -> acc, 2, s
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            (I_val x, I_val y) -> eval prog' (B_val (x >= y)::s') acc m
          |
            _ -> acc, 1, s
        end
    end
  |
    Gt::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::[] -> acc, 2, s
      |
        v1::v2::s' -> begin
          match (v1, v2) with
            (I_val x, I_val y) -> eval prog' (B_val (x > y)::s') acc m
          |
            _ -> acc, 1, s
        end
    end
  |
    Let::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::[] -> acc, 2, s
      |
        n::v::s' -> begin
          match (n, v) with
            (N_val n, I_val i) -> eval prog' s' acc ((n, I_val i)::m)
          |
            (N_val n, B_val b) -> eval prog' s' acc ((n, B_val b)::m)
          |
            (N_val n, S_val s) -> eval prog' s' acc ((n, S_val s)::m)
          |
            (N_val n, U_val) -> eval prog' s' acc ((n, U_val)::m)
          |
            (N_val n, N_val n') -> eval prog' s' acc ((n, N_val n')::m)
          |
            (N_val n, F_val f) -> eval prog' s' acc ((n, F_val f)::m)
          |
            _ -> acc, 1, s (* If n is not a Name *)
          end
      end
  |
    Ask::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        n::s' -> begin
          match n with
            N_val str -> begin
              match fetch m str with
                Some v -> eval prog' (v::s') acc m
              |
                None -> acc, 4, s (* Lookup failed: unbound variable *)
              end
          |
            _ -> acc, 1, s
          end
      end
  |
    (BeginEnd comls)::prog' ->

    (* BeginEnd must contain commands *)
    if comls = [] then acc, 2, s else begin
      match eval comls [] [] m with

      (* The stack may not be empty after evaluating comls, since the
         top value of it must be pushed to the outer stack *)
        (newacc, _, []) -> (newacc @ acc), 2, s
      |

      (* Successful return, push top value of inner stack to outer and concat output list *)
        (newacc, 0, v::_) -> eval prog' (v::s) (newacc @ acc) m
      |

      (* Failed return, keep track of the output list but exit with code err *)
        (newacc, err, s) -> (newacc @ acc), err, s
    end
  |
    IfElseEnd (truecoms, falsecoms)::prog' ->
      if truecoms = [] || falsecoms = [] then acc, 2, s else begin
      match s with
        [] -> acc, 2, s
      |
        v::s' -> begin
          match v with
            B_val true -> eval (truecoms @ prog') s' acc m
          |
            B_val false -> eval (falsecoms @ prog') s' acc m
          |
            _ -> acc, 1, s (* Not a boolean, exit*)
          end
    end
  |
    DefFun (fname, arg, coms)::prog' ->begin
      match fname with
        Name n -> eval prog' s acc ((n, F_val (m, fname, arg, coms))::m)
      |
        _ -> failwith "Not a name"
      end
  |
    (* Consume an argument value and the closure function
       represented by F_val, execute the commands in F_val, and push the top of the inner
       stack to the outer stack
       Binds the argument value to the name of the argument defined during DefFun
       along and bindings the function to its name again to allow recursion *)
    Call::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        _::[] -> acc, 2, s

      |
        arg_val::f::s' -> begin
          match f with
            F_val (env, Name fname, Name arg, coms) -> begin
              match eval coms [] [] (((arg, arg_val)::((fname, f)::env))) with
                (newacc, 0, v::_) -> eval prog' (v::s') (newacc @ acc) m
              |
                (newacc, err, s'') -> (newacc @ acc), err, s''
              end
          |
            _ -> acc, 4, s (* Wrong type *)
        end
    end
  |
    Throw::prog' -> begin
      match s with
        [] -> acc, 2, s
      |
        v::s' -> begin
          match v with
            I_val i -> acc, i, s (* Throw user-defined error code i *) 
          |
            _ -> acc, 1, s
          end
      end
  |
    TryCatchEnd (trycoms, catchcoms)::prog' -> begin
      match eval trycoms [] [] m with
        (newacc, 0, s') -> eval prog' (s' @ s) (newacc @ acc) m
      |
        (* An error was thrown, evaluate the catch block within the
           original env and stack with the error code at the top *)
        (newacc, err, s') -> begin
          match eval catchcoms (I_val err::s) [] m with

            (* Catch block is finished, continue rest of evaluation *)
            (newacc', 0, s'') -> eval prog' s'' (newacc' @ newacc @ acc) m
          |
            (newacc', err, s'') -> (newacc' @ newacc @ acc), err, s''
        end
      end
  |
    [] -> acc, 0, s

(* Debugging functions *)
let show_stack prog = let (revlog, err, scopy) = eval prog [] [] [] in (List.rev revlog, err, scopy)

let debug s =
  match parse (commandsp ()) s with
   Some (prog, []) -> show_stack prog
  |
    _ -> failwith "Invalid"

(* Wrapper for evaluate
   Takes a prog calls evaluate on it
   A successfully parsed program returns an empty list as the second member
   in the tuple *)
let interpreter (s : string) : string list * int =
  match parse (commandsp ()) s with
    Some (commands, []) -> begin
    match eval commands [] [] [] with
        ls, err, _ -> (List.rev ls), err
  end
  |
    _ -> failwith "Invalid string"

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
  interpreter s ;;

