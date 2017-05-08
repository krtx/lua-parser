open Lexer
open Lexing
open Syntax

let print_position fmt lexbuf =
  let pos = lexbuf.lex_curr_p in
  Format.fprintf fmt "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.chunk Lexer.read lexbuf with
  | Parser.Error -> begin
    print_endline "error";
    Format.(fprintf std_formatter "%a: syntax error\n"
              print_position lexbuf);
    exit (-1)
  end

let sum lst = List.fold_left (+) 0 lst

let (>>=) x f =
  match x with
  | Some x -> f x
  | None   -> 0

let rec count_fdef_stat = function
  | Assign (_, exps)           -> sum (List.map count_fdef_exp exps)
  | FunctionCall fc            -> count_fdef_fc fc
  | (Label _ | Break | Goto _) -> 0
  | DoEnd block                -> count_fdef_block block
  | WhileDoEnd (exp, block)    -> count_fdef_exp exp + count_fdef_block block
  | RepeatUntil (block, exp)   -> count_fdef_block block + count_fdef_exp exp
  | If (branches, else_branch) ->
     (List.map
        (fun (exp, block) -> count_fdef_exp exp + count_fdef_block block)
        branches
      |> sum)
     + (else_branch >>= count_fdef_block)
  | ForStep (_, exp1, exp2, exp3, block) ->
     [ count_fdef_exp exp1
     ; count_fdef_exp exp2
     ; exp3 >>= count_fdef_exp
     ; count_fdef_block block
     ]
     |> sum
  | ForIn (_, exps, block) ->
     sum (List.map count_fdef_exp exps) + count_fdef_block block
  | LocalAssign (_, exps) ->
     begin
       match exps with
       | Some exps -> sum (List.map count_fdef_exp exps)
       | None      -> 0
     end

and count_fdef_fc = function
  | Function (pe, args) ->
     count_fdef_prefixexp pe +
       (sum (List.map count_fdef_exp args))
  | Method (pe, _, args) ->
     count_fdef_prefixexp pe +
       (sum (List.map count_fdef_exp args))

and count_fdef_exp = function
  | (Nil | False | True | Integer _ | Float _ |
      LiteralString _ | Vararg) -> 0
  | FunctionDef (_, _, block)   -> 1 + count_fdef_block block
  | PrefixExp pe                -> count_fdef_prefixexp pe
  | Table table ->
     List.map
       (fun (_, exp) -> count_fdef_exp exp) (* ignore key *)
       table
     |> sum
  | BinOp (_, exp1, exp2) -> count_fdef_exp exp1 + count_fdef_exp exp2
  | UnOp (_, exp) -> count_fdef_exp exp

and count_fdef_block (stats, retstat) =
  sum (List.map count_fdef_stat stats)
  +
    (match retstat with
    | Some exps -> sum (List.map count_fdef_exp exps)
    | None      -> 0)

and count_fdef_prefixexp = function
  | Var var            -> count_fdef_var var
  | FunctionCallExp fc -> count_fdef_fc fc
  | Exp exp            -> count_fdef_exp exp

and count_fdef_var = function
  | Name _               -> 0
  | IndexTable (pe, exp) -> count_fdef_prefixexp pe + count_fdef_exp exp

let () =
  let filename = Sys.argv.(1) in
  let chunk = parse_with_error (Lexing.from_channel (open_in filename)) in
  print_endline ("functiondef: " ^ (string_of_int (count_fdef_block chunk)))
