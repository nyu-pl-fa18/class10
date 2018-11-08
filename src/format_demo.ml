type bop =
  | Plus
  | Mult

type term =
  | Const of int
  | BinOp of bop * term * term

let pr_bop ppf = function
  | Mult -> Format.fprintf ppf "%s" "*"
  | Plus -> Format.fprintf ppf "%s" "+"

let rec pr_term (ppf: Format.formatter) (t: term) : unit =
  match t with
  | Const i -> Format.fprintf ppf "%d" i
  | BinOp (bop, t1, t2) ->
    Format.fprintf ppf "@[<2>(%a@ %a@ %a)@]" pr_term t1 pr_bop bop pr_term t2

(** Convert the term [t] to its string representation *)
let string_of_term (t: term) : string =
  pr_term Format.str_formatter t;
  Format.flush_str_formatter ()

(** Pretty print the term [t] to output channel [out_ch]. *)
let print_term (out_ch: out_channel) (t: term) : unit =
  Format.fprintf
    (Format.formatter_of_out_channel out_ch) "%a@?" pr_term t


(** t represents the term 3 + 2 * 4 *)
let t = BinOp (Plus, Const 3, BinOp (Mult, Const 2, Const 4))
    
let _ = print_term stdout t
