type formula =
  | Var of string
  | Bot
  | Implication of formula * formula

let rec equals f1 f2 =
  match (f1, f2) with
    | (Var x, Var y) -> x = y
    | (Bot, Bot) -> true
    | (Implication(f1, f2), Implication(f3, f4)) -> equals f1 f3 && equals f2 f4
    | (_, _) -> false

let rec contains_formula f formulas =
  match formulas with
    | [] -> false
    | x :: xs -> if equals x f then true else contains_formula f xs;;

let rec remove_formula f formulas = 
  match formulas with
    | [] -> []
    | x :: xs -> if equals x f then xs else x :: remove_formula f xs;; 

let add_formula f formulas = 
  if not (contains_formula f formulas) then f :: formulas else formulas

let rec append_formulas formulas1 formulas2 = 
  match formulas2 with
    | [] -> formulas1
    | f :: fs -> append_formulas (add_formula f formulas1) fs

let rec string_of_formula f =
  match f with
    | Bot -> "⊥"
    | Var var -> var
    | Implication(f1, f2) 
      -> match (f1, f2) with
        | (Implication(_, _), (Var _|Bot)) 
          -> "(" ^ string_of_formula f1 ^ ") 🠒 " ^ string_of_formula f2
        | ((Var _|Bot), Implication(_, _)) | ((Var _|Bot), (Var _|Bot)) 
          -> string_of_formula f1 ^ " 🠒 " ^ string_of_formula f2
        | (Implication(_, _), Implication(_, _)) 
          -> "(" ^ string_of_formula f1 ^ ") 🠒 " ^ string_of_formula f2

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

type theorem = 
  | Assumption of (formula list * formula)
  | ImplIntro of (formula list * formula) * theorem
  | FalseElimination of (formula list * formula) * theorem
  | ImplElimination of (formula list * formula) * theorem * theorem

let assumptions thm =
  match thm with
    | Assumption(pair) -> fst pair
    | ImplIntro(pair, _) | FalseElimination(pair, _) -> fst pair
    | ImplElimination(pair, _, _) -> fst pair

let consequence thm =
  match thm with
    | Assumption(pair) -> snd pair
    | ImplIntro(pair, _) | FalseElimination(pair, _) -> snd pair
    | ImplElimination(pair, _, _) -> snd pair

let pp_print_theorem fmtr thm =
  let open Format in
  pp_open_hvbox fmtr 2;
  begin match assumptions thm with
  | [] -> ()
  | f :: fs ->
    pp_print_formula fmtr f;
    fs |> List.iter (fun f ->
      pp_print_string fmtr ",";
      pp_print_space fmtr ();
      pp_print_formula fmtr f);
    pp_print_space fmtr ()
  end;
  pp_open_hbox fmtr ();
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_formula fmtr (consequence thm);
  pp_close_box fmtr ();
  pp_close_box fmtr ()

let by_assumption f = Assumption(([f], f))

let imp_i f thm = 
  ImplIntro(
    (remove_formula f (assumptions thm), Implication(f, consequence thm)),
    Assumption((assumptions thm, consequence thm)));;
  

let imp_e th1 th2 =
  match consequence th1 with
    | Var _ -> failwith "error"
    | Bot -> failwith "error"
    | Implication(f1, f2) -> 
      if (equals f1 (consequence th2)) then
        ImplElimination(
          (append_formulas (assumptions th1) (assumptions th2), f2),
          th1,
          th2
        )
      else failwith "error";;

let bot_e f thm =
  match consequence thm with
    | Bot -> 
        FalseElimination(
          ((assumptions thm), f),
          Assumption((assumptions thm), Bot)
        )
    | formula -> failwith "error";;


(* ---------------------------------------------------------- *)

(* let p = Var "p";;
let q = Var "q";;
let r = Var "r";;

(* p → p *)
let proof1 = imp_i p (by_assumption p);;

(* p → q → p *)
let proof2 = imp_i p (imp_i q (by_assumption p));; 

(* (p → q → r) → (p → q) → p → r *)
let pqr_f = Implication(p, Implication(q, r));;
let pq_f = Implication(p, q);;

let pqr_thm = imp_e (by_assumption pqr_f) (by_assumption p);;

let pq_thm = imp_e (by_assumption pq_f) (by_assumption p);;

let pq_pr_thm = imp_i pq_f (imp_i p (imp_e pqr_thm pq_thm));;

let proof3 = imp_i pqr_f pq_pr_thm;;

(* ⊥ → p *)

let proof4 = imp_i Bot (bot_e p (by_assumption Bot));; *)