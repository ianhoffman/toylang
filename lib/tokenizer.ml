type token =
  | Equals
  | BinaryOperator of char
  | Identifier of string
  | Integer of int
  | OpenParenthesis
  | CloseParenthesis

let to_str t =
  match t with
  | Equals -> "="
  | BinaryOperator i -> Printf.sprintf "BinaryOperator(%c)" i
  | Identifier i -> Printf.sprintf "Identifier(%s)" i
  | Integer i -> Printf.sprintf "Integer(%d)" i
  | OpenParenthesis -> Printf.sprintf "("
  | CloseParenthesis -> Printf.sprintf ")"

let int_from_char c = int_of_char c - int_of_char '0'

let rec scan_int (input : char list) =
  match input with
  | [] -> None
  | head :: rest -> (
      match head with
      | '0' .. '9' ->
          Some
            (match scan_int rest with
            | None -> (int_from_char head, rest, 10)
            | Some (i, r, mag) -> ((mag * int_from_char head) + i, r, mag * 10))
      | _ -> None)

let rec scan_id input =
  match input with
  | [] -> None
  | head :: rest -> (
      match head with
      | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> (
          match scan_id rest with
          | None -> Some (String.make 1 head, rest)
          | Some (s, r) -> Some (String.make 1 head ^ s, r))
      | _ -> None)

let rec scan (input : char list) =
  match input with
  | [] -> None
  | head :: rest -> (
      match head with
      | '+' | '-' | '*' -> Some (BinaryOperator head, rest)
      | '=' -> Some (Equals, rest)
      | '0' -> Some (Integer 0, rest)
      | '1' .. '9' -> (
          match scan_int input with
          | None -> None
          | Some (i, r, _) -> Some (Integer i, r))
      | 'A' .. 'Z' | 'a' .. 'z' | '_' -> (
          match scan_id input with
          | None -> None
          | Some (s, r) -> Some (Identifier s, r))
      | ' ' | '\n' | '\t' -> scan rest
      | _ -> None)

let scan_all (input : string) =
  let rec go tokens rest =
    match scan rest with Some (t, r) -> go (tokens @ [ t ]) r | None -> tokens
  in
  go [] (List.init (String.length input) (String.get input))
