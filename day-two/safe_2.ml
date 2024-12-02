let read_lines () =
  let rec read_all acc =
    try
      let line = read_line () in
      read_all (line :: acc)
    with End_of_file -> List.rev acc
  in
  read_all []

let parse_line line =
  String.split_on_char ' ' line
  |> List.filter ((<>) "")
  |> List.map int_of_string

let is_adjacent_valid a b =
  let diff = abs (b - a) in
  diff >= 1 && diff <= 3

let is_sequence_valid nums =
  let rec check_sequence increasing = function
    | [] | [_] -> true
    | a :: b :: rest ->
        if b > a && not increasing then false
        else if b < a && increasing then false
        else if not (is_adjacent_valid a b) then false
        else check_sequence increasing (b :: rest)
  in
  let increasing = match nums with
    | [] | [_] -> true
    | a :: b :: _ -> b > a
  in
  check_sequence increasing nums

let try_remove_one nums =
  let rec aux i =
    if i >= List.length nums then false
    else
      let without_i =
        List.mapi (fun j x -> (j, x)) nums
        |> List.filter (fun (j, _) -> j <> i)
        |> List.map snd
      in
      if is_sequence_valid without_i then true
      else aux (i + 1)
  in
  aux 0

let is_safe_with_dampener nums =
  is_sequence_valid nums || try_remove_one nums

let count_safe_reports lines =
  List.filter is_safe_with_dampener (List.map parse_line lines)
  |> List.length

let () =
  let lines = read_lines () in
  let result = count_safe_reports lines in
  Printf.printf "%d\n" result
