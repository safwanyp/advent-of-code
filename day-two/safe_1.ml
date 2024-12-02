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

let count_safe_reports lines =
  List.filter is_sequence_valid (List.map parse_line lines)
  |> List.length

let () =
  let lines = read_lines () in
  let result = count_safe_reports lines in
  Printf.printf "%d\n" result
