let read_lines () =
    let rec read_all acc =
      try
        let line = read_line () in
        read_all (line :: acc)
      with End_of_file -> List.rev acc
    in
    read_all []

let parse_line line =
  match String.split_on_char ' ' line |> List.filter ((<>) "") with
  | [left; right] -> (int_of_string left, int_of_string right)
  | _ -> failwith "Invalid input line format"

let count_occurrences num lst =
  List.fold_left (fun acc x -> if x = num then acc + 1 else acc) 0 lst

let calculate_similarity_score lines =
  let left_nums, right_nums =
    List.map parse_line lines
    |> List.split
  in

  List.fold_left (fun acc num ->
    let count = count_occurrences num right_nums in
    acc + (num * count)
  ) 0 left_nums

let () =
  let lines = read_lines () in
  let similarity = calculate_similarity_score lines in
  Printf.printf "Similarity score: %d\n" similarity
