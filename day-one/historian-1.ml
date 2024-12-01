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

let calculate_total_distance lines =
  let left_nums, right_nums =
    List.map parse_line lines
    |> List.split
  in

  let sorted_left = List.sort compare left_nums in
  let sorted_right = List.sort compare right_nums in

  let distances =
    List.map2 (fun x y -> abs (x - y)) sorted_left sorted_right
  in

  List.fold_left (+) 0 distances

let () =
  let lines = read_lines () in
  let total = calculate_total_distance lines in
  Printf.printf "Total distance: %d\n" total
