let parse_mul str =
  let regex = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" in
  let rec find_all pos acc =
    try
      let _ = Str.search_forward regex str pos in
      let x = int_of_string (Str.matched_group 1 str) in
      let y = int_of_string (Str.matched_group 2 str) in
      find_all (Str.match_end()) ((x, y) :: acc)
    with Not_found -> List.rev acc
  in
  find_all 0 []

let calculate_sum line =
  let multiplications = parse_mul line in
  List.fold_left (fun acc (x, y) -> acc + (x * y)) 0 multiplications

let process_lines lines =
  List.fold_left (fun acc line -> acc + calculate_sum line) 0 lines

let () =
  let input_lines = ["xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"] in
  let result = process_lines input_lines in
  Printf.printf "%d\n" result
