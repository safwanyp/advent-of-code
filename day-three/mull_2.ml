let parse_instructions str =
  let mul_regex = Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" in
  let do_regex = Str.regexp "do()" in
  let dont_regex = Str.regexp "don't()" in

  let rec find_all pos acc enabled =
    try
      let do_pos = try Str.search_forward do_regex str pos with Not_found -> max_int in
      let dont_pos = try Str.search_forward dont_regex str pos with Not_found -> max_int in
      let mul_pos = try Str.search_forward mul_regex str pos with Not_found -> max_int in

      let min_pos = min (min do_pos dont_pos) mul_pos in
      if min_pos = max_int then
        List.rev acc
      else if min_pos = do_pos then
        find_all (do_pos + 4) acc true
      else if min_pos = dont_pos then
        find_all (dont_pos + 6) acc false
      else begin
        let x = int_of_string (Str.matched_group 1 str) in
        let y = int_of_string (Str.matched_group 2 str) in
        if enabled then
          find_all (mul_pos + 1) ((x * y) :: acc) enabled
        else
          find_all (mul_pos + 1) acc enabled
      end
    with Not_found -> List.rev acc
  in
  find_all 0 [] true

let process_lines lines =
  List.fold_left (fun acc line ->
    acc + (List.fold_left (+) 0 (parse_instructions line))
  ) 0 lines

let () =
  let input_lines = ["xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)do()?mul(8,5))"] in
  let result = process_lines input_lines in
  Printf.printf "%d\n" result
