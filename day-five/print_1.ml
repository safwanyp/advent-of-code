(* Create a Set module for integers *)
module IntSet = Set.Make(Int)

(* List utility functions *)
let rec take n lst =
  if n <= 0 then []
  else match lst with
    | [] -> []
    | x::xs -> x :: take (n-1) xs

let rec drop n lst =
  if n <= 0 then lst
  else match lst with
    | [] -> []
    | _::xs -> drop (n-1) xs

let find_index pred lst =
  let rec aux i = function
    | [] -> None
    | x::xs -> if pred x then Some i else aux (i+1) xs
  in
  aux 0 lst

(* Parse a line containing a dependency rule (x|y) *)
let parse_rule line =
  match String.split_on_char '|' line with
  | [before; after] -> (int_of_string before, int_of_string after)
  | _ -> failwith "Invalid rule format"

(* Parse a line containing an update (comma-separated numbers) *)
let parse_update line =
  String.split_on_char ',' line
  |> List.map int_of_string

(* Check if a list respects all ordering rules that apply to its elements *)
let is_valid_order rules pages =
  (* Create a set of pages in this update for quick lookup *)
  let page_set = List.fold_left (fun set p -> IntSet.add p set) IntSet.empty pages in

  (* Function to get index of a page in the list *)
  let find_index x = List.find_index ((=) x) pages in

  (* Check each rule *)
  List.for_all (fun (before, after) ->
    (* Rule only applies if both pages are in the update *)
    if IntSet.mem before page_set && IntSet.mem after page_set then
      match find_index before, find_index after with
      | Some i, Some j -> i < j  (* before must come before after *)
      | _ -> false
    else
      true  (* Rule doesn't apply if either page is missing *)
  ) rules

(* Get middle element of a list *)
let get_middle lst =
  let len = List.length lst in
  List.nth lst (len / 2)

let solve input =
  (* Split input into rules and updates sections *)
  let sections = String.split_on_char '\n' input |> List.filter (fun s -> s <> "") in
  let rules_end = find_index (fun line -> not (String.contains line '|')) sections in

  match rules_end with
  | None -> failwith "No updates section found"
  | Some idx ->
      let rules = take idx sections |> List.map parse_rule in
      let updates = drop idx sections |> List.map parse_update in

      (* Find valid updates and get their middle numbers *)
      updates
      |> List.filter (is_valid_order rules)
      |> List.map get_middle
      |> List.fold_left (+) 0

let example = "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47"

let () =
  let result = solve example in
  Printf.printf "%d\n" result
