exception Error of string

let err s = raise (Error s)

type distance = int
type allignment = string * string
type vertex = int * int

let rec get_min = function
  | top :: [] -> top
  | top :: rest -> if top >= get_min rest then get_min rest else top
  | _ -> 0

let flip f x y = f y x

let get_parent v =
  let rec sanitize l = 
    match l with
    | (x, y) as top :: rest ->
      if x - 1 >= 0 && y - 1 >= 0 then top :: (sanitize rest) else sanitize rest
    | [] -> [] in
  match v with
  | (x, y) -> sanitize [(x-1, y-1); (x-1,y); (x, y-1)]

let slash_cost = 2
let default_cost = 1
let reward_cost = -1

let seq0 = "gttgaataatgccaatcaaaaaaaaatatcca"
let seq1 = "gtacagtgtagaagctccaagaaataaaagattcgttctcataaagtttttgaaaagtta"
let null_char = "-"

let string_repeat s n =
  let len = Bytes.length s in
  let res = Bytes.create(n * len) in
  for i = 0 to pred n do
    Bytes.blit s 0 res (i * len) len
  done;
  Bytes.to_string res

let known = ref []
let rec search_known l v s0 s1= 
  (match l with 
   | (a, b) :: rest -> if a = v then b else search_known rest v s0 s1
   | [] -> (match v with
       | (n, 0) -> slash_cost * n
       | (0, n) -> slash_cost * n
       | (n, m) -> let (dist, _) =  alignment (String.sub s0 0 n) (String.sub s1 0 m) in dist)) 
and alignment s0 s1 = 
  (* distance: *)
  let rec distance (x, y) (v ,w) = 
    let (a, b) = (v - x, w - y) in 
    try 
      if (x, y) = (0, 0) then search_known !known (v, w) s0 s1 else err("not from point 0")
    with e -> 
    match (a, b) with
    | (n, 0) -> slash_cost * n
    | (0, n) -> slash_cost * n
    | (1, 1) -> 
      let c0 = seq0.[x] in
      let c1 = seq1.[y] in
      if c0 = c1 then reward_cost else default_cost
    | (n, m) -> 
      let current = (v, w) in
      let parents = get_parent current in
      let l_distances = List.map (distance (x, y)) parents in
      let s_distances =  List.map (flip distance current) parents in
      let distances = List.map2 (+) l_distances s_distances in
      let r = get_min distances in
      known := ((v, w), r) :: !known;
      r in
  ( distance (0, 0) (String.length s0, String.length s1), !known) 

let pp_matrix lst l1_n l2_n s0 s1 =
  for i = 0 to l1_n do 
    let line = ref "" in
    for j = 0 to l2_n do
      line := !line ^ "\t" ^ string_of_int (search_known lst (i,j) s0 s1);
    done;
    print_string (!line ^ "\n")
  done

let main =
  if Array.length Sys.argv = 3 then
    let in_ch1 = open_in Sys.argv.(1) in
    let in_ch2 = open_in Sys.argv.(2) in
    let seq0 = input_line in_ch1 in
    let seq1 = input_line in_ch2 in
    let (dist, matrix) = alignment seq0 seq1 in
    print_string ("seq0: " ^ seq0 ^ "\n"); 
    print_string ("seq1: " ^ seq1 ^ "\n"); 
    pp_matrix matrix (String.length seq0) (String.length seq1) seq0 seq1;
    print_string ("result: " ^ (string_of_int dist) ^ "\n");
  else 
    print_string "no input provided"

