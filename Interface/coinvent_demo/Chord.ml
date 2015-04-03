type chordel =
  RelNote of int | AbsNote of int | Root of int
type chord = 
   Nil | Cons of (chordel * chord) 

let rec findroot  music = 
  match music with
    | Nil -> None
    | Cons(Root x,_) -> Some x
    | Cons(RelNote _,t) -> findroot t
    | Cons(AbsNote _,t) -> findroot t

let rec calcnoteslist root music = 
   match music with
     | Nil -> []
     | Cons(Root x,t) -> calcnoteslist root t
     | Cons(RelNote 0,t) -> calcnoteslist root t
     | Cons(RelNote x,t) -> ((x+root) mod 12)::(calcnoteslist root t)
     | Cons(AbsNote x,t) -> if (x=root) then (calcnoteslist root t)
       else (x::(calcnoteslist root t))

let rec gettexstring l  = 
  let get_val x = 
  match x with
    | 0 -> "c "
    | 1 -> "des "
    | 2 -> "d "
    | 3 -> "ees "
    | 4 -> "e "
    | 5 -> "f "
    | 6 -> "fis "
    | 7 -> "g "
    | 8 -> "aes "
    | 9 -> "a "
    | 10 -> "bes "
    | 11 -> "b " 
    | _ -> "" 
  in
  match l with 
    | [] -> ">2 "
    | h::t -> (get_val h)^(gettexstring t)

let calcnotesfromlist pre prebass post postbass root list = 
  let rec rem_dupls l = 
   match l with
     | [] -> []
     | h::t -> if (List.mem h t) then
	 (rem_dupls t) else h::(rem_dupls t)
    in 
  let getbass r = 
    match r with 
      | 0 -> "c"
      | 1-> "des"
      | 2 -> "d"
      | 3 -> "ees"
      | 4 -> "e"
      | 5 -> "f"
      | 6 -> "fis"
      | 7 -> "g"
      | 8 -> "aes"
      | 9 -> "a"
      | 10 -> "bes"
      | 11 -> "b"
      | _ -> ""
  in
  let gt a b = 
    if (a=b) then 0 else 
      (if (a>b) then 1 else -1)
  in
  let sortlist = List.sort gt (rem_dupls list) in
  let stave2 = ("<"^(gettexstring sortlist)) in
  ((pre^" "^stave2^" "^post),(prebass^" "^(getbass root)^"2 "^postbass))
  
