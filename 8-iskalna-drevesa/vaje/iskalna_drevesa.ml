(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree

(*----------------------------------------------------------------------------*]
Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
[leaf], ki iz podatka zgradi list.
        5
       / \
      2   7
     /   / \
    0   6   11
[*----------------------------------------------------------------------------*)

let leaf x = Node(Empty, x, Empty) 

let test_tree = 
  Node(Node(leaf 0, 2, Empty), 5, Node(leaf 6, 7, leaf 11))
(*----------------------------------------------------------------------------*]
Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
        5
       / \
      7   2
     / \   \
    11  6   0
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# mirror test_tree ;;
- : int tree =
Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)

let rec mirror = function
  | Empty -> Empty
  | Node (l, a, r) -> Node (mirror r, a, mirror l) 

(*----------------------------------------------------------------------------*]
Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
vseh vozlišč drevesa.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# height test_tree;;
- : int = 3
# size test_tree;;
- : int = 6
[*----------------------------------------------------------------------------*)

let rec height = function
| Empty -> 0
| Node (l, a, r) -> 1 + max (height l) (height r)

let rec size = function
| Empty -> 0
| Node (l, a, r) -> 1 + (size l) + (size r)

(*----------------------------------------------------------------------------*]
Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
drevesa [tree] preslikane s funkcijo [f].
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# map_tree ((<)3) test_tree;;
- : bool tree =
Node (Node (Node (Empty, false, Empty), false, Empty), true,
Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f = function
| Empty -> Empty
| Node (l, a, r) -> Node(map_tree f l, f a, map_tree f r)

(*----------------------------------------------------------------------------*]
Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# list_of_tree test_tree;;
- : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
| Empty -> []
| Node (l, a, r) -> (list_of_tree l) @ (a :: list_of_tree r)

(*----------------------------------------------------------------------------*]
Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# is_bst test_tree;;
- : bool = true
# test_tree |> mirror |> is_bst;;
- : bool = false
[*----------------------------------------------------------------------------*)

let rec narascajoc xs = match xs with
| [] -> true
| x :: [] -> true
| x :: y :: xs -> (x <= y) && (narascajoc (y :: xs))

let rec is_bst drevo = narascajoc (list_of_tree drevo)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
[member] preveri ali je dani element v iskalnem drevesu.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# insert 2 (leaf 4);;
- : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
# member 3 test_tree;;
- : bool = false
[*----------------------------------------------------------------------------*)

let rec insert n = function
  | Empty -> Node(Empty, n, Empty)
  | Node(l, x, r) -> 
      if n < x 
        then Node(insert n l, x, r)
      else if n > x 
        then Node(l, x, insert n r)
      else Node(l, x, r)

let rec member n = function
  | Empty -> false
  | Node(l, x, r) -> 
      if n = x 
        then true 
      else if n < x 
        then member n l 
      else member n r

(*----------------------------------------------------------------------------*]
Funkcija [member2] ne privzame, da je drevo bst.

Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

let rec member2 n = function
  | Empty -> false
  | Node(l, x, r) -> (member2 n l) || (x = n) || (member2 n r)

(*----------------------------------------------------------------------------*]
Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
od korena [x].
Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
korena, če obstaja.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# succ test_tree;;
- : int option = Some 6
# pred (Node(Empty, 5, leaf 7));;
- : int option = None
[*----------------------------------------------------------------------------*)

let rec min_element bst = 
  match bst with
    | Empty -> None
    | Node(l, x, r) -> 
      if l = Empty then Some x else min_element l

let rec max_element bst = 
  match bst with
    | Empty -> None
    | Node(l, x, r) -> 
      if l = Empty then Some x else max_element r

let succ bst = 
  match bst with
    | Empty -> None
    | Node(l, x, r) -> min_element r

let pred bst = 
  match bst with
    | Empty -> None
    | Node(l, x, r) -> max_element l

(*----------------------------------------------------------------------------*]
Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
oba načina brisanja elementov.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# (*<< Za [delete] definiran s funkcijo [succ]. >>*)
# delete 7 test_tree;;
- : int tree =
Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)

let rec delete x = function 
| Empty -> Empty
| Node(l, y, r) as bst ->
    if (x < y) 
      then Node(delete x l, y, r)
    else if (x > y)
      then Node(l, y, delete x r)
    else 
      (match succ bst with
        | None -> l 
        | Some s -> (let clean_r = delete s r in Node(l, s, clean_r)))

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
SLOVARJI
S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type ('key, 'value) dict = ('key * 'value) tree

(*----------------------------------------------------------------------------*]
Napišite testni primer [test_dict]:
    "b":1
    /    \
"a":0  "d":2
       /
   "c":-2
[*----------------------------------------------------------------------------*)

let test_dict = Node(Node(Empty, ("a", 0), Empty), 
                    ("b", 1), 
                    Node(Node(Empty, ("c", -2), Empty),
                        ("d", 2), 
                        Empty))

(*----------------------------------------------------------------------------*]
Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
slovar vrednosti morda ne vsebuje, vrne [option] tip.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# dict_get "banana" test_dict;;
- : 'a option = None
# dict_get "c" test_dict;;
- : int option = Some (-2)
[*----------------------------------------------------------------------------*)

let rec get_dict k = function
| Empty -> None
| Node (l, ('k, v), r) -> if k = 'k then Some v
 else if k < 'k then 
      get_dict k l
 else get_dict k r 
(*----------------------------------------------------------------------------*]
Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
[int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
vozlišča slovarja.
Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# print_dict test_dict;;
a : 0
b : 1
c : -2
d : 2
- : unit = ()
[*----------------------------------------------------------------------------*)

(*
TA KOMENTAR SEM SAMA NAPISALA: poigraj se s funkcijami 
print_newline: unit -> unit
print_string: string -> unit
print_endline: string -> unit
print_int: int -> unit
string_of_int: int -> string
*)

(*----------------------------------------------------------------------------*]
Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# dict_insert "1" 14 test_dict |> print_dict;;
1 : 14
a : 0
b : 1
c : -2
d : 2
- : unit = ()
# dict_insert "c" 14 test_dict |> print_dict;;
a : 0
b : 1
c : 14
d : 2
- : unit = ()
[*----------------------------------------------------------------------------*)
