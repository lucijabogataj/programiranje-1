(* ========== Vaja 8: Moduli  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
"Once upon a time, there was a university with a peculiar tenure policy. All
 faculty were tenured, and could only be dismissed for moral turpitude. What
 was peculiar was the definition of moral turpitude: making a false statement
 in class. Needless to say, the university did not teach computer science.
 However, it had a renowned department of mathematics.
 One Semester, there was such a large enrollment in complex variables that two
 sections were scheduled. In one section, Professor Descartes announced that a
 complex number was an ordered pair of reals, and that two complex numbers were
 equal when their corresponding components were equal. He went on to explain
 how to convert reals into complex numbers, what "i" was, how to add, multiply,
 and conjugate complex numbers, and how to find their magnitude.
 In the other section, Professor Bessel announced that a complex number was an
 ordered pair of reals the first of which was nonnegative, and that two complex
 numbers were equal if their first components were equal and either the first
 components were zero or the second components differed by a multiple of 2π. He
 then told an entirely different story about converting reals, "i", addition,
 multiplication, conjugation, and magnitude.
 Then, after their first classes, an unfortunate mistake in the registrar's
 office caused the two sections to be interchanged. Despite this, neither
 Descartes nor Bessel ever committed moral turpitude, even though each was
 judged by the other's definitions. The reason was that they both had an
 intuitive understanding of type. Having defined complex numbers and the
 primitive operations upon them, thereafter they spoke at a level of
 abstraction that encompassed both of their definitions.
 The moral of this fable is that:
   Type structure is a syntactic discipline for enforcing levels of
   abstraction."
 from:
 John C. Reynolds, "Types, Abstraction, and Parametric Polymorphism", IFIP83
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Definirajte signaturo [NAT], ki določa strukturo naravnih števil. Ima osnovni 
 tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in množenje.
 Hkrati naj vsebuje pretvorbe iz in v OCamlov [int] tip.
 Opomba: Funkcije za pretvarjanje ponavadi poimenujemo [to_int] and [of_int],
 tako da skupaj z imenom modula dobimo ime [NAT.of_int], ki nam pove, da 
 pridobivamo naravno število iz celega števila.
[*----------------------------------------------------------------------------*)

module type NAT = sig
  type t 

  val eq : t -> t -> bool
  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t 
  val to_int : t -> int 
  (* [of_int k] če je k negativen:
  - se sesuje
  - uporabi 0
  - uporabi absolutno vrednost
  Vsak se sam odloči, asistent je vzel drugo možnost*)
  val of_int : int -> t

end

(*----------------------------------------------------------------------------*]
 Napišite implementacijo modula [Nat_int], ki zgradi modul s signaturo [NAT],
 kjer kot osnovni tip uporablja OCamlov tip [int].
 Namig: Dokler ne implementirate vse funkcij v [Nat_int] se bo OCaml pritoževal.
 Temu se lahko izognete tako, da funkcije, ki še niso napisane nadomestite z 
 [failwith "later"], vendar to ne deluje za konstante.
[*----------------------------------------------------------------------------*)

module Nat_int : NAT  = struct

  type t = int 

  let eq x y =
    x = y
  let zero = 0
  let one = 1
  let add = ( + )
  let sub x y = max 0 (x - y)
  let mult x y = ( * ) x y
  let to_int x = x
  let of_int x = max x 0

end

(*----------------------------------------------------------------------------*]
 Napišite implementacijo [NAT], ki temelji na Peanovih aksiomih:
 https://en.wikipedia.org/wiki/Peano_axioms
   
 Osnovni tip modula definirajte kot vsotni tip, ki vsebuje konstruktor za ničlo
 in konstruktor za naslednika nekega naravnega števila.
 Večino funkcij lahko implementirate s pomočjo rekurzije. Naprimer, enakost
 števil [k] in [l] določimo s hkratno rekurzijo na [k] in [l], kjer je osnoven
 primer [Zero = Zero].
[*----------------------------------------------------------------------------*)

module Nat_peano : NAT = struct

  type t = Zero | S of t 
  let rec eq x y = 
    match (x, y) with 
    | Zero, Zero -> true
    | Zero, S _ -> false
    | S _, Zero -> false
    | S x', S y' -> eq x' y'
  let zero = Zero
  let one = S Zero
  let rec add x = function
    | Zero -> x
    | S y -> add (S x) y
  let rec sub x y =
    match(x, y) with 
    | (_, Zero) -> x
    | (Zero, _) -> Zero
    | (S x, S y) -> sub x y
  let rec mult x = function
    | Zero -> Zero
    | S y -> add x (mult x y)
  let rec of_int i =
    if i <= 0 then Zero else S (of_int (i - 1))
  let rec to_int = function
    | Zero -> 0
    | S x -> 1 + (to_int x)

end

(*----------------------------------------------------------------------------*]
 V OCamlu lahko module podajamo kot argumente funkcij, z uporabo besede
 [module]. Funkcijo, ki sprejme modul torej definiramo kot
 # let f (module M : M_sig) = ...
 in ji podajamo argumente kot 
 # f (module M_implementation);;
 Funkcija [sum_nat_100] sprejme modul tipa [NAT] in z uporabo modula sešteje
 prvih 100 naravnih števil. Ker funkcija ne more vrniti rezultata tipa [NAT.t]
 (saj ne vemo, kateremu od modulov bo pripadal, torej je lahko [int] ali pa
  variantni tip) na koncu vrnemo rezultat tipa [int] z uporabo metode [to_int].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # sum_nat_100 (module Nat_int);;
 - : int = 4950
 # sum_nat_100 (module Nat_peano);;
 - : int = 4950
[*----------------------------------------------------------------------------*)

let sum_nat_100 (module Nat : NAT) = 
  let hundred = Nat.of_int 100 in
  let rec pristej i acc = 
    if Nat.eq i hundred 
      then acc 
    else 
      pristej (Nat.add Nat.one i) (Nat.add i acc)
  in Nat.to_int (pristej Nat.one Nat.zero)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Now we follow the fable told by John Reynolds in the introduction.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Definirajte signaturo modula kompleksnih števil.
 Potrebujemo osnovni tip, test enakosti, ničlo, enko, imaginarno konstanto i,
 negacijo, konjugacijo, seštevanje in množenje. 
[*----------------------------------------------------------------------------*)

module type COMPLEX = sig
  type t
  val eq : t -> t -> bool
  val zero : t 
  val one : t 
  val i : t
  val neg : t -> t 
  val kon : t -> t 
  val add : t -> t -> t
  val mult : t -> t -> t
end

(*----------------------------------------------------------------------------*]
 Napišite kartezično implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število realno in imaginarno komponento.
[*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq x y = (x.re = y.re) && (x.im = y.im)
  let zero = {re = 0.; im = 0.}
  let one = {re = 1.; im = 0.}
  let i = {re = 0.; im = 1.}
  let neg {re; im} = {re = -. re; im = -. im}
  let kon {re; im} = {re; im = -. im}
  let add x y = {re = x.re +. y.re; im = x.im +. y.im}
  let mult x y = {re = x.re *. y.re +. x.im *. y.im;
                  im = x.im *. y.re +. y.im *. x.re}

end

(*----------------------------------------------------------------------------*]
 Sedaj napišite še polarno implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število radij in kot (angl. magnitude in argument).
   
 Priporočilo: Seštevanje je v polarnih koordinatah zahtevnejše, zato si ga 
 pustite za konec (lahko tudi za konec stoletja).
[*----------------------------------------------------------------------------*)

module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}

  (* Pomožne funkcije za lažje življenje. *)
  let pi = 2. *. acos 0.
  let rad_of_deg deg = (deg /. 180.) *. pi
  let deg_of_rad rad = (rad /. pi) *. 180.

  let eq x y = 
    (x.magn = 0. && y.magn = 0.) || 
    (x.magn = y.magn && mod_float x.arg 360. = mod_float y.arg 360.)
  let zero = {magn = 0.; arg = 0.}
  let one = {magn = 1.; arg = 0.}
  let i = {magn = 0.; arg = 90.}
  let neg {magn; arg} = {magn; arg = 180. +. arg}
  let kon {magn; arg} = {magn; arg = 360. -. 2. *. arg}
  let mult x y = {magn = x.magn *. y.magn ; arg = x.arg +. y.arg}

  let add x y = failwith "Ni še konec stoletja! :) "

end

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Na vajah z iskalnimi drevesi smo definirali tip slovarjev 
 [('key, 'value) dict], ki je implementiral [dict_get], [dict_insert] in
 [print_dict]. Napišite primerno signaturo za slovarje [DICT] in naredite
 implementacijo modula z drevesi (kot na prejšnjih vajah). 
 
 Modul naj vsebuje prazen slovar [empty] in pa funkcije [get], [insert] in
 [print] (print naj ponovno deluje zgolj na [(string, int) t].
[*----------------------------------------------------------------------------*)


(*----------------------------------------------------------------------------*]
 Funkcija [count (module Dict) list] prešteje in izpiše pojavitve posameznih
 elementov v seznamu [list] s pomočjo izbranega modula slovarjev.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # count (module Tree_dict) ["b"; "a"; "n"; "a"; "n"; "a"];;
 a : 3
 b : 1
 n : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let count (module Dict : DICT) list = ()
