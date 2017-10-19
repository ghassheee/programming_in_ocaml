# Introduction

Atsushi Igarashi's OCaml Introduction is very nice.
I read his "Programming in OCaml [Japanese]".
I revised with category theory, or Algebra of Programming at some point.

# Usage

```
$ git clone http://github.com/ghassheee/programming_in_ocaml
$ cd programming_in_ocaml
$ utop      ## or just type ocaml
# #use "base.ml" ;;

module Int :
  sig
    val even : int -> bool
    val odd : int -> bool
    val abs : int -> int
    val pow : int -> int -> int
    val fact : int -> int
    val sum : int -> int -> (int -> int) -> int
    val square : int -> int
    val cube : int -> int
    val sum_of_square : int -> int
    val sum_of_cube : int -> int
    val table : (int64, int64) Hashtbl.t
    val fibo : int64 -> int64
    val fib : int -> int64
    val tbl : (int * int, int) Hashtbl.t
    val comb : int -> int -> int
    val gcd : int -> int -> int
  end
module Float :
  sig
    val pi : float
    val e : float
    val abs : float -> float
    val area_of_circle : float -> float
    val square : float -> float
    val pow : float -> int -> float
    val cube : float -> float
    val derivation :
      (float -> float) -> float -> float
    val fixedpoint :
      (float -> float) -> float -> float
    val newton1step :
      (float -> float) -> float -> float
    val newton_method :
      (float -> float) -> float -> float
    val sqrt : float -> float
    val integral :
      (float -> float) -> float -> float -> float
    val add_average : float -> float -> float
    val mul_average : float -> float -> float
    val average : float * float -> float * float
    val fix_average :
      (float * float -> float * float) ->
      float * float -> float * float
    val med_average : float -> float -> float * float
    val arctan1 : int -> float
  end
...

# let cos = derivation sin;;
val cos : float -> float = <fun>
# cos 0.0;;
- : float = 1.
# integral sin 0. pi ;;
- : float = 2.00000000000018563
```

