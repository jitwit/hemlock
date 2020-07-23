#include "share/atspre_staload.hats"
#include "prelude/fixity.ats"
staload UN = "prelude/SATS/unsafe.sats"

datatype patricia ( a:t@ype )
= B of {n:nat} (uint, int(n), patricia(a), patricia(a))
| L of (uint, a)
| E

fun{a:t@ype} singleton ( k : uint, v : a ) : patricia (a) = L (k, v)

(* from scheme : (logbit0 b (logor k (- (ash 1 b) 1)))
   set bits clear bit, setting bits below it *)
fn mask{n:nat|0 <= n && n < 64} (bit:int(n), key:uint) : uint = let
 val _2m = g0int2uint (1 << bit) in (lnot _2m) land (key lor (_2m - 1U)) end

(* ((k p b) (= (logbit0 b (logor k (- (ash 1 b) 1)))  p)) *)
fn match_prefix{n:nat|0<=n && n < 64} (bit:int(n), pre:uint, key:uint) : bool =
  g0uint_eq(pre, mask (bit, key))

fn is_bit_set{n:nat|0<=n && n < 64} (key:uint, bit:int(n)) : bool =
  g0uint_eq (1U, 1U land (key >> bit))

(* cool *)
(* branching-bit   (- (bitwise-length (logxor p1 p2)) 1) *)
fn branch_bit{n,m:nat|n != m} (p1:uint(n), p2:uint(m)) : [r:nat|0<=r && r < 64] int(r) =
  $UN.cast(63 - $extfcall(int, "__builtin_clzl", p1 lxor p2))

(* int __builtin_clz (unsigned int x) *)
val () = println!("7   = ", mask(3,10U))
val () = println!("23  = ", mask(3,20U))
val () = println!("103 = ", mask(3,100U))
val () = println!("95  = ", mask(5,100U))
val () = println!("1 = ", is_bit_set(5U,2))
val () = println!("0 = ", is_bit_set(5U,1))
val () = println!("1 = ", is_bit_set(5U,0))
val () = println!("4 = ", branch_bit(5U,20U))
val () = println!("3 = ", branch_bit(10U,5U))

implement main0 () = ()
