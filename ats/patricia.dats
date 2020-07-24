#include "share/atspre_staload.hats"
staload UN = "prelude/SATS/unsafe.sats"

typedef BB = [r:nat|0<=r && r < 64] int(r)

#define SGNN %(0-1)
#define SGNZ %(0)
#define SGNP %(1)

datatype patricia (a:t@ype)
= B of (uint, BB, patricia a, patricia a) | L of (uint, a) | E

fun{a:t@ype} singleton (k : uint, v : a) : patricia a = L (k, v)

(* set bits clear bit, setting bits below it *)
fn mask (bit:BB, key:uint) : uint = let
 val _2m = g0int2uint (1 << bit) in (lnot _2m) land (key lor (_2m - 1U)) end
(* determine if pre is prefix for key in context of bit *)
fn match_prefix (bit:BB, pre:uint, key:uint) : bool = pre = mask (bit, key)
fn is_bit_set (bit:BB, key:uint) : bool = g0uint_eq (1U, 1U land (key >> bit))

(* branching-bit:  (- (bitwise-length (logxor p1 p2)) 1) *)
fn branch_bit{n,m:nat|n != m} (p1:uint(n), p2:uint(m)) : BB =
$UN.cast(63 - $extfcall(int, "__builtin_clzl", p1 lxor p2))

fn{a:t@ype} join_tree {n,m:nat|n != m}
(px:uint(n), tx:patricia a, py:uint(m), ty:patricia a) : patricia a = 
let val bit = branch_bit(px,py)
    val msk = mask(bit,px) in
if is_bit_set (bit,px) then B(msk,bit,ty,tx) else B (msk,bit,tx,ty) end

fn{a:t@ype} make_tree 
(p:uint, b:BB, tx:patricia a, ty:patricia a) : patricia a =
case tx of | E() => ty | _ => case ty of | E() => tx | _ => B (p,b,tx,ty)

fn {a:t@ype} lookup (key:uint, tree:patricia a) : Option a = let
  fun lp (tree : patricia a) = case tree of
    | E() => None
    | L(k,v) => if k = key then Some v else None
    | B(p,b,L,R) when match_prefix (b,p,key) => if key <= p then lp L else lp R
    | _ => None
in lp tree end

fn {a:t@ype} delete (key:uint, tree:patricia a) : patricia a = let
  fun lp (tree : patricia a) = case tree of
    | E() => tree
    | L(k,v) => if k = key then E else tree
    | B(p,b,L,R) => 
      if key <= p then make_tree (p,b,lp L,R) else make_tree (p,b,L,lp R)
in lp tree end

fn {a:t@ype} insert_with
(union:(a,a) -<cloref1> a, k:uint, v: a, T:patricia a) : patricia a = let
  fun lp (T : patricia a) = case T of
  | E() => L (k, v)
  | L(k_,v_) => let
    val pk = g1ofg0(k) val pk_ = g1ofg0(k_)
    in if pk = pk_ then L(k,v) else join_tree (pk,L(k,v),pk_,T) end
  | B(p,b,L,R) => let
    val pk = g1ofg0(k) val pk_ = g1ofg0(p)
    in if pk = pk_
    then make_tree(k,b,singleton(k,v),T)
    else join_tree (pk,singleton(k,v),pk_,T) end
in lp T end

fn{a:t@ype} insert (k:uint, v: a, T:patricia a) : patricia a =
insert_with (lam(x,y) => x,k,v,T)

fn {a,b,c:t@ype} flip (f : (a,b) -<cloref1> c) : (b,a) -<cloref1> c =
lam (x,y) => f (y,x)

fn {a:t@ype} merge_with
(union:(a,a) -<cloref1> a,s:patricia a, t:patricia a): patricia a = let 
  fun lp(S:patricia a,T:patricia a) = case (S,T) of
  | (E(),_) => T
  | (_,E()) => S
  | (L(k,v),_) => insert_with (union, k, v, T)
  | (_,L(k,v)) => insert_with (flip union, k, v, T)
  | (B(p,b,sl,sr),B(q,c,tl,tr)) =>
  let val pk = g1ofg0(p) val qk = g1ofg0(q) in
    if pk != qk
    then if true then join_tree (pk,T,qk,S) else E
    else case+ compare (b,c) of 
    | SGNN => if match_prefix (c,p,q) 
              then make_tree(q,c,tl,lp(S,tr))
              else make_tree(q,c,lp(S,tl),tr)
    | SGNZ => make_tree(p,b,lp(sl,tl),lp(sr,tr))
    | SGNP => if match_prefix(b,p,q)
              then make_tree (p,b,sl,lp(sr,T))
              else make_tree (p,b,lp(sl,T),sr) end
in lp(s,t) end


(* int __builtin_clz (unsigned int x) *)
val egtree : patricia string = insert (8U, "pinou", singleton (12U, "ninou"))
val egtree_ = insert (123U, "minou", egtree)
val eerteg : patricia string = insert (80U, "uonip", singleton (21U, "uonin"))
val mtree = merge_with (lam(x,y) => x, eerteg, egtree_) 
val () = println!("7   = ", mask(3,10U))
val () = println!("23  = ", mask(3,20U))
val () = println!("103 = ", mask(3,100U))
val () = println!("95  = ", mask(5,100U))
val () = println!("1 = ", is_bit_set(2,5U))
val () = println!("0 = ", is_bit_set(1,5U))
val () = println!("1 = ", is_bit_set(0,5U))
val () = println!("4 = ", branch_bit(5U,20U))
val () = println!("3 = ", branch_bit(10U,5U))
val () = println!("pinou? ",lookup (8U, egtree_))
val () = println!("ninou? ",lookup (12U, egtree_))
val () = println!("none? ",lookup (120U, egtree_))
val () = println!("minou? ",lookup (123U, egtree_))
val () = println!("pinou? ",lookup (8U, mtree))
val () = println!("uonip? ",lookup (80U, mtree))
val () = println!("ninou? ",lookup (12U, mtree))
val () = println!("uonin? ",lookup (21U, mtree))
val () = println!("minou? ",lookup (123U, mtree))
val () = println!("none? ",lookup (123U, delete(123U,mtree)))
val () = println!("pinou? ",lookup (8U, delete(123U,mtree)))

implement main0 () = ()
