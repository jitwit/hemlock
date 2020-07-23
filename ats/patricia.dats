#include "share/atspre_staload.hats"
staload UN = "prelude/SATS/unsafe.sats"

typedef BB = [r:nat|0<=r && r < 64] int(r)

datatype patricia (a:t@ype)
= B of (uint, BB, patricia a, patricia a) | L of (uint, a) | E

fun{a:t@ype} singleton (k : uint, v : a) : patricia a = L (k, v)

(* from scheme : (logbit0 b (logor k (- (ash 1 b) 1)))
   set bits clear bit, setting bits below it *)
fn mask (bit:BB, key:uint) : uint = let
 val _2m = g0int2uint (1 << bit) in (lnot _2m) land (key lor (_2m - 1U)) end

(* ((k p b) (= (logbit0 b (logor k (- (ash 1 b) 1)))  p)) *)
fn match_prefix (bit:BB, pre:uint, key:uint) : bool = pre = mask (bit, key)

fn{a:t@ype} match_tree_prefix (tree:patricia a, key:uint) : bool = 
case tree of | B (p,b,_,_) => match_prefix (b,p,key) | _ => false

fn is_bit_set (key:uint, bit:BB) : bool = g0uint_eq (1U, 1U land (key >> bit))

(* branching-bit:  (- (bitwise-length (logxor p1 p2)) 1) *)
fn branch_bit{n,m:nat|n != m} (p1:uint(n), p2:uint(m)) : BB =
$UN.cast(63 - $extfcall(int, "__builtin_clzl", p1 lxor p2))

fn{a:t@ype} join_tree {n,m:nat|n != m}
(px:uint(n), tx:patricia a, py:uint(m), ty:patricia a) : patricia a = 
let val bit = branch_bit(px,py)
    val msk = mask(bit, px) in
if is_bit_set (px,bit) then B(msk,bit,ty,tx) else B (msk,bit,tx,ty) end

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

fn {a:t@ype} insert_with 
(union:(a,a) -> a, k:uint, v: a, T:patricia a) : patricia a = let
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
insert_with (lam(x,y) => x, k,v,T)

fn {a,b,c:t@ype} flip (f : (a,b) -> c) : (b,a) -> c = 
lam (x,y) => f(y,x)

fn {a:t@ype} merge_with
(union:(a,a) -> a,s:patricia a, t:patricia a): patricia a = let 
  fun lp(S:patricia a,T:patricia a) = case (S,T) of
  | (E(),_) => T
  | (_,E()) => S
  | (L(k,v),_) => insert_with (union, k, v, T)
  | (_,L(k,v)) => insert_with (flip union, k, v, T)
  | (B(p,b,sl,sr),B(q,c,tl,tr)) => 
  case p=b && b=c of
// fn match_prefix (bit:BB, pre:uint, key:uint) : bool = pre = mask (bit, key)
  | true => make_tree(p,b,S,T)
  | _ => case (b < c) && match_prefix(c,q,p) of
  | true => if is_bit_set (p,c)
            then make_tree(q,c,tl,lp(S,tr))
            else make_tree(q,c,lp(S,tl),tr)
  | _ => case (c < b) && match_prefix(b,p,q) of
  | true => if is_bit_set(q,b)
            then make_tree(p,b,sl,(lp(sr,T)))
            else E // make_tree(p,b,(lp(sl,T),sr))
  | _ => E
in lp(s,t) end

(* int __builtin_clz (unsigned int x) *)
val egtree : patricia string = insert (8U, "pinou", singleton (12U, "ninou"))
val egtree_ = insert (123U, "minou", egtree)
val () = println!("7   = ", mask(3,10U))
val () = println!("23  = ", mask(3,20U))
val () = println!("103 = ", mask(3,100U))
val () = println!("95  = ", mask(5,100U))
val () = println!("1 = ", is_bit_set(5U,2))
val () = println!("0 = ", is_bit_set(5U,1))
val () = println!("1 = ", is_bit_set(5U,0))
val () = println!("4 = ", branch_bit(5U,20U))
val () = println!("3 = ", branch_bit(10U,5U))
val () = println!(lookup (8U, egtree_))
val () = println!(lookup (12U, egtree_))
val () = println!(lookup (120U, egtree_))
val () = println!(lookup (123U, egtree_))

implement main0 () = ()
