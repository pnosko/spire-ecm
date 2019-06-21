spire-ecm
=========

## Elliptic Curve Method factorization algorithm

This is an experimental library exploring type-level encoding of invariants present in elliptic curve arithmetic.
The result is a factorization algorithm, which verifies membership of points (operands in the arithmetic) to a particular 
elliptic curve at compile time.

This is achieved by encoding the characteristic of elliptic curve over a finite field (forming an abelian group) as a 
type-level integer. A naive solution would be to encode the integer using Peano numbers, however this is impractical 
for large numbers. The solution here is to encode the number as binary digits (in reverse order) on the type level.

```bazaar
sealed trait Nat

// represents a type-level value of number 0
case object BNil extends Nat

// encodes a trailing digit '1' in binary representation
trait One[A <: Nat] extends Nat

// encodes a trailing digit '0' in binary representation
trait Zero[A <: Nat] extends Nat

// represents a type-level value of number 1
type one = One[BNil.type]
``` 


