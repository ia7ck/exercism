module RationalNumbers

open System

let rec gcd a b = if b = 0 then a else gcd b (a % b)

let rec pow (a: int) (b: uint) =
    if b = 0u then 1 else a * (pow a (b - 1u))

type Rational =
    { numer: int
      denom: int }
    static member (+)(r1: Rational, r2: Rational) =
        { numer = r1.numer * r2.denom + r2.numer * r1.denom
          denom = r1.denom * r2.denom }

    static member (-)(r1: Rational, r2: Rational) =
        { numer = r1.numer * r2.denom - r2.numer * r1.denom
          denom = r1.denom * r2.denom }

    static member (*)(r1: Rational, r2: Rational) =
        { numer = r1.numer * r2.numer
          denom = r1.denom * r2.denom }


    static member (/)(r1: Rational, r2: Rational) =
        { numer = r1.numer * r2.denom
          denom = r1.denom * r2.numer }

    static member (^^)(r: Rational, n: int) =
        { numer = pow r.numer (uint (abs n))
          denom = pow r.denom (uint (abs n)) }

    static member (^^)(x: float, r: Rational) =
        Math.Pow(Math.Pow(x, float r.numer), 1.0 / float r.denom)


    member this.Abs() =
        { numer = abs this.numer
          denom = abs this.denom }

    member this.Reduce() =
        let g = gcd this.numer this.denom

        { numer = this.numer / g
          denom = this.denom / g }


let create numerator denominator =
    { numer = numerator
      denom = denominator }
        .Reduce()

let add (r1: Rational) (r2: Rational) = (r1 + r2).Reduce()

let sub (r1: Rational) (r2: Rational) = (r1 - r2).Reduce()

let mul (r1: Rational) (r2: Rational) = (r1 * r2).Reduce()

let div (r1: Rational) (r2: Rational) = (r1 / r2).Reduce()

let abs (r: Rational) = r.Abs()

let exprational (n: int) (r: Rational) = (r ^^ n).Reduce()

let expreal (r: Rational) (n: int) = (float n) ^^ r

let reduce (r: Rational) = r.Reduce()
