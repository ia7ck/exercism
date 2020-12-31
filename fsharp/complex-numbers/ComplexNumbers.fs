module ComplexNumbers

open System

type Complex =
    { real: float
      imaginary: float }

    static member (+)(z1: Complex, z2: Complex) =
        { real = z1.real + z2.real
          imaginary = z1.imaginary + z2.imaginary }

    static member (-)(z1: Complex, z2: Complex) =
        { real = z1.real - z2.real
          imaginary = z1.imaginary - z2.imaginary }

    static member (*)(z1: Complex, z2: Complex) =
        { real = z1.real * z2.real - z1.imaginary * z2.imaginary
          imaginary = z1.imaginary * z2.real + z1.real * z2.imaginary }

    static member (/)(z1: Complex, z2: Complex) =
        let denom =
            z2.real * z2.real + z2.imaginary * z2.imaginary

        { real =
              (z1.real * z2.real + z1.imaginary * z2.imaginary)
              / denom
          imaginary =
              (z1.imaginary * z2.real - z1.real * z2.imaginary)
              / denom }

    member this.Abs() =
        Math.Sqrt
            (this.real * this.real
             + this.imaginary * this.imaginary)

    member this.Conjugate() =
        { this with
              imaginary = -this.imaginary }

    member this.Exp() =
        { real =
              Math.Pow(Math.E, this.real)
              * Math.Cos(this.imaginary)
          imaginary =
              Math.Pow(Math.E, this.real)
              * Math.Sin(this.imaginary) }

    member this.Real() = this.real
    member this.Imaginary() = this.imaginary

let create real imaginary = { real = real; imaginary = imaginary }

let mul (z1: Complex) (z2: Complex) = z1 * z2

let add (z1: Complex) (z2: Complex) = z1 + z2

let sub (z1: Complex) (z2: Complex) = z1 - z2

let div (z1: Complex) (z2: Complex) = z1 / z2

let abs (z: Complex) = z.Abs()

let conjugate (z: Complex) = z.Conjugate()

let real (z: Complex) = z.real

let imaginary (z: Complex) = z.imaginary

let exp (z: Complex) = z.Exp()
