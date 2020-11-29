module DiffieHellman

open System

let privateKey (primeP: bigint) =
    let rand = Random()
    rand.Next(2, int (primeP) - 1) |> bigint

let publicKey (primeP: bigint) (primeG: bigint) (privateKey: bigint) = bigint.ModPow(primeG, privateKey, primeP)

let secret (primeP:bigint) (publicKey:bigint) (privateKey:bigint) = 
    bigint.ModPow(publicKey, privateKey, primeP)
