module BankAccount

type Account =
    { mutable balance: decimal
      valid: bool }
    member this.getBalance() =
        if this.valid then Some(this.balance) else None

let mkBankAccount () = { balance = 0m; valid = false }

let openAccount account = { account with valid = true }

let closeAccount account = { account with valid = false }

let getBalance (account: Account) = account.getBalance ()

let updateBalance (change: decimal) (account: Account) =
    lock account (fun () -> account.balance <- account.balance + change)
    account
