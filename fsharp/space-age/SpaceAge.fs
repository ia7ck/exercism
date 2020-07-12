module SpaceAge

type Planet =
    | Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune

let earthYear(seconds : int64) : float =
    (float) seconds / (float) 31557600

let age (planet : Planet) (seconds : int64) : float =
    match planet with
    | Mercury -> earthYear seconds / 0.2408467
    | Venus -> earthYear seconds / 0.61519726
    | Earth -> earthYear seconds
    | Mars -> earthYear seconds / 1.8808158
    | Jupiter -> earthYear seconds / 11.862615
    | Saturn -> earthYear seconds / 29.447498
    | Uranus -> earthYear seconds / 84.016846
    | Neptune -> earthYear seconds / 164.79132
