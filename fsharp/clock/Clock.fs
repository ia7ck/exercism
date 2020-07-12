module Clock

type Clock =
    struct
        val Hours: int
        val Minutes: int

        new(hours: int, minutes: int) =
            if minutes >= 0 then
                { Hours = ((hours + minutes / 60) % 24 + 24) % 24
                  Minutes = minutes % 60 }
            else
                let a = (minutes - 59) / 60
                { Hours = ((hours + a) % 24 + 24) % 24
                  Minutes = (minutes % 60 + 60) % 60 }

        member this.Format = sprintf "%02d:%02d" this.Hours this.Minutes
    end

let create hours minutes = Clock(hours, minutes)

let add minutes (clock: Clock) = Clock(clock.Hours, clock.Minutes + minutes)

let subtract minutes (clock: Clock) =
    Clock(clock.Hours, clock.Minutes - minutes)

let display (clock: Clock) = clock.Format
