module Logic.utilities
open TimeOff


[<Literal>] 
let maxTimeOff = 30

let getDurationOfTimeOff (request:TimeOffRequest)  acc =
    let result = acc + (request.End.Date.Day - request.Start.Date.Day)
    if (request.End.Date.Month = request.Start.Date.Month) then
        result + 1
    else
        result