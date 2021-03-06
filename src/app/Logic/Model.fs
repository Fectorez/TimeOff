namespace TimeOff

open System
open System.Collections.Generic

// Then our commands
type Command =
    | RequestTimeOff  of TimeOffRequest
    | ValidateRequest of UserId * Guid
    | CancelRequest   of UserId * Guid
    | RejectRequest   of UserId * Guid
    | RejectCancellationClaim of UserId * Guid  // manager refuse la demande d'annulation
    with
    member this.UserId =
        match this with
        | RequestTimeOff request -> request.UserId
        | ValidateRequest (userId, _)
        | CancelRequest   (userId, _)
        | RejectRequest   (userId, _)
        | RejectCancellationClaim (userId, _) -> userId

// And our events
type RequestEvent =
    | RequestCreated       of TimeOffRequest * DateTime
    | RequestValidated     of TimeOffRequest * DateTime
    | RequestCanceled      of TimeOffRequest * DateTime
    | RequestRejected      of TimeOffRequest * DateTime
    | CancellationClaimed  of TimeOffRequest * DateTime
    | CancellationRejected of TimeOffRequest * DateTime
    with
    member this.Request =
        match this with
        | RequestCreated (request, _)
        | RequestValidated (request, _)
        | RequestCanceled (request, _)
        | RequestRejected (request, _)
        | CancellationClaimed (request, _)
        | CancellationRejected (request, _) -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

    [<Literal>]
    let TIME_OFF_PER_MONTH = 2.08

    [<Literal>]
    let MONTHS_PER_YEAR = 12

    let HOLIDAYS = [
        DateTime(2018, 1, 1)
        DateTime(2019, 1, 1)
        DateTime(2020, 1, 1)
        DateTime(2019, 11, 11)
    ]

    type RequestState =
        | NotCreated
        | PendingValidation of TimeOffRequest
        | Validated of TimeOffRequest
        | Canceled of TimeOffRequest
        | Rejected of TimeOffRequest
        | RejectedCancellation of TimeOffRequest
        | PendingCancellation of TimeOffRequest with
        member this.Request =
            match this with
            | NotCreated -> invalidOp "Not created"
            | PendingValidation request
            | Validated request
            | Canceled request
            | Rejected request
            | RejectedCancellation request
            | PendingCancellation request -> request
        member this.IsActive =
            match this with
            | NotCreated
            | Canceled _
            | Rejected _ -> false
            | _ -> true

    type UserRequestsState = Map<Guid, RequestState>

    // Pour cette fonction, voir le schéma états/transitions
    let evolveRequest (state: RequestState) (event: RequestEvent) =
        match event with

        | RequestCreated (request, _) -> 
            match state with
            | NotCreated -> Ok [PendingValidation request]
            | _ -> Error "Cannot create an already existing request."

        | RequestValidated (request, _) ->
            match state with
            | PendingValidation _ -> Ok [Validated request]
            | _ -> Error "Cannot validate a request not in pending validation."

        | RequestCanceled (request, _) ->
            if state.IsActive then Ok [Canceled request]
            else Error "Cannot cancel."
        
        | CancellationClaimed (request, _) ->
            match state with
            | PendingValidation _
            | Validated _ -> Ok [PendingCancellation request]
            | _ -> Error "Cannot claim cancel."
        
        | CancellationRejected (request, _) ->
            match state with
            | PendingCancellation _ -> Ok [RejectedCancellation request]
            | _ -> Error "Cancellation claim cannot be rejected"

        | RequestRejected (request, _) ->
            match state with
            | PendingValidation _ -> Ok [Rejected request]
            | _ -> Error "Cannot reject a request not in pending validation."


    let evolveUserRequests (userRequests: UserRequestsState) (event: RequestEvent) =
        let requestState = defaultArg (Map.tryFind event.Request.RequestId userRequests) NotCreated
        let newRequestState = evolveRequest requestState event
        match newRequestState with
            | Ok stateList -> userRequests.Add (event.Request.RequestId, stateList.Head)
            | _ -> userRequests

    
    
    let overlapsWith (a: TimeOffRequest) (b: TimeOffRequest) =
        let inRange (range: TimeOffRequest) (b: Boundary) =
            b >= range.Start && b <= range.End

        a.Start |> inRange b || a.End |> inRange b


    let rec overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) (request: TimeOffRequest) =
        Seq.exists (fun otherReq -> request |> overlapsWith otherReq) otherRequests




    let createRequest activeUserRequests  request (today: DateTime) =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated (request, today)]

    let validateRequest requestState (today: DateTime) =
        match requestState with
        | PendingValidation request -> Ok [RequestValidated (request, today)]
        | _ -> Error "Request cannot be validated"
    
    let cancelRequest requestState user today =
        match user with
        | Manager ->
            match requestState with
            | PendingValidation request
            | PendingCancellation request
            | Validated request
            | RejectedCancellation request -> Ok [RequestCanceled (request, today)]
            | _ -> Error "Request cannot be canceled"
        | Employee _ ->
            match requestState with
            | Validated request
            | PendingValidation request -> 
                if request.Start.Date > today then Ok[RequestCanceled (request, today)]
                else Ok[CancellationClaimed (request, today)]
            | _ -> Error "Request cannot be canceled"
    
    let rejectRequest requestState (today: DateTime) =
        match requestState with
        | PendingValidation request -> Ok [RequestRejected (request, today)]
        | _ -> Error "Request cannot be rejected"

    let rejectCancellationClaim requestState (today: DateTime) =
        match requestState with
        | PendingCancellation request -> Ok [CancellationRejected (request, today)]
        | _ -> Error "Cancellation claim cannot be rejected"



    let decide (userRequests: UserRequestsState) (user: User) (command: Command) (today: DateTime) =
        let relatedUserId = command.UserId
        match user with
        | Employee userId when userId <> relatedUserId ->
            Error "Unauthorized"
        | _ ->
            match command with
            | RequestTimeOff request ->
                let activeUserRequests =
                    userRequests
                    |> Map.toSeq
                    |> Seq.map (fun (_, state) -> state)
                    |> Seq.where (fun state -> state.IsActive)
                    |> Seq.map (fun state -> state.Request)

                createRequest activeUserRequests request today

            | ValidateRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    validateRequest requestState today

            | CancelRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                cancelRequest requestState user today

            | RejectRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    rejectRequest requestState today

            | RejectCancellationClaim (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    rejectCancellationClaim requestState today

    // nb jours de la semaine entre 2 dates, hors jours fériés
    let rec getBusinessDays (startD: DateTime) (endD: DateTime): int =
        if startD > endD then getBusinessDays endD startD
        else
            let rec getBusinessDaysRec (startD: DateTime) (endD: DateTime) (acc: int): int =
                if startD > endD then
                    acc
                else if startD.DayOfWeek = DayOfWeek.Saturday
                    || startD.DayOfWeek = DayOfWeek.Sunday
                    || List.contains startD HOLIDAYS then
                    getBusinessDaysRec (startD.AddDays 1.0) endD acc
                else
                    getBusinessDaysRec (startD.AddDays 1.0) endD (acc + 1)
            getBusinessDaysRec startD endD 0

    let timeOffDuration (timeOff: TimeOffRequest): float =
        let boundaryDiff (a: Boundary) (b: Boundary) =
            let diffDays = float(getBusinessDays a.Date b.Date)
            match (diffDays, a.HalfDay, b.HalfDay) with
            | (0.0, _, _)
            | (_, AM, PM) -> diffDays
            | (_, AM, AM)
            | (_, PM, PM) -> diffDays - 0.5
            | (_, PM, AM) -> diffDays - 1.0
        
        boundaryDiff timeOff.Start timeOff.End

    //  Le cumul des congés attribués depuis le début de l’année civile.
    let timeOffDurationList (requests: list<TimeOffRequest>): float =
        List.fold (fun acc t -> acc + timeOffDuration t) 0.0 requests

    // A. Total nombre congés de l'année jusqu'à aujourd'hui (mois courant non compris)
    let totalTimeOffUntilDate (date: DateTime): float =
        float (date.Month - 1) * TIME_OFF_PER_MONTH

    // B. congés restants pour l'année (négatif si trop pris)
    let remainingInCompletedYear (requests: list<TimeOffRequest>) (year: int): float =
        TIME_OFF_PER_MONTH * float MONTHS_PER_YEAR - timeOffDurationList ( List.filter (fun timeOff -> timeOff.Start.Date.Year = year ) requests)

    // C.
    let takenToDate (requests: list<TimeOffRequest>) (today: DateTime): float =
        requests
        |> List.filter (fun timeOff -> timeOff.Start.Date.Year = today.Year)
        |> List.filter (fun timeOff -> timeOff.Start.Date <= today)
        |> timeOffDurationList
    
    // D.
    let plannedTimeOff (requests: list<TimeOffRequest>) (today: DateTime): float =
        requests
        |> List.filter (fun timeOff -> timeOff.Start.Date.Year = today.Year)
        |> List.filter (fun timeOff -> timeOff.Start.Date > today)
        |> timeOffDurationList
        
    // E. Solde disponible pour l'année
    let timeOffBalance (requests: list<TimeOffRequest>) (today: DateTime) =
        totalTimeOffUntilDate today
        + remainingInCompletedYear requests (today.Year - 1)
        - takenToDate requests today
        - plannedTimeOff requests today
    
    // History
    let getHistory (requestsEvents: seq<RequestEvent>) (today: DateTime) =
        let dateToString (date: DateTime) =
            date.ToString "dd/MM/yyyy"
        let boundaryToString (boundary: Boundary) =
            dateToString boundary.Date + " " + string boundary.HalfDay
        let buildLine event =
            match event with
            | RequestCreated (request, date) ->  (dateToString date, boundaryToString request.Start, boundaryToString request.End, timeOffDuration request, "New request")
            | RequestValidated (request, date) ->  (dateToString date, boundaryToString request.Start, boundaryToString request.End, timeOffDuration request, "Validated by manager")
            | RequestCanceled (request, date) ->  (dateToString date, boundaryToString request.Start, boundaryToString request.End, timeOffDuration request, "Canceled")
            | RequestRejected (request, date) ->  (dateToString date, boundaryToString request.Start, boundaryToString request.End, timeOffDuration request, "Rejected")
            | CancellationClaimed (request, date) ->  (dateToString date, boundaryToString request.Start, boundaryToString request.End, timeOffDuration request, "Cancellation claimed")
            | CancellationRejected (request, date) ->  (dateToString date, boundaryToString request.Start, boundaryToString request.End, timeOffDuration request, "Cancellation rejected")
        let onlyThisYear year event =
            match event with
            | RequestCreated (request, date)
            | RequestValidated (request, date)
            | RequestCanceled (request, date)
            | RequestRejected (request, date)
            | CancellationClaimed (request, date)
            | CancellationRejected (request, date) ->  request.Start.Date.Year = year
        requestsEvents

        |> Seq.filter (onlyThisYear today.Year)
        |> Seq.map buildLine
        |> Seq.toList