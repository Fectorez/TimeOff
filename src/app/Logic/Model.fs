namespace TimeOff

open System

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
    | RequestCreated       of TimeOffRequest
    | RequestValidated     of TimeOffRequest
    | RequestCanceled      of TimeOffRequest
    | RequestRejected      of TimeOffRequest
    | CancellationClaimed of TimeOffRequest
    | CancellationRejected of TimeOffRequest
    with
    member this.Request =
        match this with
        | RequestCreated request
        | RequestValidated request
        | RequestCanceled request
        | RequestRejected request
        | CancellationClaimed request
        | CancellationRejected request -> request

// We then define the state of the system,
// and our 2 main functions `decide` and `evolve`
module Logic =

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

        | RequestCreated request -> 
            match state with
            | NotCreated -> Ok [PendingValidation request]
            | _ -> Error "Cannot create an already existing request."

        | RequestValidated request ->
            match state with
            | PendingValidation _ -> Ok [Validated request]
            | _ -> Error "Cannot validate a request not in pending validation."

        | RequestCanceled request ->
            match state with
            | PendingValidation _
            | Validated _
            | RejectedCancellation _
            | PendingCancellation _ -> Ok [Canceled request]
            | _ -> Error "Cannot cancel."
        
        | CancellationClaimed request ->
            match state with
            | PendingValidation _
            | Validated _ -> Ok [PendingCancellation request]
            | _ -> Error "Cannot claim cancel."
        
        | CancellationRejected request ->
            match state with
            | PendingCancellation _ -> Ok [RejectedCancellation request]
            | _ -> Error "Cancellation claim cannot be rejected"

        | RequestRejected request ->
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
        let compBoundaries (a: Boundary) (b: Boundary) =
            let compHalfDays (h1: HalfDay) (h2: HalfDay) =
                match h1, h2 with
                | AM, PM -> -1
                | PM, AM -> 1
                | _ -> 0
            let compDate = a.Date.CompareTo(b.Date)
            if compDate <> 0 then compDate
            else compHalfDays a.HalfDay b.HalfDay

        let inRange (range: TimeOffRequest) (b: Boundary) =
            compBoundaries b range.End <> 1 && compBoundaries b range.Start <> -1

        a.Start |> inRange b || a.End |> inRange b


    let rec overlapsWithAnyRequest (otherRequests: TimeOffRequest seq) (request: TimeOffRequest) =
        if Seq.isEmpty otherRequests then
            false
        else (Seq.head otherRequests) |> overlapsWith request
            || overlapsWithAnyRequest (Seq.tail otherRequests) request




    let createRequest activeUserRequests  request (today: DateTime) =
        if request |> overlapsWithAnyRequest activeUserRequests then
            Error "Overlapping request"
        elif request.Start.Date <= today then
            Error "The request starts in the past"
        else
            Ok [RequestCreated request]

    let validateRequest requestState =
        match requestState with
        | PendingValidation request -> Ok [RequestValidated request]
        | _ -> Error "Request cannot be validated"
    
    let cancelRequest requestState user today =
        match user with
        | Manager ->
            match requestState with
            | PendingValidation request
            | PendingCancellation request
            | Validated request
            | RejectedCancellation request -> Ok [RequestCanceled request]
            | _ -> Error "Request cannot be canceled"
        | Employee _ ->
            match requestState with
            | Validated request
            | PendingValidation request -> 
                if request.Start.Date > today then Ok[RequestCanceled request]
                else Ok[CancellationClaimed request]
            | _ -> Error "Request cannot be canceled"
    
    let rejectRequest requestState =
        match requestState with
        | PendingValidation request -> Ok [RequestRejected request]
        | _ -> Error "Request cannot be rejected"

    let rejectCancellationClaim requestState =
        match requestState with
        | PendingCancellation request -> Ok [CancellationRejected request]
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
                    validateRequest requestState

            | CancelRequest (_, requestId) ->
                let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                cancelRequest requestState user today

            | RejectRequest (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    rejectRequest requestState

            | RejectCancellationClaim (_, requestId) ->
                if user <> Manager then
                    Error "Unauthorized"
                else
                    let requestState = defaultArg (userRequests.TryFind requestId) NotCreated
                    rejectCancellationClaim requestState
