module TimeOff.Tests

open Expecto
open System

let Given (events: RequestEvent list) = events
let ConnectedAs (user: User) (events: RequestEvent list) = events, user
let WithToday (today: DateTime) (events: RequestEvent list, user: User) = events, user, today
let When (command: Command) (events: RequestEvent list, user: User, today: DateTime) = events, user, today, command
let Then expected message (events: RequestEvent list, user: User, today: DateTime, command: Command) =
    let evolveGlobalState (userStates: Map<UserId, Logic.UserRequestsState>) (event: RequestEvent) =
        let userState = defaultArg (Map.tryFind event.Request.UserId userStates) Map.empty
        let newUserState = Logic.evolveUserRequests userState event
        userStates.Add (event.Request.UserId, newUserState)

    let globalState = Seq.fold evolveGlobalState Map.empty events
    let userRequestsState = defaultArg (Map.tryFind command.UserId globalState) Map.empty
    let result = Logic.decide userRequestsState user command today
    Expect.equal result expected message

open System

let reqOctober1 = {
  UserId = "jdoe"
  RequestId = Guid.NewGuid()
  Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
  End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
}

let reqOctober2 = {
  UserId = "jdoe"
  RequestId = Guid.NewGuid()
  Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
  End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
}

let reqOctober3 = {
  UserId = "jdoe"
  RequestId = Guid.NewGuid()
  Start = { Date = DateTime(2019, 10, 3); HalfDay = AM }
  End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
}

let reqJanuaryFull = {
  UserId = "jdoe"
  RequestId = Guid.NewGuid()
  Start = { Date = DateTime(2019, 1, 1); HalfDay = AM }
  End = { Date = DateTime(2019, 1, 31); HalfDay = PM }
}

let reqMidJanuaryToMidFebruary = {
  UserId = "jdoe"
  RequestId = Guid.NewGuid()
  Start = { Date = DateTime(2019, 1, 15); HalfDay = AM }
  End = { Date = DateTime(2019, 2, 15); HalfDay = PM }
}

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      Expect.isTrue (Logic.overlapsWith reqOctober1 reqOctober1) "A request should overlap with istself"
    }

    test "2 requests overlap" {
      Expect.isTrue (Logic.overlapsWith reqJanuaryFull reqMidJanuaryToMidFebruary && Logic.overlapsWith reqMidJanuaryToMidFebruary reqJanuaryFull) "2 requests should overlap, even with reversed order"
    }

    test "Requests on 2 distinct days don't overlap" {
      Expect.isFalse (Logic.overlapsWith reqOctober1 reqOctober2) "The requests don't overlap"
    }

    test "Requests on 3 distinct days don't overlap" {
      let otherRequests = seq {
        yield reqOctober1
        yield reqOctober2
      }
      Expect.isFalse (Logic.overlapsWithAnyRequest otherRequests reqOctober3) "The requests don't overlap"
    }

    test "A request overlaps with at least one of anothers" {
      let otherRequests = seq {
        yield reqOctober1
        yield reqOctober2
      }
      Expect.isTrue (Logic.overlapsWithAnyRequest otherRequests reqOctober2) "A request should overlap with another with the same date"
    }
  ]


[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> WithToday (DateTime(2019, 1, 1))
      |> When (RequestTimeOff reqOctober1)
      |> Then (Ok [RequestCreated reqOctober1]) "The request should have been created"
    }

    test "A request cannot be created in the past" {
      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> WithToday (DateTime(2019, 12, 31))
      |> When (RequestTimeOff reqOctober1)
      |> Then (Error "The request starts in the past") "The request should not have been created because it is in the past!"
    }
  ]


[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      Given [ RequestCreated reqOctober1 ]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (ValidateRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Ok [RequestValidated reqOctober1]) "The request should have been validated"
    }

    test "A request cannot be validated by oneself" {
      Given [ RequestCreated reqOctober1 ]
      |> ConnectedAs (Employee "jdoe")
      |> WithToday (DateTime(2019, 1, 1))
      |> When (ValidateRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Error "Unauthorized") "The request should not have been validated by oneself."
    }

    test "A validated request cannot be validated" {
      Given [ RequestCreated reqOctober1 ; RequestValidated reqOctober1 ]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (ValidateRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Error "Request cannot be validated") "The validated request should not have been validated."
    }
  ]


[<Tests>]
let rejectionTests =
  testList "Rejection tests" [
    test "A request is rejected" {
      Given [ RequestCreated reqOctober1 ]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (RejectRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Ok [RequestRejected reqOctober1]) "The request should have been rejected."
    }

    test "A request cannot be rejected by oneself" {
      Given [ RequestCreated reqOctober1 ]
      |> ConnectedAs (Employee "jdoe")
      |> WithToday (DateTime(2019, 1, 1))
      |> When (RejectRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Error "Unauthorized") "The request should not have been rejected by oneself."
    }

    test "A validated request cannot be rejected" {
      Given [ RequestCreated reqOctober1 ; RequestValidated reqOctober1 ]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (RejectRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Error "Request cannot be rejected") "The validated request should not have been rejected."
    }
  ]


[<Tests>]
let cancellationTests =
  testList "Cancellation tests" [
    test "A request is canceled by the manager" {
      Given [ RequestCreated reqOctober1 ]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (CancelRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Ok [RequestCanceled reqOctober1]) "The request should have been canceled by the manager."
    }

    test "A request is canceled by the employee" {
      Given [ RequestCreated reqOctober1 ]
      |> ConnectedAs (Employee "jdoe")
      |> WithToday (DateTime(2019, 1, 1))
      |> When (CancelRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Ok [RequestCanceled reqOctober1]) "The request should have been canceled by the employee."
    }

    test "A rejected request cannot be canceled" {
      Given [ RequestCreated reqOctober1 ; RequestRejected reqOctober1]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (CancelRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Error "Request cannot be canceled") "The request should have been canceled by the employee."
    }
  ]


[<Tests>]
let cancellationClaimsTests =
  testList "Canellation claims tests" [
    test "An employee claims a cancellation (request in the past)" {
      Given [ RequestCreated reqJanuaryFull ; RequestValidated reqJanuaryFull ]
      |> ConnectedAs (Employee "jdoe")
      |> WithToday (DateTime(2019, 12, 12))
      |> When (CancelRequest ("jdoe", reqJanuaryFull.RequestId))
      |> Then (Ok [CancellationClaimed reqJanuaryFull]) "The request should have been in pending cancellation."
    }

    test "The manager reject a cancellation claim" {
      Given [ RequestCreated reqJanuaryFull ; RequestValidated reqJanuaryFull ; CancellationClaimed reqJanuaryFull]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 12, 12))
      |> When (RejectCancellationClaim ("jdoe", reqJanuaryFull.RequestId))
      |> Then (Ok [CancellationRejected reqJanuaryFull]) "The cancellation claim should have been rejected."
    }

    test "The manager accepts a cancellation claim" {
      Given [ RequestCreated reqJanuaryFull ; RequestValidated reqJanuaryFull ; CancellationClaimed reqJanuaryFull]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 12, 12))
      |> When (CancelRequest ("jdoe", reqJanuaryFull.RequestId))
      |> Then (Ok [RequestCanceled reqJanuaryFull]) "The cancellation claim should have been accepted."
    }
  ]
