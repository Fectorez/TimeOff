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

[<Tests>]
let overlapTests = 
  testList "Overlap tests" [
    test "A request overlaps with itself" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWith request request) "A request should overlap with istself"
    }

    test "Requests on 2 distinct days don't overlap" {
      let request1 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
      }

      let request2 = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWith request1 request2) "The requests don't overlap"
    }

    test "Requests on 3 distinct days don't overlap" {
      let otherRequests = seq {
        yield {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
        }
        yield {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
        }
      }

      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 3); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
      }

      Expect.isFalse (Logic.overlapsWithAnyRequest otherRequests request) "The requests don't overlap"
    }

    test "A request overlaps with at least one of anothers" {
      let otherRequests = seq {
        yield {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
        }
        yield {
          UserId = "jdoe"
          RequestId = Guid.NewGuid()
          Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
          End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
        }
      }

      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
        End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
      }

      Expect.isTrue (Logic.overlapsWithAnyRequest otherRequests request) "A request should overlap with another with the same date"
    }
  ]

[<Tests>]
let creationTests =
  testList "Creation tests" [
    test "A request is created" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> WithToday (DateTime(2019, 1, 1))
      |> When (RequestTimeOff request)
      |> Then (Ok [RequestCreated request]) "The request should have been created"
    }

    test "A request is created in the past" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ ]
      |> ConnectedAs (Employee "jdoe")
      |> WithToday (DateTime(2019, 12, 31))
      |> When (RequestTimeOff request)
      |> Then (Error "The request starts in the past") "The request should not have been created because it is in the past!"
    }
  ]

[<Tests>]
let validationTests =
  testList "Validation tests" [
    test "A request is validated" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestValidated request]) "The request should have been validated"
    }

    test "A request cannot be validated by oneself" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> WithToday (DateTime(2019, 1, 1))
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Error "Unauthorized") "The request should not have been validated by oneself."
    }

    test "A validated request cannot be validated" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ; RequestValidated request ]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (ValidateRequest ("jdoe", request.RequestId))
      |> Then (Error "Request cannot be validated") "The validated request should not have been validated."
    }
  ]


[<Tests>]
let rejectionTests =
  testList "Rejection tests" [
    test "A request is rejected" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (RejectRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestRejected request]) "The request should have been rejected."
    }

    test "A request cannot be rejected by oneself" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> WithToday (DateTime(2019, 1, 1))
      |> When (RejectRequest ("jdoe", request.RequestId))
      |> Then (Error "Unauthorized") "The request should not have been rejected by oneself."
    }

    test "A validated request cannot be rejected" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ; RequestValidated request ]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (RejectRequest ("jdoe", request.RequestId))
      |> Then (Error "Request cannot be rejected") "The validated request should not have been rejected."
    }
  ]



[<Tests>]
let CancellationTests =
  testList "Cancellation tests" [
    test "A request is canceled by the manager" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCanceled request]) "The request should have been canceled by the manager."
    }

    test "A request is canceled by the employee" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ]
      |> ConnectedAs (Employee "jdoe")
      |> WithToday (DateTime(2019, 1, 1))
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Ok [RequestCanceled request]) "The request should have been canceled by the employee."
    }

    test "A rejected request cannot be canceled" {
      let request = {
        UserId = "jdoe"
        RequestId = Guid.NewGuid()
        Start = { Date = DateTime(2019, 12, 27); HalfDay = AM }
        End = { Date = DateTime(2019, 12, 27); HalfDay = PM } }

      Given [ RequestCreated request ; RequestRejected request]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (CancelRequest ("jdoe", request.RequestId))
      |> Then (Error "Request cannot be canceled") "The request should have been canceled by the employee."
    }
  ]
