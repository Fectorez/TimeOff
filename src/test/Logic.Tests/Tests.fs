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
open TimeOff.AuthTypes

let reqOctober1 = {
  UserId = "jdoe"
  RequestId = Guid.NewGuid()
  Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
  End = { Date = DateTime(2019, 10, 1); HalfDay = PM }
}

let reqOctober1AM = {
  UserId = "jdoe"
  RequestId = Guid.NewGuid()
  Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
  End = { Date = DateTime(2019, 10, 1); HalfDay = AM }
}

let reqOctober2 = {
  UserId = "jdoe"
  RequestId = Guid.NewGuid()
  Start = { Date = DateTime(2019, 10, 2); HalfDay = AM }
  End = { Date = DateTime(2019, 10, 2); HalfDay = PM }
}

let reqOctober2_2018 = {
  UserId = "jdoe"
  RequestId = Guid.NewGuid()
  Start = { Date = DateTime(2018, 10, 2); HalfDay = AM }
  End = { Date = DateTime(2018, 10, 2); HalfDay = PM }
}

let reqOctober3 = {
  UserId = "jdoe"
  RequestId = Guid.NewGuid()
  Start = { Date = DateTime(2019, 10, 3); HalfDay = AM }
  End = { Date = DateTime(2019, 10, 3); HalfDay = PM }
}

let reqOctober3_2018 = {
  UserId = "jdoe"
  RequestId = Guid.NewGuid()
  Start = { Date = DateTime(2018, 10, 3); HalfDay = AM }
  End = { Date = DateTime(2018, 10, 3); HalfDay = PM }
}

let reqJanuaryFull = {
  UserId = "jdoe"
  RequestId = Guid.NewGuid()
  Start = { Date = DateTime(2019, 1, 1); HalfDay = AM }
  End = { Date = DateTime(2019, 1, 31); HalfDay = PM }
}

let reqJanuaryFull2018 = {
  UserId = "jdoe"
  RequestId = Guid.NewGuid()
  Start = { Date = DateTime(2018, 1, 1); HalfDay = AM }
  End = { Date = DateTime(2018, 1, 31); HalfDay = PM }
}

let reqOctoberFull = {
  UserId = "jdoe"
  RequestId = Guid.NewGuid()
  Start = { Date = DateTime(2019, 10, 1); HalfDay = AM }
  End = { Date = DateTime(2019, 10, 31); HalfDay = PM }
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
      |> Then (Ok [RequestCreated (reqOctober1, DateTime(2019, 1, 1))]) "The request should have been created"
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
      Given [ RequestCreated (reqOctober1, DateTime(2019, 1, 1)) ]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (ValidateRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Ok [RequestValidated (reqOctober1, DateTime(2019, 1, 1))]) "The request should have been validated"
    }

    test "A request cannot be validated by oneself" {
      Given [ RequestCreated (reqOctober1, DateTime(2019, 1, 1)) ]
      |> ConnectedAs (Employee "jdoe")
      |> WithToday (DateTime(2019, 1, 1))
      |> When (ValidateRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Error "Unauthorized") "The request should not have been validated by oneself."
    }

    test "A validated request cannot be validated" {
      Given [ RequestCreated (reqOctober1, DateTime(2019, 1, 1)) ; RequestValidated (reqOctober1, DateTime(2019, 1, 1)) ]
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
      Given [ RequestCreated (reqOctober1, DateTime(2019, 1, 1)) ]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (RejectRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Ok [RequestRejected (reqOctober1, DateTime(2019, 1, 1))]) "The request should have been rejected."
    }

    test "A request cannot be rejected by oneself" {
      Given [ RequestCreated (reqOctober1, DateTime(2019, 1, 1)) ]
      |> ConnectedAs (Employee "jdoe")
      |> WithToday (DateTime(2019, 1, 1))
      |> When (RejectRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Error "Unauthorized") "The request should not have been rejected by oneself."
    }

    test "A validated request cannot be rejected" {
      Given [ RequestCreated (reqOctober1, DateTime(2019, 1, 1)) ; RequestValidated (reqOctober1, DateTime(2019, 1, 1)) ]
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
      Given [ RequestCreated (reqOctober1, DateTime(2019, 1, 1)) ]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (CancelRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Ok [RequestCanceled (reqOctober1, DateTime(2019, 1, 1))]) "The request should have been canceled by the manager."
    }

    test "A request is canceled by the employee" {
      Given [ RequestCreated (reqOctober1, DateTime(2019, 1, 1)) ]
      |> ConnectedAs (Employee "jdoe")
      |> WithToday (DateTime(2019, 1, 1))
      |> When (CancelRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Ok [RequestCanceled (reqOctober1, DateTime(2019, 1, 1))]) "The request should have been canceled by the employee."
    }

    test "A rejected request cannot be canceled" {
      Given [ RequestCreated (reqOctober1, DateTime(2019, 1, 1)) ; RequestRejected (reqOctober1, DateTime(2019, 1, 1))]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 1, 1))
      |> When (CancelRequest ("jdoe", reqOctober1.RequestId))
      |> Then (Error "Request cannot be canceled") "The request should have been canceled by the employee."
    }
  ]


[<Tests>]
let cancellationClaimsTests =
  testList "Cancellation claims tests" [
    test "An employee claims a cancellation (request in the past)" {
      Given [ RequestCreated (reqJanuaryFull, DateTime(2019, 12, 12)) ; RequestValidated (reqJanuaryFull, DateTime(2019, 12, 12)) ]
      |> ConnectedAs (Employee "jdoe")
      |> WithToday (DateTime(2019, 12, 12))
      |> When (CancelRequest ("jdoe", reqJanuaryFull.RequestId))
      |> Then (Ok [CancellationClaimed (reqJanuaryFull, DateTime(2019, 12, 12))]) "The request should have been in pending cancellation."
    }

    test "The manager reject a cancellation claim" {
      Given [ RequestCreated (reqJanuaryFull, DateTime(2019, 12, 12)) ; RequestValidated (reqJanuaryFull, DateTime(2019, 12, 12)) ; CancellationClaimed (reqJanuaryFull, DateTime(2019, 12, 12))]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 12, 12))
      |> When (RejectCancellationClaim ("jdoe", reqJanuaryFull.RequestId))
      |> Then (Ok [CancellationRejected (reqJanuaryFull, DateTime(2019, 12, 12))]) "The cancellation claim should have been rejected."
    }

    test "The manager accepts a cancellation claim" {
      Given [ RequestCreated (reqJanuaryFull, DateTime(2019, 12, 12)) ; RequestValidated (reqJanuaryFull, DateTime(2019, 12, 12)) ; CancellationClaimed (reqJanuaryFull, DateTime(2019, 12, 12))]
      |> ConnectedAs Manager
      |> WithToday (DateTime(2019, 12, 12))
      |> When (CancelRequest ("jdoe", reqJanuaryFull.RequestId))
      |> Then (Ok [RequestCanceled (reqJanuaryFull, DateTime(2019, 12, 12))]) "The cancellation claim should have been accepted."
    }
  ]

[<Tests>]
let getBusinessDaysTests =
  testList "getBusinessDaysTests" [
    test "1 week" {
      Expect.equal (Logic.getBusinessDays (DateTime(2019,12,2)) (DateTime(2019,12,8))) 5 "should be 5"
    }
    test "1 month with 1 holiday" {
      Expect.equal (Logic.getBusinessDays (DateTime(2019,11,25)) (DateTime(2020,1,5))) 29 "should be 29"
    }
    test "1 day" {
      Expect.equal (Logic.getBusinessDays (DateTime(2019,11,12)) (DateTime(2019,11,12))) 1 "should be 1"
    }
]


[<Tests>]
let timeOffDurationTests =
  testList "timeOffDurationTests" [
    test "1 month" {
      Expect.equal (Logic.timeOffDuration reqJanuaryFull) 22.0 "should be 22"
    }
    test "1 day" {
      Expect.equal (Logic.timeOffDuration reqOctober2) 1.0 "should be 1"
    }
    test "1 AM" {
      Expect.equal (Logic.timeOffDuration reqOctober1AM) 0.5 "should be 0.5"
    }
  ]

[<Tests>]
let timeOffDurationListTests =
  testList "timeOffDurationListTests" [
    test "1 TimeOff" {
      Expect.equal (Logic.timeOffDurationList [reqJanuaryFull]) 22.0 "should be 22"
    }
    test "2 TimeOff" {
      Expect.equal (Logic.timeOffDurationList [reqJanuaryFull; reqOctober2]) 23.0 "should be 23"
    }
  ]

[<Tests>]
let totalTimeOffTests =
  testList "totalTimeOffTests" [
    test "february 11" {
      Expect.equal (Logic.totalTimeOffUntilDate (DateTime(2019, 2, 11))) 2.08 "should be 2.08"
    }
    test "july 2" {
      Expect.equal (Logic.totalTimeOffUntilDate (DateTime(2019, 7, 2))) 12.48 "should be 12.48"
    }
  ]

[<Tests>]
let remainingInCompletedYearTests =
  testList "remainingInCompletedYearTests" [
    test "1 month + 2 day taken" {
      let result: float = Logic.remainingInCompletedYear [reqJanuaryFull2018; reqOctober2_2018; reqOctober3_2018] 2018
      Expect.equal (Math.Round(result, 2)) 0.96 "should be 0.96"
    }
    test "1 day + 1 day taken" {
      Expect.equal (Logic.remainingInCompletedYear [reqOctober2_2018; reqOctober3_2018] 2018) 22.96 "should be 22.96"
    }
]

[<Tests>]
let takenToDateTests =
  testList "takenToDateTests" [
    test "1 month taken" {
      Expect.equal (Logic.takenToDate [reqJanuaryFull] (DateTime(2019,12,12))) 22.0 "should be 22.0"
    }
    test "1 month taken with a day planned" {
      Expect.equal (Logic.takenToDate [reqJanuaryFull; reqOctober3] (DateTime(2019,9,12))) 22.0 "should be 22.0"
    }
]

[<Tests>]
let plannedTimeOffTests =
  testList "palannedTimeOffTests" [
    test "1 month planned" {
      Expect.equal (Logic.plannedTimeOff [reqOctoberFull] (DateTime(2019,1,1))) 23.0 "should be 23.0"
    }
    test "1 month planned with 1 month day taken" {
      Expect.equal (Logic.plannedTimeOff [reqOctoberFull; reqJanuaryFull] (DateTime(2019,5,12))) 23.0 "should be 23.0"
    }
]

[<Tests>]
let timeOffBalanceTests =
  testList "timeOffBalanceTests" [
    test "Cumulation of last year and current year time off"{
      Expect.equal (Logic.timeOffBalance [] (DateTime(2019,5,12))) 33.28 "Should be 33.28"
    }
    test "Full remained time off of last year + full january taken of the current year" {
      let result: float = Logic.timeOffBalance [reqJanuaryFull] (DateTime(2019,05,15))
      Expect.equal (Math.Round(result, 2)) 11.28 "Should be 11.28"
    }
  ]

[<Tests>]
let historyTests =
  testList "historyTests" [
    test "history 1 request of 1 day" {
      let result = Logic.getHistory [RequestCreated (reqOctober1, DateTime(2019, 1, 1))] (DateTime(2019,12,12))
      let expected = [ ("01/01/2019", "01/10/2019 AM", "01/10/2019 PM", 1.0, "New request") ]
      Expect.equal result expected "[('01/01/2019', '01/10/2019 AM', '01/10/2019 PM', 1.0, 'New request')]"
    }
  ]
