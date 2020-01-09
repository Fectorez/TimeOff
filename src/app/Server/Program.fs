module ServerCode.App

open TimeOff
open Storage.Events

open System
open System.IO
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Cors.Infrastructure
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Giraffe.HttpStatusCodeHandlers.RequestErrors
open FSharp.Control.Tasks
open Newtonsoft.Json.Linq
open TimeOff.Logic

// ---------------------------------
// Handlers
// ---------------------------------

module HttpHandlers =

    open Microsoft.AspNetCore.Http

    let getRequestsForUser (eventStore: IStore<UserId, RequestEvent>) (userName: string) =
      fun (next: HttpFunc) (ctx: HttpContext) ->
        task {
            let eventStream = eventStore.GetStream(userName)
            let state = eventStream.ReadAll()
            return! Successful.OK state next ctx
        }
            
    let requestTimeOff (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let! timeOffRequest = ctx.BindJsonAsync<TimeOffRequest>()
                let command = RequestTimeOff timeOffRequest
                let result = handleCommand command
                match result with
                | Ok _ -> return! json timeOffRequest next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }

    let validateRequest (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let! userAndRequestId = ctx.BindJsonAsync<UserAndRequestId>()
                let command = ValidateRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [RequestValidated (timeOffRequest, today)] -> return! json (timeOffRequest, today) next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }
            
    let cancelRequest (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let! userAndRequestId = ctx.BindJsonAsync<UserAndRequestId>()
                let command = CancelRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [RequestCanceled (cancelRequest, today)] -> return! json (cancelRequest, today) next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }
    
    let rejectRequest (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let! userAndRequestId = ctx.BindJsonAsync<UserAndRequestId>()
                let command = RejectRequest (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [RequestRejected (rejectRequest, today)] -> return! json (rejectRequest, today) next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }
            
    let rejectCancellationClaim (handleCommand: Command -> Result<RequestEvent list, string>) =
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let! userAndRequestId = ctx.BindJsonAsync<UserAndRequestId>()
                let command = RejectCancellationClaim (userAndRequestId.UserId, userAndRequestId.RequestId)
                let result = handleCommand command
                match result with
                | Ok [CancellationRejected (rejectRequest, today)] -> return! json (rejectRequest, today) next ctx
                | Ok _ -> return! Successful.NO_CONTENT next ctx
                | Error message ->
                    return! (BAD_REQUEST message) next ctx
            }
    
    // Get current Balance
    let getCurrentBalance (userRequests: Map<Guid, RequestState>) (requestsEvents: seq<RequestEvent>)=
        let getRequestTimeOffFromResquestState (requests: list<TimeOffRequest>) (request: RequestState) =
            match request with
            | PendingValidation  timeOffRequest -> timeOffRequest::requests
            | Validated timeOffRequest -> timeOffRequest::requests
            | Canceled  timeOffRequest -> timeOffRequest::requests
            | Rejected  timeOffRequest -> timeOffRequest::requests
            | RejectedCancellation  timeOffRequest -> timeOffRequest::requests
            | PendingCancellation  timeOffRequest -> timeOffRequest::requests
            | _ -> requests
            
        fun (next: HttpFunc) (ctx: HttpContext) ->
            task {
                let today = DateTime.Today
                let requests =  Map.fold (fun list guid requestStats -> getRequestTimeOffFromResquestState list requestStats) list.Empty userRequests
                let allotmentAccruedToDate = Logic.totalTimeOffUntilDate today
                let carriedOverFromLastYear = Logic.remainingInCompletedYear  requests today.Year
                let takenToDate = Logic.takenToDate requests today
                let planned = Logic.plannedTimeOff requests today
                let currentBalance = allotmentAccruedToDate + carriedOverFromLastYear - takenToDate - planned
                let history =
                    Logic.getHistory requestsEvents today
                    |> List.map (fun (date, from, to_, nb, msg) -> JObject [
                        JProperty("date", date)
                        JProperty("from", from)
                        JProperty("to", to_)
                        JProperty("days", nb)
                        JProperty("event", msg)
                    ])
                
                let jsonResponse = JObject [
                    JProperty("allotmentAccruedToDate", allotmentAccruedToDate)
                    JProperty("carriedOverFrom", carriedOverFromLastYear)
                    JProperty("takenToDate", takenToDate)
                    JProperty("planned", planned)
                    JProperty("currentBalance", currentBalance)
                    JProperty("history", history)
                ]
                
                return! Successful.OK jsonResponse next ctx
            } 
        
    
    
    
// ---------------------------------
// Web app
// ---------------------------------

let webApp (eventStore: IStore<UserId, RequestEvent>) =
    let handleCommand (user: User) (command: Command) =
        let userId = command.UserId

        let eventStream = eventStore.GetStream(userId)
        let state = eventStream.ReadAll() |> Seq.fold Logic.evolveUserRequests Map.empty

        // Decide how to handle the command
        let result = Logic.decide state user command DateTime.Today

        // Save events in case of success
        match result with
        | Ok events -> eventStream.Append(events)
        | _ -> ()

        // Finally, return the result
        result
    
    let getRequestsEvents (user: User) =
        match user with
        | Employee userId ->
            let eventStream = eventStore.GetStream(userId)
            eventStream.ReadAll()
        | _ -> Seq.empty

    let getUserRequests (user: User) =
        match user with
        | Employee userId ->
            let eventStream = eventStore.GetStream(userId)
            eventStream.ReadAll() |> Seq.fold Logic.evolveUserRequests Map.empty
        | _ -> Map.empty
            
    choose [
        subRoute "/api"
            (choose [
                route "/users/login" >=> POST >=> Auth.Handlers.login
                subRoute "/timeoff"
                    (Auth.Handlers.requiresJwtTokenForAPI (fun identity ->
                        choose [
                            GET >=> route "/requests" >=> HttpHandlers.getRequestsForUser eventStore identity.UserName
                            POST >=> route "/request" >=> HttpHandlers.requestTimeOff (handleCommand identity.User)
                            POST >=> route "/validateRequest" >=> HttpHandlers.validateRequest (handleCommand identity.User)
                            POST >=> route "/cancelRequest" >=> HttpHandlers.cancelRequest (handleCommand identity.User)
                            POST >=> route "/rejectRequest" >=> HttpHandlers.rejectRequest (handleCommand identity.User)
                            POST >=> route "/rejectCancellationClaim" >=> HttpHandlers.rejectCancellationClaim (handleCommand identity.User)
                            GET >=> route "/currentBalance" >=> HttpHandlers.getCurrentBalance (getUserRequests identity.User) (getRequestsEvents identity.User)
                        ]
                    ))
            ])
        setStatusCode 404 >=> text "Not Found" ]

// ---------------------------------
// Error handler
// ---------------------------------

let errorHandler (ex: Exception) (logger: ILogger) =
    logger.LogError(EventId(), ex, "An unhandled exception has occurred while executing the request.")
    clearResponse >=> setStatusCode 500 >=> text ex.Message

// ---------------------------------
// Config and Main
// ---------------------------------

let configureCors (builder: CorsPolicyBuilder) =
    builder.WithOrigins("http://localhost:8080")
           .AllowAnyMethod()
           .AllowAnyHeader()
           |> ignore

let configureApp (eventStore: IStore<UserId, RequestEvent>) (app: IApplicationBuilder) =
    let webApp = webApp eventStore
    let env = app.ApplicationServices.GetService<IHostingEnvironment>()
    (match env.IsDevelopment() with
    | true -> app.UseDeveloperExceptionPage()
    | false -> app.UseGiraffeErrorHandler errorHandler)
        .UseCors(configureCors)
        .UseStaticFiles()
        .UseGiraffe(webApp)

let configureServices (services: IServiceCollection) =
    services.AddCors() |> ignore
    services.AddGiraffe() |> ignore

let configureLogging (builder: ILoggingBuilder) =
    let filter (l: LogLevel) = l.Equals LogLevel.Error
    builder.AddFilter(filter).AddConsole().AddDebug() |> ignore

[<EntryPoint>]
let main _ =
    let contentRoot = Directory.GetCurrentDirectory()

    //let eventStore = InMemoryStore.Create<UserId, RequestEvent>()
    let storagePath = System.IO.Path.Combine(contentRoot, "../../../.storage", "userRequests")
    let eventStore = FileSystemStore.Create<UserId, RequestEvent>(storagePath, sprintf "%s")

    let webRoot = Path.Combine(contentRoot, "WebRoot")
    WebHostBuilder()
        .UseKestrel()
        .UseContentRoot(contentRoot)
        .UseIISIntegration()
        .UseWebRoot(webRoot)
        .Configure(Action<IApplicationBuilder>(configureApp eventStore))
        .ConfigureServices(configureServices)
        .ConfigureLogging(configureLogging)
        .Build()
        .Run()
    0