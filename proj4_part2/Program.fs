open Suave
open Suave.Http
open Suave.Operators
open Suave.Filters
open Suave.Successful
open Suave.Files
open Suave.RequestErrors
open Suave.Logging
open Suave.Utils
open System
open System.Net
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket
open Newtonsoft.Json
open FSharp.Json
open System.Collections.Generic



let userDB = new Dictionary<string, string>()
let tweetDB = new Dictionary<string, (string * list<string>) >()
let relationDB = new Dictionary<string, list<string>>()

type tweet_now = 
    { messagetype: string
      name : string
      content : string }
    
let ws (webSocket : WebSocket) (context: HttpContext)=
    socket {
        let mutable loop = true
        while loop do
            let! msg = webSocket.read()
            match msg with
            | (Text, data, true) ->
                let str = UTF8.toString data
                let tweet_msg = Json.deserialize<tweet_now> str
                let mType=tweet_msg.messagetype
                
                if mType="tweet" then
                    let temp=snd (tweetDB.Item(tweet_msg.name))
                    let temp=temp @ [tweet_msg.content]
                    tweetDB.Item(tweet_msg.name) <- ("Active", temp)
                    let response:tweet_now = {
                        messagetype= "tweet"
                        name = tweet_msg.name
                        content = tweet_msg.name + " : " + tweet_msg.content
                    }
                    let json = Json.serialize response
                    let byteResponse =
                      json
                      |> System.Text.Encoding.ASCII.GetBytes
                      |> ByteSegment
                    do! webSocket.send Text byteResponse true
                                                                                                              
                    printfn "uploaded into the database from user %s" tweet_msg.name
                elif mType="register" then
                    if tweetDB.ContainsKey tweet_msg.name then
                        printfn "user register failed: duplicated name"
                        let response:tweet_now = {
                            messagetype = "register";
                            name = tweet_msg.name;
                            content = "user already registered, please try a different name"
                        }
                        let json = Json.serialize response
                        let byteResponse =
                          json
                          |> System.Text.Encoding.ASCII.GetBytes
                          |> ByteSegment
                        do! webSocket.send Text byteResponse true
                    else
                        tweetDB.Add(tweet_msg.name, ("Inactive",[]))
                        printfn "user %s registered" tweet_msg.name
                        relationDB.Add(tweet_msg.name, [])
                        userDB.Add(tweet_msg.name, tweet_msg.content)

                        let response:tweet_now = {
                            messagetype = "register";
                            name = tweet_msg.name;
                            content = "user " + tweet_msg.name + " registered"
                        }
                        let json = Json.serialize response
                        let byteResponse =
                          json
                          |> System.Text.Encoding.ASCII.GetBytes
                          |> ByteSegment
                        do! webSocket.send Text byteResponse true

                elif mType="login" then
                    if tweetDB.ContainsKey tweet_msg.name then
                        if tweet_msg.content = userDB.[tweet_msg.name] then
                            let response:tweet_now = {
                                messagetype = "register";
                                name = tweet_msg.name;
                                content = "user " + tweet_msg.name + " logged in"
                            }
                            let json = Json.serialize response
                            let byteResponse =
                              json
                              |> System.Text.Encoding.ASCII.GetBytes
                              |> ByteSegment
                            do! webSocket.send Text byteResponse true

                            let temp=snd (tweetDB.Item(tweet_msg.name))
                            tweetDB.Item(tweet_msg.name) <- ("Active", temp)
                            printfn "user %s login successfully" tweet_msg.name
                            if relationDB.[tweet_msg.name] <> [] then 
                                let watch_list = relationDB.[tweet_msg.name]        
                                for wtc in watch_list do
                                    for each1 in snd (tweetDB.[wtc]) do
                                        let response:tweet_now= {
                                            messagetype= "tweet"
                                            name = tweet_msg.name
                                            content = wtc + " : " + each1
                                        }
                                        let json = Json.serialize response
                                        let byteResponse =
                                            json
                                            |> System.Text.Encoding.ASCII.GetBytes
                                            |> ByteSegment
                                        do! webSocket.send Text byteResponse true
                                        printfn"%s: %s" wtc each1
                            for each2 in (snd (tweetDB.[tweet_msg.name])) do
                                let response:tweet_now = {
                                    messagetype= "tweet"
                                    name = tweet_msg.name
                                    content = tweet_msg.name + " : " + each2
                                }
                                let json = Json.serialize response
                                let byteResponse =
                                    json
                                    |> System.Text.Encoding.ASCII.GetBytes
                                    |> ByteSegment
                                do! webSocket.send Text byteResponse true
                                printfn"%s: %s" tweet_msg.name each2
                        else
                            printfn "login failed: incorrect username or password"
                            let response: tweet_now = {
                                messagetype= "login"
                                name = "NA"
                                content = "incorrect username or password"
                            }
                            let json = Json.serialize response
                            let byteResponse =
                              json
                              |> System.Text.Encoding.ASCII.GetBytes
                              |> ByteSegment
                            do! webSocket.send Text byteResponse true
                    else
                        printfn "login failed: no user found"
                        let response:tweet_now = {
                            messagetype = "login";
                            name = "Error";
                            content = "user does not exist, try again"
                        }
                        let json = Json.serialize response
                        let byteResponse =
                          json
                          |> System.Text.Encoding.ASCII.GetBytes
                          |> ByteSegment
                        do! webSocket.send Text byteResponse true
                elif mType= "subscribe" then
                    if tweetDB.ContainsKey tweet_msg.name then
                        let temp=relationDB.Item(tweet_msg.name)
                        let temp=temp @ [tweet_msg.content]
                        relationDB.Item(tweet_msg.name) <- temp
                        printfn "user %s subscribed" tweet_msg.content
                        let response:tweet_now = {
                            messagetype = "subscribe";
                            name = tweet_msg.name;
                            content = "subsribed to " + tweet_msg.name
                        }
                        let json = Json.serialize response
                        let byteResponse =
                          json
                          |> System.Text.Encoding.ASCII.GetBytes
                          |> ByteSegment
                        do! webSocket.send Text byteResponse true
                    else 
                        printfn "subsribe failed: no user name found"
                        let response:tweet_now = {
                            messagetype = "subscribe";
                            name = "error";
                            content = "subsribe failed: no user " + tweet_msg.name + " found"
                        }
                        let json = Json.serialize response
                        let byteResponse =
                          json
                          |> System.Text.Encoding.ASCII.GetBytes
                          |> ByteSegment
                        do! webSocket.send Text byteResponse true
                elif mType= "search" then
                    
                    for name in tweetDB.Keys do
                        for each in (snd (tweetDB.Item(name)) ) do
                            if (each.Contains tweet_msg.content) then 
                                let response:tweet_now= {
                                    messagetype= "search"
                                    name = tweet_msg.name
                                    content = name + " : " + each
                                }
                                let json = Json.serialize response
                                let byteResponse =
                                  json
                                  |> System.Text.Encoding.ASCII.GetBytes
                                  |> ByteSegment
                                do! webSocket.send Text byteResponse true
                                printfn "%s" each
                    printfn "search %s successfully" (tweet_msg.content)                   
            | (Close, _, _) ->
                let emptyResponse = [||] |> ByteSegment
                do! webSocket.send Close emptyResponse true
                loop <- false

            | _ -> ()
      }      

let app : WebPart =
    choose [
        path "/websocket" >=> handShake ws
        GET >=> choose [ path "/" >=> file "index.html"; browseHome ]
        NOT_FOUND "Found no handlers." ]

[<EntryPoint>]
let main _ =
  startWebServer { defaultConfig with logger = Targets.create Verbose [||] } app
  0
