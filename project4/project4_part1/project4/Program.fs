open System
open Akka.FSharp
open System.Threading
open client
open server

//let system = System.create "system" (Configuration.defaultConfig())

//let normalDist = new Normal(50.0, 10.0) 


[<EntryPoint>]
let main argv =
    let get_message = argv.[0]
    if get_message = "test" then
        tweetDB.Add("test1", ("Inactive", ["First_tweet"; "Second_tweet#tag1";"Third_tweet@test2"]))    
        tweetDB.Add("test2", ("Inactive", ["First_tweet_2"; "Second_tweet_2#tag1";"Third_tweet_2@test1"]))
        tweetDB.Add("test3", ("Inactive", ["First_tweet_3"; "Second_tweet_3#tag1";"Third_tweet_2@test1"]))
        tweetDB.Add("test4", ("Inactive", ["First_tweet_4"; "Second_tweet_4#tag1";"Third_tweet_2@test1"])) 
        relationDB.Add("test1", ["test2"; "test3"])
        relationDB.Add("test2", ["test4"])
        
        printfn"*******************************************************************************************"
        printfn"Welcome Back! Please Login or Register to enter your account (input number only):"
        printfn"1.Register"
        printfn"2.Login"
        printfn"*******************************************************************************************"
  
        let mutable input = Console.ReadLine()
        let input2() =
            printfn"Please input your username:"
            let mutable u_name = Console.ReadLine()
            while(not(tweetDB.ContainsKey u_name)) do
                printfn"Username does not exist, please try again:"
                u_name <- Console.ReadLine()
            //let user = spawn system u_name Tweeter
            let user2=spawn system u_name Tweeter
            user2 <! Login u_name
            System.Console.Clear()
            printfn"***************************************************"
            printfn"Hello %s, welcome back!" u_name

            user2 <! Deliver u_name  //let ig = Deliver u_name

            //user2 <! Deliver u_name

            user2 <! Print u_name

            let mutable choice = Console.ReadLine()
            while(choice<>"8") do
                if choice = "1" then
                    printfn"Please enter your tweet:"
                    let tw_msg = Console.ReadLine()
                    System.Console.Clear()
                    user2<! Tweet tw_msg
                    user2 <! Deliver u_name
                if choice = "2" then
                    if relationDB.ContainsKey u_name = true then
                        System.Console.Clear()
                        printfn"***************************************************"
                        printfn"Below is your subscriptions: "
                        let all_sub = relationDB.[u_name]
                        for it in all_sub do
                            user2<!Query it
                    else
                        printfn"No subscription."
                        printfn" "
                if choice = "3" then
                    printfn"Please enter the tag(Start with #):"
                    let ta_msg = Console.ReadLine() 
                    System.Console.Clear()
                    printfn"***************************************************"
                    printfn"Below is the tweet(s) with %A: " ta_msg
                    user2<!Hashtag ta_msg
                if choice = "4" then
                    let tmp0 = "@" + u_name
                    System.Console.Clear()
                    printfn"***************************************************"
                    printfn"Below is the result(s) mentioned you: "
                    user2<!Mention tmp0
                if choice = "5" then
                    printfn"Please enter the username:"
                    let new_sub = Console.ReadLine()
                    if tweetDB.ContainsKey new_sub = true then
                        user2<!Subscribe new_sub
                        System.Console.Clear()
                        printfn"You subscribe %A successfully." new_sub 
                        printfn" "
                    else
                        System.Console.Clear()
                        printfn"***************************************************"
                        printfn"The username does not exist, please check that again!"
                        printfn"***************************************************"
                if choice = "6" then
                    printfn"Please enter the username:"
                    let un_sub = Console.ReadLine()
                    if tweetDB.ContainsKey un_sub = true then
                        user2<!Unsubscribe un_sub
                        System.Console.Clear()
                        printfn"You unsubscribe %A successfully." un_sub 
                        printfn" "
                    else
                        System.Console.Clear()
                        printfn"***************************************************"
                        printfn"The username does not exist, please check that again!"
                        printfn"***************************************************"
                if choice = "7" then
                    user2 <! Deliver u_name
                    printfn"Please enter full username to retweet their tweet(s):"
                    printfn" "
                    let re_user = Console.ReadLine()
                    if tweetDB.ContainsKey re_user = true then
                        let watch_list = snd (tweetDB.[re_user])     
                        if  watch_list.Length <> 0 then
                            for i = 1 to watch_list.Length do
                                //for each1 in snd (tweetDB.[wtc]) do
                                printfn"%d: %s" i watch_list.[i-1]
                        printfn"Please enter the number of the tweets that you want to retweet:"
                        let re_num = Console.ReadLine()
                        let re_tw = re_user + ": " + watch_list.[int(re_num) - 1]
                        user2<!Tweet re_tw
                        user2 <! Deliver u_name
                    else
//                        System.Console.Clear()
                        printfn"***************************************************"
                        printfn"Username does not exist, please try that again!"
                        printfn"***************************************************"

                user2 <! Print u_name
                choice <- Console.ReadLine()

            user2<!Logout u_name
            printfn"Bye."
        let mutable flag = 1
        while flag = 1 do
            if input = "1" then
                printfn"Please create your username(must contain letters)"
                let mutable reg_name = Console.ReadLine()
                while((tweetDB.ContainsKey reg_name)=true) do
                    printfn"This username has been used, please try another one:"
                    reg_name <- Console.ReadLine()
                let sys = reg_name + "ac"
                let user=spawn system sys Tweeter
                user <! Register reg_name
                System.Console.Clear()
                //Clear All
                printfn"Welcome, %s! Your account was successfully created." reg_name
                printfn" "
                printfn"Please select functions:"
                printfn"1.Register"
                printfn"2.Login"
                input <- Console.ReadLine()
            elif input  = "2" then
//                printfn"please enter your username to login:"
                input2()
                flag <- 0 
            else
                printfn"you can only input 1 or 2 to register to login"
                
    elif get_message = "simulate" then

        let simulator=spawn system "t1" Simulator
        Thread.Sleep(500)   
        simulator <! Simulate "1"
        Thread.Sleep(500)   
        let simulator5=spawn system "t6" Simulator
        Thread.Sleep(500)   
        simulator5 <! ReTweetLive "1"
        Thread.Sleep(500)   
        let simulator6=spawn system "t8" Simulator
        Thread.Sleep(500)   
        simulator6 <! FollowerRe "1"
        Thread.Sleep(500)   
        let simulator2=spawn system "t2" Simulator
        Thread.Sleep(500)   
        simulator2 <! LoginLive "1"
        Thread.Sleep(500)   
        let simulator4=spawn system "t5" Simulator
        Thread.Sleep(500)   
        simulator4 <! Report "1"
        
        printfn "Done!"
        stopWatch.Stop
        let result=stopWatch.Elapsed.TotalMilliseconds
        printfn "The total running time should be: %f" result

    else 
        printfn"Wrong input."

    0

