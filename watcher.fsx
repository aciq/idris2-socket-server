open System.IO
open System
open System.Diagnostics

let watcher = new FileSystemWatcher(".","*.idr",EnableRaisingEvents=true, IncludeSubdirectories=true)

let p = 
    new Process(StartInfo=ProcessStartInfo(
            fileName="idris2",
            arguments="-p network -p contrib ./Main.idr",
            WorkingDirectory= __SOURCE_DIRECTORY__ ,
            RedirectStandardInput=true
        )
    )
       
p.Start()

let rec loop (nextproctime:DateTimeOffset) : unit =
    let _ = watcher.WaitForChanged(WatcherChangeTypes.Changed)
    match DateTimeOffset.Now > nextproctime with 
    | false -> loop (nextproctime) 
    | true -> 
        p.StandardInput.WriteLine(":r")
        loop (nextproctime.AddSeconds(2))   

let consoleStream = Console.OpenStandardInput()
consoleStream.CopyToAsync(p.StandardInput.BaseStream)

loop DateTimeOffset.Now
