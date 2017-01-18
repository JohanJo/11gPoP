open System
open System.IO

type Loader (filename:string, n:int)= class
  let readLines (filename:string) = seq {
      use reader = new StreamReader (filename)
      while not reader.EndOfStream do
          yield reader.ReadLine ()
  }

  let mutable im1 = 0
  let mutable im2 = 0
  let tmp = readLines filename
  let mutable data = Seq.toList tmp
  let m1 = "$$SOE"
  let m2 = "$$EOE"

  member this.Array = data

  member this.pData =
    for i = 0 to data.Length-1 do
      if data.[i] = m1 then
        im1 <- i+1
      elif data.[i] = m2 then
        im2 <- i-1
    data <- data.[im1..im2]

    let mutable array = data
                        |> List.map (fun e -> e.Split ([|" "|], System.StringSplitOptions.RemoveEmptyEntries))
                        //|> List.map (Array.map decimal)

    //printfn "%s" "Hvor mange dage vil du have!??"
    //let n = System.Convert.ToInt32(System.Console.ReadLine());

    array <- array.[array.Length-n..array.Length-1]
    //(Array.get array.[0] 1)
    
    let mutable lon0 = (Array.get array.[0] 1) |> float
    let mutable lon1 = (Array.get array.[1] 1) |> float
    let mutable lat0 = (Array.get array.[0] 2) |> float
    let mutable lat1 = (Array.get array.[1] 2) |> float
    let mutable rad0 = (Array.get array.[0] 3) |> float
    let mutable rad1 = (Array.get array.[1] 3) |> float
        
    printfn "%A" lon0
    printfn "%A" lon1
    printfn "%A" lat0
    printfn "%A" lat1
    printfn "%A" rad0
    printfn "%A" rad1
end

let dulle = new Loader("C:\Users\Jebiel\Documents\Opgave 11g\data\Earth.txt", 5)
let mulle = new Loader("C:\Users\Jebiel\Documents\Opgave 11g\data\Venus.txt", 5)
dulle.pData
mulle.pData
