open System
open System.IO

let filename = "C:\Users\Jebiel\Documents\Opgave 11g\data\Earth.txt"
//data.Split ([|" "|], System.StringSplitOptions.RemoveEmptyEntries)

let readLines (filename:string) = seq {
    use reader = new StreamReader (filename)
    while not reader.EndOfStream do
        yield reader.ReadLine ()
}

let data = readLines filename

let data1 = Seq.toList data

let mutable im1 = 0
let mutable im2 = 0
let m1 = "$$SOE"
let m2 = "$$EOE"

for i = 0 to data1.Length-1 do
  if data1.[i] = m1 then
    im1 <- i+1
  elif data1.[i] = m2 then
    im2 <- i-1

printfn "%A" data1.[im1..im2]

let splitlines = data1.Split ([|""|], System.StringSplitOptions.RemoveEmptyEntries)
splitlines |>
printfn "%A" 
(**
let im1 = 0
let im2 = 0

for i = 0 to i<data.length do
  if linelist.[i] = m1 then
    im1 <- i
  elif lineList.[i] = m2 then
    im2 <- i

reader.close();




//NOTER

//Huske at reverse lineList da vi conser
while not reader.end do
  let line = reader.line

  lineList :: line
**)
