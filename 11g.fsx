open System
open System.IO
open System.Windows.Forms
open System.Drawing
open Microsoft.FSharp.Control.CommonExtensions

// dialog
//printfn "Please choose how many days to simulate"
let mutable daysInt = 0
let mutable days = 0

let form = new Form(Width=300, Height=200)

let label = new Label(Text = "Hvor mange dage vil du simulere.", Dock = DockStyle.Top, Left=130)
form.Controls.Add(label)

let textBox = new TextBox(Left = 110, Top=50, Width=50)
form.Controls.Add(textBox)

let button = new Button(Text = "Start Simulering!", Dock = DockStyle.Bottom)
button.Click.Add(fun _ ->
    //Her skal den istedet sætte n(dage)
    daysInt <- textBox.Text |> int32
    days <- textBox.Text |> int32
    form.Close())
form.Controls.Add(button)

Application.Run(form)

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
  let mutable array = []

  member this.Array = array

  member this.pData =
    for i = 0 to data.Length-1 do
      if data.[i] = m1 then
        im1 <- i+1
      elif data.[i] = m2 then
        im2 <- i-1
    data <- data.[im1..im2]
    array <- data |> List.map (fun e -> e.Split ([|" "|], System.StringSplitOptions.RemoveEmptyEntries))

    array <- array.[array.Length-n..array.Length-1]

  member this.lon0 = (Array.get array.[0] 1) |> float
  member this.lon1 = (Array.get array.[1] 1) |> float
  member this.lat0 = (Array.get array.[0] 2) |> float
  member this.lat1 = (Array.get array.[1] 2) |> float
  member this.rad0 = (Array.get array.[0] 3) |> float
  member this.rad1 = (Array.get array.[1] 3) |> float
end

// load filerne med positionsdata
let mercuryData = new Loader("data/Mercury.txt", daysInt)
mercuryData.pData
let venusData = new Loader("data/Venus.txt", daysInt)
venusData.pData
let earthData = new Loader("data/Earth.txt", daysInt)
earthData.pData
let marsData = new Loader("data/Mars.txt", daysInt)
marsData.pData
let saturnData = new Loader("data/Saturn.txt", daysInt)
saturnData.pData
let uranusData = new Loader("data/Uranus.txt", daysInt)
uranusData.pData
let jupiterData = new Loader("data/Jupiter.txt", daysInt)
jupiterData.pData
let neptuneData = new Loader("data/Neptune.txt", daysInt)
neptuneData.pData
let plutoData = new Loader("data/Pluto.txt", daysInt)
plutoData.pData


let G = 67384e-11
let GMsol = 2.959122082322128e-4
let deltaT = 1
let AU = float(1495978707)
let deg2rad deg =
  deg * (System.Math.PI / 180.0)
type Position = {mutable x : float; mutable y : float; mutable z : float}
type Brush = Color * int
//type Point = float * float
//type Coordinate = {mutable x : float; mutable y : float;}
let rnd = new Random()
type Planet (Lon0, Lat0, Rad0, Lon1, Lat1, Rad1) = class
  let myBrush = new SolidBrush(Color.FromArgb(rnd.Next(256), rnd.Next(256), rnd.Next(256)))
  //startposition
  let x0 = Rad0 * cos(deg2rad Lat0) * cos(deg2rad Lon0)
  let y0 = Rad0 * cos(deg2rad Lat0) * sin(deg2rad Lon0)
  let z0 = Rad0 * sin(deg2rad Lat0)
  //gør posistionen om til en mutable
  let mutable _pos = {x = x0;
                      y = y0;
                      z = z0}
  //finder den næste position
  let x1 = Rad1 * cos(deg2rad Lat1) * cos(deg2rad Lon1)
  let y1 = Rad1 * cos(deg2rad Lat1) * sin(deg2rad Lon1)
  let z1 = Rad1 * sin(deg2rad Lat1)
  //dens startaccerelation. her vil solen(GMsol) trække lidt i planeten så den kører i en cirkel
  let ax0 = - GMsol / (Rad0 * Rad0 * Rad0) * x0
  let ay0 = - GMsol / (Rad0 * Rad0 * Rad0) * y0
  let az0 = - GMsol / (Rad0 * Rad0 * Rad0) * z0
  //hastigheden fundet ved at gange de to positioner
  let vx0 = (x1 - x0) / float(deltaT - 0)
  let vy0 = (y1 - y0) / float(deltaT - 0)
  let vz0 = (z1 - z0) / float(deltaT - 0)
  //start positionen
  let rx0 = x0
  let ry0 = y0
  let rz0 = z0
  //ny position fundet med vores hastighed * deltaT(1)
  let rx1 = rx0 + vx0 * float(deltaT)
  let ry1 = ry0 + vy0 * float(deltaT)
  let rz1 = rz0 + vz0 * float(deltaT)
  //den nye hastighed
  let vx1 = vx0 + ax0 * float(deltaT)
  let vy1 = vy0 + ay0 * float(deltaT)
  let vz1 = vz0 + az0 * float(deltaT)
  //den nye acceleration
  let ax1 = - GMsol / (Rad1 * Rad1 * Rad1) * rx1
  let ay1 = - GMsol / (Rad1 * Rad1 * Rad1) * ry1
  let az1 = - GMsol / (Rad1 * Rad1 * Rad1) * rz1
  //gør værdierne mutable når jeg skal overskrive dem i min løkke
  let mutable rx = rx1 + vx1 * float(deltaT)
  let mutable ry = ry1 + vy1 * float(deltaT)
  let mutable rz = rz1 + vz1 * float(deltaT)
  let mutable rsim = sqrt(rx1*rx1+ry1*ry1+rz1*rz1)
  let mutable ax = - GMsol / (rsim * rsim * rsim) * rx
  let mutable ay = - GMsol / (rsim * rsim * rsim) * ry
  let mutable az = - GMsol / (rsim * rsim * rsim) * rz
  let mutable vx = vx1 + ax1 * float(deltaT)
  let mutable vy = vy1 + ay1 * float(deltaT)
  let mutable vz = vz1 + az1 * float(deltaT)
  //starter med dag = 0
  let mutable dag = 0
  //en tom liste til at indæstte planetens positioner
  let mutable _listpos = List.Empty
  //attributes
  member this.r = rnd.Next(256)
  member this.g = rnd.Next(256)
  member this.b = rnd.Next(256)

  member this.RandomColor = myBrush

  member this.AU2Pixel AU = (float (fst (800, 600)) / (2.0*33.0)) * AU + (float(fst (800, 600)) / 2.0)

  member this.Pos2AUpos pos = {Position.x=this.AU2Pixel(pos.x); Position.y=this.AU2Pixel(pos.y);Position.z=this.AU2Pixel(pos.z)}

  member this.listpos = _listpos
  //methods
  //bevæger planeten
  member this.Move () =
      _pos <- {x = rx
               y = ry
               z = rz }
      ax <- - GMsol / (rsim * rsim * rsim) * rx
      ay <- - GMsol / (rsim * rsim * rsim) * ry
      az <- - GMsol / (rsim * rsim * rsim) * rz
      vx <- vx + ax * float(deltaT)
      vy <- vy + ay * float(deltaT)
      vz <- vz + az * float(deltaT)
      rx <- rx + vx * float(deltaT)
      ry <- ry + vy * float(deltaT)
      rz <- rz + vz * float(deltaT)
      rsim <- sqrt(rx*rx+ry*ry+rz*rz)
  //simulerer planeten med x antal dage
  member this.SimPlanet (dage:int) =
    while dag <= dage do
      _listpos <- _listpos @ [_pos]
      this.Move()
      dag <- dag + deltaT

  member this.GetAU2Pixels = this.listpos |> List.map (fun {Position.x=x;Position.y=y;Position.z=z} -> {Position.x=this.AU2Pixel(x); Position.y=this.AU2Pixel(y);Position.z=this.AU2Pixel(z)})
end

// opret objekter
let earth = new Planet(earthData.lon0, earthData.lat0, earthData.rad0, earthData.lon1, earthData.lat1, earthData.rad1)
let sun = new Planet(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
let neptune = new Planet(neptuneData.lon0, neptuneData.lat0, neptuneData.rad0, neptuneData.lon1, neptuneData.lat1, neptuneData.rad1)
let venus = new Planet(venusData.lon0, venusData.lat0, venusData.rad0, venusData.lon1, venusData.lat1, venusData.rad1)
let jupiter = new Planet(jupiterData.lon0, jupiterData.lat0, jupiterData.rad0, jupiterData.lon1, jupiterData.lat1, jupiterData.rad1)
let mars = new Planet(marsData.lon0, marsData.lat0, marsData.rad0, marsData.lon1, marsData.lat1, marsData.rad1)
let mercury = new Planet(mercuryData.lon0, mercuryData.lat0, mercuryData.rad0, mercuryData.lon1, mercuryData.lat1, mercuryData.rad1)
let pluto = new Planet(plutoData.lon0, plutoData.lat0, plutoData.rad0, plutoData.lon1, plutoData.lat1, plutoData.rad1)
let saturn = new Planet(saturnData.lon0, saturnData.lat0, saturnData.rad0, saturnData.lon1, saturnData.lat1, saturnData.rad1)
let uranus = new Planet(uranusData.lon0, uranusData.lat0, uranusData.rad0, uranusData.lon1, uranusData.lat1, uranusData.rad1)
let planets =
  [earth; neptune; venus; jupiter; mars; mercury; pluto; saturn; uranus]
List.map (fun (p : Planet)-> printfn "farve: %A" p.RandomColor.Color) planets
// simuler planeter
sun.SimPlanet (days)
earth.SimPlanet (days)
neptune.SimPlanet (days)
venus.SimPlanet (days)
saturn.SimPlanet (days)
mars.SimPlanet (days)
mercury.SimPlanet (days)
jupiter.SimPlanet (days)
pluto.SimPlanet (days)
uranus.SimPlanet (days)
;;

type Animation (planets : List<Planet>) = class
  let title = "Milkyway"
  let backColor = Image.FromFile("Star_background.png")
  let size = (800, 600)
  let mutable positions = List.fold (fun acc (p:Planet) -> List.append acc p.listpos) List.empty<Position> planets
  let createForm backColor (width, height) title draw =
    let win = new Form ()
    win.Text <- title
    win.BackgroundImage <- backColor
    win.ClientSize <- Size (width, height)
    //List.map (fun (p : Planet) -> List.map (fun pos -> win.Paint.Add (fun (e : PaintEventArgs) -> draw (2, pos, e)))p.GetAU2Pixels) planets |> ignore
    for i in 0 .. days-1 do
      List.map (fun (p : Planet) -> win.Paint.Add (fun (e : PaintEventArgs) -> draw (p, p.Pos2AUpos p.listpos.[i], e, i))) planets |> ignore
    win


  let drawUni (p : Planet, pos : Position, e : PaintEventArgs, day : int) =
    //For prolonged animation use Thread.Sleep(int)
    //System.Threading.Thread.Sleep(2)
    e.Graphics.FillEllipse(p.RandomColor, int(pos.x), int(pos.y), 2 ,2)
    printfn "day %d of %d" day days
    //new Brush()

  member this.create() =
    let win = createForm backColor size title drawUni
    Application.EnableVisualStyles()
    Application.Run win

end
;;

let solardraw = new Animation (planets)

solardraw.create()
