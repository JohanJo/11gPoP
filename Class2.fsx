let G = 67384e-11
let GMsol = 2.959122082322128e-4
let deltaT = 1.0

let deg2rad deg =
  deg * (System.Math.PI / 180.0)
type Position = {mutable x : float; mutable y : float; mutable z : float}
type planet (Lon0, Lat0, Rad0, Lon1, Lat1, Rad1) = class
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
      let vx0 = (x1 - x0) / (deltaT - 0.0)
      let vy0 = (y1 - y0) / (deltaT - 0.0)
      let vz0 = (z1 - z0) / (deltaT - 0.0)
      //start positionen
      let rx0 = x0
      let ry0 = y0
      let rz0 = z0
      //ny position fundet med vores hastighed * deltaT(1)
      let rx1 = rx0 + vx0 * deltaT
      let ry1 = ry0 + vy0 * deltaT
      let rz1 = rz0 + vz0 * deltaT
      //den nye hastighed
      let vx1 = vx0 + ax0 * deltaT
      let vy1 = vy0 + ay0 * deltaT
      let vz1 = vz0 + az0 * deltaT
      //den nye acceleration
      let ax1 = - GMsol / (Rad1 * Rad1 * Rad1) * rx1
      let ay1 = - GMsol / (Rad1 * Rad1 * Rad1) * ry1
      let az1 = - GMsol / (Rad1 * Rad1 * Rad1) * rz1
      //gør værdierne mutable når jeg skal overskrive dem i min løkke
      let mutable rx = rx1 + vx1 * deltaT
      let mutable ry = ry1 + vy1 * deltaT
      let mutable rz = rz1 + vz1 * deltaT
      let mutable rsim = sqrt(rx1*rx1+ry1*ry1+rz1*rz1)
      let mutable ax = - GMsol / (rsim * rsim * rsim) * rx
      let mutable ay = - GMsol / (rsim * rsim * rsim) * ry
      let mutable az = - GMsol / (rsim * rsim * rsim) * rz
      let mutable vx = vx1 + ax1 * deltaT
      let mutable vy = vy1 + ay1 * deltaT
      let mutable vz = vz1 + az1 * deltaT
      //starter med dag = 0
      let mutable dag = 0.0
      //en tom liste til at indæstte planetens positioner
      let mutable _listpos = List.Empty
      //attributes
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
          vx <- vx + ax * deltaT
          vy <- vy + ay * deltaT
          vz <- vz + az * deltaT
          rx <- rx + vx * deltaT
          ry <- ry + vy * deltaT
          rz <- rz + vz * deltaT
          rsim <- sqrt(rx*rx+ry*ry+rz*rz)
      //simulerer planeten med x antal dage
      member this.SimPlanet (dage:float) =
        while dag <= dage do
          _listpos <- _listpos @ [_pos]
          this.Move()
          dag <- dag + deltaT
    end
let earth = new planet(100.6001, 0.0044, 0.983295949009, 101.6192, 0.0044 ,0.983281670434)
earth.SimPlanet (365.0)
printfn "%A" (earth.listpos.[0])
printfn "%A" (earth.listpos.[365])
