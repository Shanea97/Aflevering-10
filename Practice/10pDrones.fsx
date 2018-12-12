type Drone (x,y, speed:float,  c,z) = 
    let mutable Pos = (x,y) // Actual position / Letting x and y be given by meters
    let Speed = speed // Meters pr. Minute
    let Destination = (c,z)  // Final destination
    member this.Position = Pos 
    member this.Fly = Pos <- ((fst Pos + Speed), (snd Pos + Speed))
    member this.isFinished = 
            if Pos = Destination then //Checks if the drone have reached the destination
                true 
            else 
                false 

type Airspace (drones: Drone list) = 
    let mutable dronesList = drones
    member this.DronesPosition = 
        for i = 0 to dronesList.Length-1 do 
            printfn "Drone %A's current location: %A" i dronesList.[i].Position
    member this.droneDist = 
        for i = 0 to dronesList.Length-1 do 
            printfn " "
            for j = 0 to dronesList.Length-1 do 
                let dron1 = dronesList.[j].Position 
                let dron2 = dronesList.[i].Position
                let distance = (sqrt(((fst dron2-fst dron1)**2.0)+((snd dron2-snd dron1)**2.0))) 
                printfn "Drone %A's distance to drone %A: %A" i j distance
    member this.flyDrones(minutes) = 
        for i = 0 to dronesList.Length-1 do
            for j = 1 to minutes do  
                dronesList.[i].Fly
            printfn "Drone %A's  position After 1 minute of flying: %A" i dronesList.[i].Position
    member this.AddDrone(drone) = dronesList <- dronesList @ drone  // DronesList gets updated with a new drone (This has to be given by a Drone list)
    member this.willCollide = 
        for i = 0 to dronesList.Length-1 do 
            printfn " "
            for j = 0 to dronesList.Length-1 do 
                let dron1 = dronesList.[j].Position 
                let dron2 = dronesList.[i].Position
                let distance = (sqrt(((fst dron2-fst dron1)**2.0)+((snd dron2-snd dron1)**2.0))) 
                printfn "List without collided drones %A" dronesList |> List.filter (fun x -> distance > 5.0) 


let TestDrone = [Drone(5.0,7.5,2.5,30.0,30.0)]
let NewDrone = Drone(10.0,15.0,4.0,22.0,27.0)
let AwsomeDrone = Drone(14.0,18.0,2.0,30.0,34.0)
let DroneList = Airspace([Drone(10.0,15.0,4.0,22.0,27.0);Drone(14.0,18.0,2.0,30.0,34.0);Drone(8.0,4.0,3.0,14.0,10.0);Drone(22.0,23.0,7.0,43.0,44.0)])
printfn "NewDrone Position Start: %A" NewDrone.Position
NewDrone.Fly
printfn "NewDrone Fly 1st flight: %A" NewDrone.Position
NewDrone.Fly 
printfn "Newdrone Fly 2nd fligth: %A" NewDrone.Position
printfn "NewDrone IsFinished?: %A" NewDrone.isFinished
NewDrone.Fly
printfn "Newdrone Fly 3rd fligth: %A" NewDrone.Position
printfn "NewDrone IsFinished?: %A\n" NewDrone.isFinished

printfn "AirSpace Dronelist: Drones current location: %A" DroneList.DronesPosition
printfn "AirSpace Dronelist: %A" DroneList.droneDist
DroneList.flyDrones(1)
printfn "AirSpace Dronelist, distance after one minute flight: %A" DroneList.droneDist
DroneList.flyDrones(1)
printfn "AirSpace Dronelist, distance after two minutes of flight: %A" DroneList.droneDist
DroneList.AddDrone(TestDrone)
DroneList.DronesPosition
printf "Flight\n"
DroneList.flyDrones(1)
printfn "Distance"
DroneList.droneDist

DroneList.willCollide