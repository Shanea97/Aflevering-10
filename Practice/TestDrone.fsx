member this.willCollide(minutes) = 
         for l = 0 to dronesList.Length-1 do
            for m = 1 to minutes do  
                printf "%A minutes" m
                dronesList.[l].Fly
                for i = 0 to dronesList.Length-1 do 
                    printfn " "
                    for j = 0 to dronesList.Length-1 do 
                        let dron1 = dronesList.[j].Position 
                        let dron2 = dronesList.[i].Position
                        let distance = (sqrt(((fst dron2-fst dron1)**2.0)+((snd dron2-snd dron1)**2.0))) 
                        if distance <= 5.0 && distance > 0.0 then 
                            printfn "Two or more Drones have collided"
                        else 
                            printfn "No Drones have collided"



member this.willCollide = 
        for i = 0 to dronesList.Length-1 do 
            printfn " "
            for j = 0 to dronesList.Length-1 do 
                let dron1 = dronesList.[j].Position 
                let dron2 = dronesList.[i].Position
                let distance = (sqrt(((fst dron2-fst dron1)**2.0)+((snd dron2-snd dron1)**2.0))) 
                if distance <= 5.0 && distance > 0.0 then 
                    printfn "Collision have occured"
                else 
                    printfn "No collision detected"