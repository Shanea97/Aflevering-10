/// skal kunne køre mono experimentWAnimals.exe 40 test.txt 10 30 10 2 10 4
///hvor T =40,n=10,e=30, felg =10,u=2, fulv = 10 og s=4
open Animals
[<EntryPoint>]
let main args  =
    printfn "%A" args
    let time = int(args.[0]) //Ticks der skal køres
    let navn = args.[1] //filnavn
    let boardWidth = int(args.[2]) //board w n x n
    let NMooses = int(args.[3]) //elge
    let NWolves = int(args.[4]) //ulve
    let mooseRepLen = int(args.[5]) //f_els
    let wolvesRepLen = int(args.[6]) //f_ulv
    let wolvesHungLen = int(args.[7]) //sulttid
    let env = environment (boardWidth, NMooses, mooseRepLen, NWolves, wolvesRepLen, wolvesHungLen, true)
    ///vi skriver data ud i csv.format
    let writer = System.IO.File.CreateText navn
    writer.Write "Samlet antal dyr,"
    writer.Write "Ulve,"
    writer.Write "Elge,"
    writer.Write "Ticks\n"
    for i = 1 to time do
      env.tick()
      printfn "%A" env
      writer.Write (env.count)
      writer.Write ","
      writer.Write (env.wolfcount)
      writer.Write ","
      writer.Write (env.moosecount)
      writer.Write ","
      writer.Write (i)
      writer.Write "\n"
    writer.Flush()
    writer.Close()
    0 //succesindikator for funktion
