﻿open System.IO

type Coordinate = Coordinate of int * int * int * int
module Coordinate =
    let neighbours coord =
        let (Coordinate (x,y,z,w)) = coord
        [x-1..x+1] |> List.collect (fun x ->
        [y-1..y+1] |> List.collect (fun y ->
        [z-1..z+1] |> List.collect (fun z -> 
        [w-1..w+1] |> List.map (fun w ->
            Coordinate (x,y,z,w)))))
        |> List.except [coord]

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines("input")

    let xMin,xMax = 0,input.[0].Length-1
    let yMin,yMax = 0,input.Length-1
    let zMin,zMax = 0,0
    let wMin,wMax = 0,0

    let initiallyActiveCubes =
        let z,w = 0,0
        input |> Array.mapi (fun y row ->
            row |> Seq.mapi (fun x cube ->
                match cube with
                | '#' -> Some (Coordinate (x,y,z,w))
                | _ -> None)
            |> Seq.choose id
            |> Array.ofSeq)
        |> Array.collect id
        |> Set.ofArray

    let cycle times activeCubes =
        let rec cycleImpl times xMin xMax yMin yMax zMin zMax wMin wMax activeCubes =
            if times = 0 then activeCubes else
            let isActive c = activeCubes |> Set.contains c
            let nextActiveCubes =
                [xMin..xMax] |> List.collect (fun x ->
                [yMin..yMax] |> List.collect (fun y ->
                [zMin..zMax] |> List.collect (fun z ->
                [wMin..wMax] |> List.choose (fun w ->
                    let coord = Coordinate(x,y,z,w)
                    let activeNeighbours =
                        coord
                        |> Coordinate.neighbours
                        |> List.filter isActive
                        |> List.length
                    match isActive coord,activeNeighbours with
                    | true,2 | true,3 | false,3 -> Some(coord)
                    | _ -> None))))
            cycleImpl (times-1) (xMin-1) (xMax+1) (yMin-1) (yMax+1) (zMin-1) (zMax+1) (wMin-1) (wMax+1) (Set.ofList nextActiveCubes)

        cycleImpl times (xMin-1) (xMax+1) (yMin-1) (yMax+1) (zMin-1) (zMax+1) (wMin-1) (wMax+1) activeCubes

    initiallyActiveCubes
    |> cycle 6
    |> Set.count
    |> printfn "%i"

    0 // return an integer exit code
