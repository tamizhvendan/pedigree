#load "source.fsx"
open Pedigree
open Source

let sembiyan = identity "Sembiyan"
let bama = identity "Bama"
let sb = { Mother = bama; Father = sembiyan}

let natarajan = identity "Natarajan"
let selvi = identity "Selvi"
let ns : ParentIdentity = {Mother = selvi; Father = natarajan}

let atchaya = identity "Atchaya"

let tamizh = identity "Tamizh"
let abi = identity "Abi"
let ta : ParentIdentity = {Mother = abi; Father = tamizh}

let abaya = identity "Abaya"
let avanthi = identity "Avanthi"

let events = 
  [
    AddBachelor natarajan
    AddBachelor sembiyan
    AddSpinster bama
    AddSpinster selvi
    Wedding (selvi, natarajan)
    Wedding (bama, sembiyan)
    DaughterBorn (abi, ns)
    SonBorn(tamizh, sb)
    DaughterBorn (atchaya, ns)
    Wedding (abi, tamizh)
    DaughterBorn (abaya, ta)
    DaughterBorn (avanthi, ta)
  ]
let run () = 
  familyTree events
  |> Set.iter (fun h -> printfn "%s" (h.ToString()))