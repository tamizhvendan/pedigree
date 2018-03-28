#load "pedigree.fsx"
open Pedigree

type PedigreeEvent =
| Wedding of Identity * Identity
| SonBorn of Identity * ParentIdentity
| DaughterBorn of Identity * ParentIdentity
| AddBachelor of Identity
| AddSpinster of Identity

let isSamePerson identity (h: Human) = 
  h.Identity = identity
let isAlreadyExists identity humans =
  Set.exists (isSamePerson identity) humans

let removeAndAdd r a humans =
  humans
  |> Set.remove r
  |> Set.add a

let addIfNotExists identity parentIdentity f humans =
  if isAlreadyExists identity humans then
    humans
  else
    let h = f {Identity = identity; ParentIdentity = parentIdentity}
    Set.add h humans

let childBorn (parentIdentity : ParentIdentity) updateRelationship humans =
  let mmh = tryFindHuman parentIdentity.Father humans
  let mfh = tryFindHuman parentIdentity.Mother humans
  match mfh, mmh with
  | Some (MarriedFemaleHuman mf), Some (MarriedMaleHuman mm) ->
    let updatedRelationship = updateRelationship mm.Relationship
    let updatedMMH = MarriedMaleHuman {mm with Relationship = updatedRelationship}
    let updatedMFH = MarriedFemaleHuman {mf with Relationship = updatedRelationship}
    humans
    |> removeAndAdd (MarriedFemaleHuman mf) updatedMMH
    |> removeAndAdd (MarriedMaleHuman mm) updatedMFH
  | _ -> humans

let update (humans : Set<Human>) (event : PedigreeEvent) = 
  match event with
  | AddBachelor identity ->
    addIfNotExists identity None (Bachelor >> BachelorHuman) humans
  | AddSpinster identity ->
    addIfNotExists identity None (Spinster >> SpinsterHuman) humans
  | Wedding (sIdentity, bIdentity) ->
    let sho = tryFindHuman sIdentity humans
    let bho = tryFindHuman bIdentity humans
    match sho, bho with
    | Some (SpinsterHuman s), Some (BachelorHuman b) ->
      let relationship = newRelationship s b
      let mmh = MarriedMaleHuman {Relationship = relationship} 
      let mfh = MarriedFemaleHuman {Relationship = relationship}
      let (Spinster sp) = s
      let (Bachelor bp) = b
      let spo = tryFindParent sp.ParentIdentity humans
      let bpo = tryFindParent bp.ParentIdentity humans
      let updatedHumans =
        match spo with
        | Some sp -> humans
        | None -> removeAndAdd (SpinsterHuman s) mfh humans
      humans
      |> removeAndAdd (SpinsterHuman s) mfh
      |> removeAndAdd (BachelorHuman b) mmh
    | _ -> humans
  | DaughterBorn (identity, parent) ->
    let daughter =
        {Identity = identity; ParentIdentity = Some parent} |> Spinster |> SpinsterDaughter
    let updateRelationship relationship =
      {relationship with Daughters = Set.add daughter relationship.Daughters}
    childBorn parent updateRelationship humans
  | SonBorn (identity, parent) ->
    let son =
        {Identity = identity; ParentIdentity = Some parent} |> Bachelor |> BachelorSon
    let updateRelationship relationship =
      {relationship with Sons = Set.add son relationship.Sons}
    childBorn parent updateRelationship humans

let familyTree events = 
  let rec familyTree' events humans =
    match events with
    | [] -> humans
    | x :: xs -> 
      update humans x
      |> familyTree' xs 
  familyTree' events Set.empty