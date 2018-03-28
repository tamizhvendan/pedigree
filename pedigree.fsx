type Identity = {
  Name : string
}
type ParentIdentity = {
  Father : Identity
  Mother : Identity
}
type Person = {
  Identity : Identity
  ParentIdentity : ParentIdentity option
}
type Spinster = Spinster of Person
type Bachelor = Bachelor of Person
type MarriedMale = {
  Relationship : Relationship
} and MarriedFemale = {
  Relationship : Relationship
} and Relationship = {
  Male : Bachelor
  Female : Spinster
  Sons : Set<Son>
  Daughters : Set<Daughter>
}
and Son =
| BachelorSon of Bachelor
| MarriedSon of MarriedMale
and Daughter =
| SpinsterDaughter of Spinster
| MarriedDaughter of MarriedFemale

type Human =
| BachelorHuman of Bachelor
| MarriedMaleHuman of MarriedMale
| SpinsterHuman of Spinster
| MarriedFemaleHuman of MarriedFemale
with 
  member this.Identity = 
    match this with
    | BachelorHuman (Bachelor b) -> b.Identity
    | MarriedMaleHuman mm ->
      let (Bachelor b) = mm.Relationship.Male 
      b.Identity
    | SpinsterHuman (Spinster s) -> s.Identity
    | MarriedFemaleHuman mf ->
      let (Spinster s) = mf.Relationship.Female 
      s.Identity

let identity name = {Name = name}

let newRelationship s b =
  {
    Male = b
    Female = s
    Sons = Set.empty
    Daughters = Set.empty
  }



let rec tryFindHuman (identity : Identity) (humans : Set<Human>) : Human option =
  humans
  |> Set.toList
  |> List.tryPick (tryFindHumanInFamily identity)
and tryFindHumanInFamily (identity : Identity) (human : Human) : Human option =
  if human.Identity = identity then
    Some human
  else
    match human with
    | MarriedMaleHuman mm -> tryFindHumanInRelationShip identity mm.Relationship
    | MarriedFemaleHuman mf -> tryFindHumanInRelationShip identity mf.Relationship
    | _ -> None
and tryFindHumanInRelationShip (identity : Identity) (relationship : Relationship) : Human option =
  let sons = 
    relationship.Sons
    |> Set.map (fun s -> 
      match s with
      | BachelorSon b -> BachelorHuman b
      | MarriedSon mm -> MarriedMaleHuman mm
    )
  let daughters =
    relationship.Daughters
    |> Set.map (fun s -> 
      match s with
      | SpinsterDaughter s -> SpinsterHuman s
      | MarriedDaughter md -> MarriedFemaleHuman md
    )
  Set.union sons daughters
  |> tryFindHuman identity


type Parent = {
  Father : MarriedMale
  Mother : MarriedFemale
}
let tryFindParent (parentIdentityO : ParentIdentity option) humans : Parent option = 
  let fho = 
    let fi = Option.map (fun (p : ParentIdentity) -> p.Father) parentIdentityO
    Option.bind (fun fi -> tryFindHuman fi humans) fi
  let mho = 
    let mi = Option.map (fun (p : ParentIdentity) -> p.Mother) parentIdentityO
    Option.bind (fun fi -> tryFindHuman fi humans) mi

  match fho, mho with
  | Some (MarriedFemaleHuman mf), Some (MarriedMaleHuman mm) ->
    Some {Father = mm; Mother = mf}
  | _ -> None
