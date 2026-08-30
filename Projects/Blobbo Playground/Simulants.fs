namespace BlobboPlayground
open System
open Nu

// this module provides global handles to the game's key simulants.
// having a Simulants module for your game is optional, but can be nice to avoid duplicating string literals across
// the code base.
[<RequireQualifiedAccess>]
module Simulants =

    let Splash = Game / "Splash"
    let Title = Game / "Title"
    let Scene01_BlobboThrow = Game / "Scene01_BlobboThrow"
    let Scene02_BoxRewind = Game / "Scene02_BoxRewind"
    let Scene03_MathSimplify = Game / "Scene03_MathSimplify"
    let Scene04_SquareRace = Game / "Scene04_SquareRace"
    let Scene05_HeaterCooler = Game / "Scene05_HeaterCooler"
    let Scene06_M1ControlStudy = Game / "Scene06_M1ControlStudy"