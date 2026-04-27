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
    let Scene01_PauseAndPlan = Game / "Scene01_PauseAndPlan"
    let Scene02_BoxRewind = Game / "Scene02_BoxRewind"
    let Scene03_MathSimplify = Game / "Scene03_MathSimplify"
    let Scene04_SquareRace = Game / "Scene04_SquareRace"