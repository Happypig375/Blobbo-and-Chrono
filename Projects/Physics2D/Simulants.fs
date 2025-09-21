﻿namespace Physics2D
open System
open Nu

// this module provides global handles to the game's key simulants.
// having a Simulants module for your game is optional, but can be nice to avoid duplicating string literals across
// the code base.
[<RequireQualifiedAccess>]
module rec Simulants =

    let EnclosureScreen = Game / "Enclosure"
    let RacecourseScreen = Game / "Racecourse"

    let [<Literal>] SceneGroup = "Scene"
    let [<Literal>] BorderEntity = "Border"
