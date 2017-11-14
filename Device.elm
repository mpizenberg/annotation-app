-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


module Device exposing (..)


type alias Device =
    { kind : Kind
    , orientation : Orientation
    , width : Int
    , height : Int
    }


default : Device
default =
    { kind = Phone
    , orientation = Portrait
    , width = 360
    , height = 480
    }


type Kind
    = Phone
    | Tablet
    | SmallDesktop
    | BigDesktop


type Orientation
    = Portrait
    | Landscape


classify : { width : Int, height : Int } -> Device
classify { width, height } =
    let
        deviceOrientation =
            if width < height then
                Portrait
            else
                Landscape

        minDimension =
            if deviceOrientation == Portrait then
                width
            else
                height

        deviceKind =
            if minDimension < 450 then
                Phone
            else if minDimension < 850 then
                Tablet
            else if minDimension < 1250 then
                SmallDesktop
            else
                BigDesktop
    in
    { kind = deviceKind
    , orientation = deviceOrientation
    , width = width
    , height = height
    }
