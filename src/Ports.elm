-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/


port module Ports exposing (..)

import Json.Encode as Encode
import Packages.Device as Device exposing (Device)


port resizes : (Device.Size -> msg) -> Sub msg


port loadImageFile : Encode.Value -> Cmd msg


port imageLoaded : (( String, Int, Int ) -> msg) -> Sub msg


port loadConfigFile : Encode.Value -> Cmd msg


port configLoaded : (String -> msg) -> Sub msg
