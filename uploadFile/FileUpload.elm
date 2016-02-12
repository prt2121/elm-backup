module FileUpload where

import String

port output : Signal (List String)
port output = Signal.map String.lines openFromFile

port openFromFile : Signal String
-- elm make FileUpload.elm --output=build/FileUpload.js
