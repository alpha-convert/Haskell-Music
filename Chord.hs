module Chord where
import Note

data Chord = Chord {notes :: [Note]} deriving (Eq)

majorChord :: Note -> [Note]
majorChord n = [n,n `plus` 4, n `plus` 7]

minorChord :: Note -> [Note]
minorChord n = [n,n `plus` 3, n `plus` 7]

