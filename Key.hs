module Key where
import Note

data Key = Key {note :: Note, octave :: Int} deriving (Eq)

instance Show (Key) where
        show k = (show . note) k ++ (show . octave) k

noteToKey :: Note -> Key
noteToKey n = Key n 4
