module Note where
data Note = A | B | C | D | E | F | G | Sharp Note | Flat Note 

-- Remove Accidentals
remAcc :: Note -> Note
remAcc (Flat C) = B
remAcc (Sharp B) = C
remAcc (Flat F) = E
remAcc (Sharp E) = F
remAcc n = n

sameNote :: Note -> Note -> Bool
sameNote A A                    = True
sameNote B B                    = True
sameNote C C                    = True
sameNote D D                    = True
sameNote E E                    = True
sameNote F F                    = True
sameNote G G                    = True
sameNote (Sharp C) (Flat D)     = True
sameNote (Flat D) (Sharp C)     = True
sameNote (Sharp D) (Flat E)     = True
sameNote (Flat E) (Sharp D)     = True
sameNote (Sharp F) (Flat G)     = True
sameNote (Flat G) (Sharp F)     = True
sameNote (Sharp G) (Flat A)     = True
sameNote (Flat A) (Sharp G)     = True
sameNote (Sharp A) (Flat B)     = True
sameNote (Flat B) (Sharp A)     = True
sameNote _ _ = False

instance Eq (Note) where 
        (==) a' b'
          | sameNote a b = True
          | otherwise = False
          where a = remAcc a'
                b = remAcc b'

noteStr :: Note -> String
noteStr (Sharp n) = noteStr n ++ "#"
noteStr (Flat n) = noteStr n ++ "b"
noteStr A = "A"
noteStr B = "B"
noteStr C = "C"
noteStr D = "D"
noteStr E = "E"
noteStr F = "F"
noteStr G = "G"

instance Show (Note) where
        show n = noteStr n

--This all works because These notes for a group under addition isomorphic to Z/12Z
succ :: Note -> Note
succ A = Sharp A
succ B = C
succ C = Sharp C
succ D = Sharp D
succ E = F
succ F = Sharp F
succ G = Sharp G
succ (Sharp A) = B
succ (Sharp C) = D
succ (Sharp D) = E
succ (Sharp E) = Sharp F
succ (Sharp F) = G
succ (Sharp G) = A
succ (Flat A) = A
succ (Flat B) = B
succ (Flat C) = C
succ (Flat D) = D
succ (Flat E) = E
succ (Flat F) = F
succ (Flat G) = G

plus :: Note -> Int -> Note
plus a n = (foldr (.) id (replicate (n `mod` 12) Note.succ)) a

--Wrap around
pred :: Note -> Note
pred a = a `plus` 11


