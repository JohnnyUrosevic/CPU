type Bits = [Bool]

nor :: Bits -> Bits -> Bits
nor [] (_:__) = error "mismatched length"
nor (_:__) [] = error "mismatched length"
nor [] [] = []
nor (x:xs) (y:ys) = (norBit x y):nor xs ys
    where norBit x y = if x == True || y == True then False else True

not :: Bits -> Bits
not x = nor x x

or :: Bits -> Bits -> Bits
or x = Main.not . nor x

main = do
    print (Main.or [True, False] [False, False])
