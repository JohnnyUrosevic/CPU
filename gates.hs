type Bits = [Bool]

nor :: Bits -> Bits -> Bits
nor [] [] = []
nor (x:xs) (y:ys) = (norBit x y):nor xs ys
    where norBit x y = if x == True || y == True then False else True

not :: Bits -> Bits
not x = nor x x

main = do
    print True
