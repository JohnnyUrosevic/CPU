type Bits = [Bool]

nor :: (Bits a) => a -> a -> a
nor [] ys = error "length mismatch"
nor xs [] = error "length mismatch"
nor [] [] = []
nor x:xs y:ys = (\x y -> if x == True || y == True then False else True):nor xs ys

not :: (Bits a) => a -> a
not x = nor x x