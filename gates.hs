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
or x y = Main.not $ nor x y

and :: Bits -> Bits -> Bits
and x y = nor (Main.not x) (Main.not y)

nand :: Bits -> Bits -> Bits
nand x y = Main.not $ Main.and x y

xor :: Bits -> Bits -> Bits
xor x y = Main.and (Main.or x y) (Main.nand x y)

main = do
    print (Main.xor [True, False, True] [False, False, True])
