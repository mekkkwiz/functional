-- join 2 string function
join :: ([a], [a]) -> [a]
join ([], b) = b
join (f:fs, b) = f : join (fs, b)


-- reverse string function
rev :: ([a]) -> [a]
rev ([]) = []
rev (x:xs) = join(rev(xs) , [x])
-- type of rev is ([a] -> [a])


-- zip 2 lists function
zipper :: ([a],[b]) -> [(a,b)]
zipper ([],[]) = []
zipper ([],_) = []
zipper (_,[]) = []
zipper (f:fs, b:bs) = (f,b) : zipper(fs,bs)
-- type of zipper is ([a],[b] -> [(a,b)])

