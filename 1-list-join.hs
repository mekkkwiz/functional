join :: ([a], [a]) -> [a]
join (f, []) = f
join ([], b) = b
join (f:fs, b) = f : join (fs, b)

-- shorter that 1st version
-- join ([], b) = b
-- join (f:fs, b) = f : join (fs, b)



