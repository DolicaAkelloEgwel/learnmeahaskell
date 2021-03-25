divides d n = rem n d == 0
ldf k n | divides k n = k -- Start by seeing if n can be divided by two
        | k^2 > n     = n -- Proposition 1.2
        | otherwise   = ldf (k + 1) n -- Try again with the next number
ld n = ldf 2 n
prime n | n < 1 = error "not a positive integer"
        | n == 1 = False
        | otherwise = ld n == n

-- Type declarations
is_even = divides 2
