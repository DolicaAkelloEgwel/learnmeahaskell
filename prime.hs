divides d n = rem n d == 0
ldf k n | divides k n = k -- Start by seeing if n can be divided by two
        | k^2 > n     = n -- Proposition 1.2
        | otherwise   = ldf (k + 1) n -- Try again with the next number
ld n = ldf 2 n
prime n = ld n == n
