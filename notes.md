```haskell
min True False
```
Why does this return False?

```haskell
min 1 2
1 `min` 2
```
Changing between infix and prefix notation.

```haskell
succ (succ 10)
```
Returns 12.

```haskell
doubleSmallNumber x = if x > 100
                        then x
                        else doubleMe x
```
if-statements are expressions and return values. The else statement is _mandatory_, so every if-statement will
return something.
