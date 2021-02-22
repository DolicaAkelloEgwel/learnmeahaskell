doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else doubleMe x

-- You're allowed to use apostophes in a function name.
-- A function name needs to start with a lower case character.
conanO'Brien = "It's a-me, Conan O'Brien!"
