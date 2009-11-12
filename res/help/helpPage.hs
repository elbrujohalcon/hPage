-- Welcome to hPage!
-- To start using it, just write some haskell expressions separated by empty lines 
-- like the following ones:

12 + 30

length [0..41]

-- Now you can place your cursor on any of them and then press the [Interpret] button below
-- or just use Ctrl-I to get their values or types

-- You can also type in Type names, like...

Int

IO

-- ...to see their kind, using the same mechanism

-- You can define functions too, let's try (once again) with the well known fact...

fact x = foldl (*) 1 [1..x]

-- Now if you place your cursor on the next expression, you can ask for its interpretation

fact 20

-- If you don't want to use the whole page content in an evaluation, you can mark the exact page zone you want to evaluate and then press [Interpret]

-- That's just to start up... you can do some serious stuff with this application, just surf the different menues