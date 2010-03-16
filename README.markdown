# A Cocoa Test Application with Haskell

This project contains a test Cocoa application to demonstrate the usage of the `OBJC` Haskell typeclass.

A typical `OBJC` Haskell/Cocoa application would consist of:

* A GUI build with the "Interface Builder".

* A small Objective-C proxy controller: This is mainly for exporting the `IBOutlet`s to the Haskell controller and setting up the Haskell controller.

* A Haskell controller. This will be stateless and is basically a classical MVC controller.

* A Haskell model. 


## The `OBJC` typeclass

This typeclass deals with the conversion of Objective-C objects from/to Haskell values. For example there is an easy way to convert a `NSArray` to a Haskell list and vice versa.

## Further reading

For more information please read please follow the links to my blog posts:

* ["Curry’n’C Converter — Using Haskell with Objective-C in the Classic Cocoa Tutorial"][my1]
* ["A small Haskell / Objective-C Interface"][my2]
* ["A Cocoa application (almost) completely implemented in Haskell"][my3]


[my2]: http://tscheff.blogspot.com/2010/02/small-haskell-objective-c-interface.html

[my1]: http://tscheff.blogspot.com/2010/02/currync-converter-using-haskell-with.html

[my3]: http://tscheff.blogspot.com/2010/03/cocoa-application-almost-completely.html
