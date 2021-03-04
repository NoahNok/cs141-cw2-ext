# Scratch clone
This is my CW2 extension version of the Scratch Clone

My resulting comments are:
* Parsing complex structures like functions is really hard
* Parsing a function calls parameters to be run is even harder
* I have learnt a lot about why I shouldn't write Parsers in Haskell :)
* Haskell just works!

Import the fib program for an example **(see bold note at bottom of readme)**

## Extension
### Functions
I have added functionality to use all the function blocks given.
Errors will be given either as parse errors viewable in the console, or Actual errors that show in
the bottom memory box

As far as I am aware from my testing they should work fine, and work with multiple inputs
(The editor gets a bit finicky with function inputs depending on when you rename them, and thus may pass the wrong one to the haskell program)

Functions can go to infinite stack depth reliably, but this will of course never terminate or if it does, will
take along time to process (depending on how deeply nested you've set it to go)

Functions also support running any other block inside them that the editor gives to you

### Tests
I have added a few tests that check basic function running and error handling
Some of the error handling occurs within the parsers and is reported before you even get a parsed result,
so those are thrown like other parse errors


<br>

**Please note that if you import the given fibonnaci file then you need to move the start function and 
delete the old start stub, otherwise the Interpreter will run the blank start stub first as thats
what it recieves first**
