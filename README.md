# Conway's Game Of Life 
[![Build Status](https://travis-ci.org/RossMeikleham/Idris-Conway.svg?branch=master)](https://travis-ci.org/RossMeikleham/Idris-Conway)
## Implementation of [Conway's Game Of Life]( https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) in Idris
![pulsar](/images/Pulsar.gif?raw=true) 

# Required
- SDL and Idris SDL bindings, bindings can be found [here](https://github.com/edwinb/SDL-idris)
- [Idris](http://www.idris-lang.org/idris-0-9-19-released/) 0.9.19 or above

# Building
- `idris --build conway.ipkg`

# Running
  `conway example.RLE`

Currently this implementation reads a file representing an initial Conway State in [Runtime Length Encoded Format](http://www.conwaylife.com/wiki/Run_Length_Encoded). Pressing a key with the SDL window focussed increments
the state

# TODO
- Properly support RLE format, there's a few bugs in parsing the format
- Generate animated GIF image for up to a given amount of states (if the conway state loops at any point before the specified amount of states to capture, we can stop capturing at this point)


# Examples

## Pulsar
`./conway examples/pulsar.RLE`


![pulsar](/images/Pulsar.gif?raw=true) 

## Octagon2
`./conway examples/octagon2.RLE`


![octagon2](/images/Octagon2.gif?raw=true)
