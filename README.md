# interpreter
## Build BNFC parser 
bnfc -m --functor emm.cf

## Build project
ghc -o main --make main.hs

## Usage
./main [args]
