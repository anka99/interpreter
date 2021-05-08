# interpreter
## Build BNFC parser
bnfc -m --functor emm.cf

## Build project
ghc -o main --make Main.hs

## Usage
./main [args]
