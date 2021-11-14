# Instructions
When using nix run the project by doing:
```
nix-shell
cabal run
```

When using another package manager install the development package of `zlib` before running `cabal run`.

The word frequency service will now be available on "http://localhost:8080" with endpoints at 
'/calculateFrequencyForWord', '/calculateHighestFrequency' and '/calculateMostFrequentWords'.  

## Examples

```
curl -X POST -d '{"word":"Hoop", "text":"Hello Hella Hoop hoop hoop hoop"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8080/calculateFrequencyForWord
```
```
curl -X POST -d '{"text" : "Hello Hella Hoop hoop hoop hoop"}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8080/calculateHighestFrequency
```
```
curl -X POST -d '{"text" : "Hello Hella Hoop hoop hoop hoop", "n" : 5}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8080/calculateMostFrequentNWords
```

## Test suite
Execute `cabal test` to run the test suite.