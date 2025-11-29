# Commands to Check Caqti API

Run these commands on your system to see what functions are available:

## 1. Check installed Caqti packages:
```bash
opam list | grep caqti
```

## 2. Show Caqti package information:
```bash
opam show caqti
opam show caqti-request  # if it exists as separate package
```

## 3. Use ocamlfind to list available modules:
```bash
ocamlfind list | grep caqti
```

## 4. Check the compiled interface files:
```bash
# Find where Caqti is installed
opam var lib
# Then look for .cmi files:
find $(opam var lib)/caqti -name "*.cmi" 2>/dev/null
find $(opam var lib)/caqti-request -name "*.cmi" 2>/dev/null
```

## 5. Use utop to inspect interactively:
```bash
utop
# Then in utop:
#require "caqti";;
#require "caqti-request";;
open Caqti_request;;
# Try to see what's available:
Caqti_request.exec;;
Caqti_request.find_opt;;
Caqti_request.collect;;
```

## 6. Or create a simple test file and compile it:
Create a file `test_api.ml`:
```ocaml
open Caqti_request
let _ = exec
let _ = find_opt  
let _ = collect
```

Then try:
```bash
ocamlc -I $(opam var lib)/caqti -I $(opam var lib)/caqti-request -c test_api.ml
```

## 7. Check online documentation:
- https://github.com/paurkedal/ocaml-caqti
- Or check the README/docs in: `$(opam var lib)/caqti/`
