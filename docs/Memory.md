# Memory

## Description
The memory is implemented to be __sparse__ and __unified__:
* a `Map<Address, byte>` is used for storage, avoiding the storage of unused addresses `->` sparsity
* _storage_, _instructions_ (in form of an AST) and _labels_ are under a single data structure `->` unity
---
## Types
* `type Memory = { AST: AST ; Storage: Map<Address, byte> ; Labels: LabelMap }` 
record of AST, storage map and labels map.
---
## Functions
* `Memory.AST_ : ( Memory -> AST ) * ( AST -> Memory -> Memory ) ` Lens for `AST`, useful for UI.
* `Memory.Storage_ : ( Memory -> Map<Address, byte> ) * ( Map<Address, byte> -> Memory -> Memory ) ` 
Lens for `Storage` map, useful for UI.
* `Memory.Labels_ : ( Memory -> LabelMap ) * ( LabelMap -> Memory -> Memory ) ` Lens for `LabelMap`, useful for UI.

* `private Memory.getOrZero : ( Address -> Map<Address, byte> -> byte ) ` 
Returns byte content of specified address, if not accessed before returns `0uy`.
* `Memory.Byte_ : ( Address -> Memory -> byte ) * ( Address -> byte -> Memory -> Memory ) ` 
Lens for `MemoryLocation`, `get<|>set` of memory location (byte) given an address.
* `Memory.Word_ : ( Address -> Memory -> Data ) * ( Address -> Data -> Memory -> Memory ) ` 
Lens for `Data`, `get<|>set` of memory locations (word|4-bytes) given an address.
* `make : (AST * LabelMap) -> Memory` Memory initialiser - it will be called only once in every execution of the assembly code.
* `makeEmpty` unit -> Memory` Memory initialiser - used only for individual memory testing without complete AST.
---
## Dependencies
* `Common.AST`
* `Common.Address`
* `Common.LabelMap`
* `Optics.get`
* `Optics.set`
---
## Contributors
Angelos Filos