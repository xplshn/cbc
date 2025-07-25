# (cbc) | The C B Compiler

This compiler is a project aiming to make a valid B compiler, with _optional_ extensions for C interoperability, and a modules system like Go's

The project is currently in its infancy, and the long-term goals are very ambitious. This is the current roadmap:

> ##### ROADMAP
>
> ###### (i) Tests
> * ~~Make a script that takes the tests from [tsoding/b](https://github.com/tsoding/b), and filters the tests.json to only include the IR tests~~
> * Make a Go program that runs each test, displays the passing/failing ones
>
> ###### (ii) Compatibility with [tsoding/b](https://github.com/tsoding/b)
> 1. ~~Support the "extrn" keyword, as well as inline assembly~~
> 2. ~~Use the same warning & error messages [tsoding/b](https://github.com/tsoding/b)~~ / our warnings n errors are much better
> 3. ~~Be able to pass the IR tests of [tsoding/b](https://github.com/tsoding/b)~~
>
> ###### (iii) Packages / Modules inspired by Go
> * ¿.. Namespaces based on .mod file ..?
> * Implement a way to import/export symbols from different .B files, in different namespaces
>

### Contributions are hyper-mega welcome

##### References
- https://research.swtch.com/b-lang
- https://www.nokia.com/bell-labs/about/dennis-m-ritchie/kbman.html
- https://www.nokia.com/bell-labs/about/dennis-m-ritchie/bref.html
- https://www.nokia.com/bell-labs/about/dennis-m-ritchie/btut.html
- https://github.com/Spydr06/BCause
- https://github.com/kparc/bcc
###### Not B-related, but I did find these helpful for learning how to write the compiler:
- [comp-bib](https://c9x.me/compile/bib/): "Resources for Amateur Compiler Writers"
- [qcc](https://c9x.me/qcc): dead-simple C compiler
- [cproc](https://github.com/michaelforney/cproc): QBE-backed compiler written in C99, with support for some C23 & GNU C extensions

###### Cool stuff used by this project:
- [QBE](https://c9x.me/compile/): The QBE compiler backend

---

###### TODO: ...I should write a Limbo compiler when I finish this project...
