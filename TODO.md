# Steps to continue the development of the compiler

- Allow toplevel names to be bound
    - bind the name to a virtual register before generating the body to allow recursion
- Constants
    - constants are capitalized toplevel names not bound 
    - annotated with their valency
    - desugar before binding 
        to a function whose body is some kind of internal expression that initialized it as a struct
    - pattern matching
- Better register allocation
    - analyse when vregs are alive
    - create a graph which knows which vregs are alive at the same time
    - assign to hardware registers as it goes, spill register with most neighbors when needed
- Type checking
    - generate then solve constraints