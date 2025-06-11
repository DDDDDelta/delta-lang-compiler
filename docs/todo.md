# TODO – deltac

> **Milestone:** **v0.2.0**  
> Target date: _TBD (set when the scope feels locked)_

## Language Features
- [ ] **Implement bit-wise operators** &nbsp;`&` `|` `^` `~` `<<` `>>`
- [ ] **Implement missing Comparison operators** &nbsp;`==` `!=` `<` `<=` `>` `>=`
- [ ] **Implement unary `!` operator**
- [ ] **`break` / `continue`** statements in loops
- [ ] **Array type**
  - [ ] Fixed size array for basic types
  - [ ] Make string literals lvalue i8 arrays

## Compiler Architecture
- [ ] **Implement basic control-flow analysis**  
  - [ ] Implicitly add control flow hint for `ir_gen` module 
  - [ ] Detect and warn unreachable code 
- [ ] **Decouple semantic analysis from parser**  
  - [ ] Move type checker & symbol resolver into sema module  

## Regression & Release Checklist
- [ ] Add grammar & docs (`docs/grammar.md`, examples)  
- [ ] Create and add missing test suite for new operators, arrays, control flow  
- [ ] Bump version metadata and tag `v0.2.0`  

---

