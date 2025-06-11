# δ DeltaLang Compiler

> *An experimental system‑programming language designed and implemented by myself (therefore named after myself). Main purpose is to support my personal research.*


## ✨ Current feature set (v0.1.0)

| Features                                      | Status |
| --------------------------------------------- | ------ |
| **Functions** (recursion supported)           | ✅      |
| **Variables** — global & local                | ✅      |
| **Fixed‑width integers** (`i8 and i32`)       | ✅      |
| **Pointers** with explicit `*T` syntax        | ✅      |
| **Foreign C calls** (`extern fn …`)           | ✅      |
| Short‑circuit `&&`, `\|\|`                    | ✅      |
| Control flow `if`, `while`                    | ✅      |

> 🌟 For the planned features in 0.2.0, see [`docs/todo.md`](./docs/todo.md).

---

## 🚀 Getting started

### 🛠️ Prerequisites

* **Rust** ≥ 1.85 (2024 edition)
* **LLVM 18 with LibPolly** (shared libraries + `llvm-config` in `PATH`)

### ⚙️ Build & run

```bash
# Clone
$ git clone https://github.com/DDDDDelta/delta-lang-compiler
$ cd delta-lang-compiler

# Build the compiler binary
$ cargo build --release

# Install the compiler
$ cargo install --path .

# Compile a DeltaLang source file to native code
$ deltac tests/code/hello.mtxx -o hello --verify-llvm
$ ./hello
```

See [`tests/code`](./tests/code) for more snippets.

---

## 🏗️ Technology stack

| Where         | What                | Why                                     |
| ------------- | ------------------- | --------------------------------------- |
| Front‑end     | **Regular Rust**    | Mainly for great tooling    |
| Middle-end (IR gen) | **Inkwell & llvm-sys** | Safe high‑level bindings with raw FFI escape hatch |
| IR / Back‑end | **LLVM 18** | Mature optimiser & multi‑target codegen |

---

## 📄 License

This project is licensed under

* **📜 GNU General Public License, Version 3**

You are free to use, modify, and share the code, so long as you keep it open‑source under the same license and provide the source.
The software comes without warranty.

---

> **🚧 Status:**  Very early development stage. Feel free to play around with it.
