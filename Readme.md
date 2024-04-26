
# AbstractTensor.lisp

# Workload

- [ ] Coalton-Based Implementation
- [ ] Lazy Graph Construction
- [ ] Only 27 principle operators. JIT Compiler
- [ ] Super eazy to add a new backend
- [ ] Keep the code simple and tiny.
- [ ] PyTorch-Level Speed
- [ ] Quantization Support
- [ ] Complex number support dedicated to HiFi-GAN
- [ ] Super fast to compile the model.
- [ ] Add: Eazy to add Python/Coalton binding.
- [ ] Eazy to add manual optimization techniques by users
- [ ] Documentation System
- [ ] Better Examples

## Usage (WIP)

- Lisp風DSLを記述
- gcc的に使えるようにしたい

### Codegen

```
$ caten --runtime cuda
"
(function ((A{T}[M, N], B{T}[M, N]) -> (A{T}[M, k]) where K = (* 2 M))
    (setf z1 (+ A B))
    (return z1))

"

```

### Autodiff (WIP)

```
$ adeep --runtime cuda --plugin cuda-backend.lisp
"
modules:
  function L1: A{Dense}[M, N] B{Dense}[M, N] -> A{T}[M, k]
  function L2: A{Dense}[M, N] B{Dense}[M, N] -> A{T}[M, k]

parameters:
  A{Grad}
  B{Grad}

main:
  output = (L1 (L2 A B) C)
"
```

- T = {Dense, Sparse, Complex}

## 設計的な

- [ ] AbstractTensor/IR 内部IR
    - Tensorの情報 (Name, Required-Grad, Dynamic Shape) を伝える
    - BodyのDAGを記述する部分
    - Iterations can be only expressed as permutations.
    - 
    
- [ ] AbstractTensor/