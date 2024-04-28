
# AbstractTensor.lisp

# Goal

- Python/Coalton/Common Lisp/その他言語でAPIを呼び出せる

- Keep Simple; < 7000 lines

- Kernel自動生成の考え方。(Operator, Composite, Realizeの考え方)

- 超デバッグが簡単

- 20年後でもメンテナンス可能にする

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
- [ ] Support these ops and optimizations: Take, Slice
- [ ] Elegant Error
- [ ] Eazy to add graph-rewriting
- [ ] Every Ops are in-place, but for example add is defined as: (add (copy x) y).
- [ ] A Concept of Composite
- [ ] Super fast compiling time
- [ ] GraphView System
- [ ] Super eazy to debug/optimize/and understand the bottleneck.
- [ ] Kernel自動生成の考え方，Conv/ReLUなどは事前に自動生成しておく。
- [ ] Eazy (and fast) to rewriting a graph
    - CompileされたCompositeの一覧をどっかのLUTに割り振れるようにする。
    - Training/Inferenceの実行は，datenにGraphを記述することで実行できるようにする。
- [ ] randn/betaとかもコンパイルして生成
- winograd

- [ ] TinygradのScheduling, Loweringをもってくる。
- コンセプトはこう:
    - 27のPrinciple Operatorがある。(Including Take, If, etc...)
    - Composite = 27 Opsを組み合わせて複雑な命令(ReLU, Conv2D, Gemm...)を表現
    - In-Placeにこだわりすぎなくていい (LayerNormとか，ある程度でかいブロックでおk)
    - Composite Cache System. 自動でコンパイル結果をCache+割り当てする。 (~/tmpとかに配置)
    - Dynamicなグラフ書き換えに対応。
    - ^- defoptimizerみたいな感じで，書き換えのルールを作成。(OpenBLASのGemmに書き換える，など)
    - Composite call from compositeができるようにしたい
    - 今考える: Multiple BackendにCompileできるPrinciple Ops(Take)を実装. -> Scheduler -> Compile
    - [10, 10][10, 0]にしたらSIMD化できないけど，どう実装するのか気になる
    - ^- 上の最適化が必要になるのってどんな状況だろう？
        - Composite <-> Compositeの箇所でSliceは挟まないから，一旦Composite内部で考えればよさそう？
	- 要は早いConv2DのCompositeさえ入手できれば(PyTorchがそうなように) 十分な速度を期待できる。
	- Scalarに対するCompilerもつくる！
	
- [ ] daten.ros, abstracttensor/high-level/autograd, abstracttensor/high-level/quantization, nn, etc...

## Usage (WIP)

- Lisp風DSLを記述 (他の言語から自動生成されること前提)
- gcc的に使えるようにしたい

### Codegen

```
$ caten --runtime cuda
"
(composite ((A{T}[M, N], B{T}[M, N]) -> (A{T}[M, k]) where K = (* 2 M))
    (setf z1 (+ A B))
    (return z1))
"
↑ yamlでもいい気がする
conv2d:
    - in:
        - A{T}[M, N]<0 1>(where N = 1)
	- B{T} ...
    - out:
	- B # outはSymbolのみ
    - impl:
        - "(progn ...)"
```

### Autodiff (WIP)

```
$ daten --runtime cuda --plugin cuda-backend.lisp
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

- [ ] AbstractTensor/Lang
    - DL Compiler専用プログラミング言語的な