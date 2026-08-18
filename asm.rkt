#lang racket/base

;; evm-redex/asm — the EVM-assembly language, programmatic surface.
;;
;;   (require evm-redex/asm)   ; the assembler / disassembler / runner as functions
;;   #lang evm-redex/asm       ; the same, as a language (see asm/lang/reader.rkt)
;;
;; A module path of the form `evm-redex/asm` resolves to this file; `#lang
;; evm-redex/asm` instead resolves the reader at `asm/lang/reader.rkt`.  Both go
;; through the one parser (asm/parse.rkt) and the one assembler (asm/assemble.rkt).

(require "asm/parse.rkt" "asm/assemble.rkt" "asm/execute.rkt")

(provide ;; parse text -> AST (with source locations); assemble -> bytecode
         parse-evm
         assemble assemble-parsed
         disassemble
         (struct-out asm-config) directives->config
         ;; run assembled bytecode on the interpreter
         run-source (struct-out evm-result)
         ;; the AST, for tools that want to walk a program
         (struct-out parsed) (struct-out dir) (struct-out label)
         (struct-out instr) (struct-out raw-bytes) (struct-out loc)
         (struct-out exn:fail:evm-asm))
