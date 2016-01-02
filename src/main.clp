;-----------------------------------------------------------------------------
; msp430ucode
; Copyright (c) 2015, Joshua Scoggins
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR 
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;-----------------------------------------------------------------------------
; main.clp - microcode entry point
;-----------------------------------------------------------------------------
(defgeneric apply$)
(defgeneric reverse$)
(defgeneric zero-register)
(defgeneric set)
(defgeneric body 
            "a hack that makes it possible to describe a segment")
(defmethod body
  ($?elements))
(defmethod zero-register
  ((?register register
              alias))
  (format nil 
          "move %s = %s"
          (send ?register resolve)
          (send [zero] resolve)))
(defmethod zero-register
  ((?registers MULTIFIELD))
  (apply$ zero-register
          ?registers))
(defmethod zero-register
  ($?registers)
  (zero-register ?registers))

(defmethod set
  ((?destination register
                 alias)
   (?eq SYMBOL (eq ?current-argument =))
   (?value SYMBOL))
  (format nil 
          "set %s = %s" 
          (send ?destination resolve)
          ?value))
(defmethod set
  ((?destination register
                 alias)
   (?eq SYMBOL (eq ?current-argument =))
   (?value SYMBOL)
   $?rest)
  (create$ (set ?destination 
                ?eq 
                ?value)
           (set (expand$ ?rest))))
(defmethod apply$
  ((?function SYMBOL)
   (?args MULTIFIELD))
  (bind ?output
        (create$))
  (progn$ (?a ?args)
          (bind ?output
                ?output
                (funcall ?function
                         ?a)))
  ?output)
(defmethod apply$
  ((?function SYMBOL)
   $?args)
  (apply$ ?function
          ?args))
(defmethod reverse$
  ((?args MULTIFIELD))
  (bind ?contents
        (create$))
  (progn$ (?a ?args)
          (bind ?contents
                ?a
                ?contents))
  ?contents)
(defmethod reverse$
  ($?args)
  (reverse$ ?args))
(deffunction single-operand-instruction
             (?operation ?operand)
             (format nil
                     "%s %s"
                     ?operation
                     (send ?operand resolve)))
(deffunction save-register
             (?register)
             (single-operand-instruction push 
                                         ?register))
(deffunction restore-register
             (?register)
             (single-operand-instruction pop
                                         ?register))
(deffunction args
             ($?input)
             ?input)
(deffunction !deffunction
             (?name ?save $?body)
             (create$ (label ?name)
                      (apply$ save-register
                              ?save)
                      ?body
                      (apply$ restore-register
                              (reverse$ ?save))
                      return))

(deffacts msp430-registers
          (aliases pc -> r192)
          (aliases m0 -> r192)
          (aliases sp -> r193)
          (aliases m1 -> r193)
          (aliases status -> r194)
          (aliases m2 -> r194)
          (aliases cgen -> r195)
          (aliases m3 -> r195)
          (aliases m4 -> r196)
          (aliases m5 -> r197)
          (aliases m6 -> r198)
          (aliases m7 -> r199)
          (aliases m8 -> r200)
          (aliases m9 -> r201)
          (aliases m10 -> r202)
          (aliases m11 -> r203)
          (aliases m12 -> r204)
          (aliases m13 -> r205)
          (aliases m14 -> r206)
          (aliases m15 -> r207)
          ;-----------------------------------------------------------------------------
          ; Processor special function registers (interrupt control registers)
          ; in the description of the memory map these are byte address locations but we'll do some magic internally to use registers instead
          ; at some point we could introduce optimizations to keep that data local in registers
          ;-----------------------------------------------------------------------------
          ;(aliases psfr0 -> r208)
          ;(aliases psfr1 -> r209)
          ;(aliases psfr2 -> r210)
          ;(aliases psfr3 -> r211)
          ;(aliases psfr4 -> r212)
          ;(aliases psfr5 -> r213)
          ;(aliases psfr6 -> r214)
          ;(aliases psfr7 -> r215)
          ;-----------------------------------------------------------------------------
          ; internal registers for decoding instructions
          ;-----------------------------------------------------------------------------
          (aliases currinst -> r216)
          (aliases prefix -> r217)
          (aliases opcode -> r218)
          ;-----------------------------------------------------------------------------
          ; single operand arithmetic
          ;-----------------------------------------------------------------------------
          (aliases bw -> r219)
          (aliases addr-mode -> r220)
          ;-----------------------------------------------------------------------------
          ; Conditional jump; PC -> PC + 2 * offset
          ;-----------------------------------------------------------------------------
          (aliases condition -> r221)
          (aliases offset -> r222)
          ;-----------------------------------------------------------------------------
          ; Two operand arithmetic
          ;-----------------------------------------------------------------------------
          (aliases source -> r223)
          (aliases register -> r223)
          (aliases destination -> r224)
          (aliases ad -> r225)
          ;-----------------------------------------------------------------------------
          ; hardcoded instruction masks
          ;-----------------------------------------------------------------------------
          (aliases prefix-mask -> r255)
          (aliases opcode-mask-single-operand-arithmetic -> r254)
          (aliases bw-mask -> r253)
          (aliases as-mask -> r252)
          (aliases condition-mask -> r251)
          (aliases opcode-mask-two-operand-arithmetic -> r250)
          (aliases ad-mask -> r249)
          (aliases source-register-mask -> r248)
          (aliases destination-register-mask -> r247)
          (aliases signed-offset-mask -> r246)
          (aliases register-single-operand-arithmetic-mask -> r245)
          (aliases single-operand-arithmetic-check-mask -> r244)
          (aliases single-bit-mask -> r243))

(deffunction test 
             ()
             (create$ (.code)
                      (!deffunction ucode-start 
                                    (args)
                                    (zero-register [pc] 
                                                   [sp]
                                                   [status]
                                                   [cgen])
                                    (set [prefix-mask]                                = #b1110000000000000
                                         [single-operand-arithmetic-check-mask]       = #b0001110000000000
                                         [opcode-mask-single-operand-arithmetic]      = #b0000001110000000
                                         [bw-mask]                                    = #b0000000001000000
                                         [as-mask]                                    = #b0000000000110000
                                         [register-single-operand-arithmetic-mask]    = #b0000000000001111
                                         [condition-mask]                             = #b0001110000000000
                                         [signed-offset-mask]                         = #b0000001111111111
                                         [opcode-mask-two-operand-arithmetic]         = #b1111000000000000
                                         [source-register-mask]                       = #b0000111100000000
                                         [ad-mask]                                    = #b0000000010000000
                                         [destination-register-mask]                  = #b0000000000001111
                                         [single-bit-mask]                            = #b0000000000000001))
                      ))
;                 (branch load-instruction-from-memory)))
