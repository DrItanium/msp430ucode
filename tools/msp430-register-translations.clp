(deffacts donuts
          (process .alias pc -> r192
                   .alias m0 -> r192
                   .alias sp -> r193
                   .alias m1 -> r193
                   .alias status -> r194
                   .alias m2 -> r194
                   .alias cgen -> r195
                   .alias m3 -> r195
                   .alias m4 -> r196
                   .alias m5 -> r197
                   .alias m6 -> r198
                   .alias m7 -> r199
                   .alias m8 -> r200
                   .alias m9 -> r201
                   .alias m10 -> r202
                   .alias m11 -> r203
                   .alias m12 -> r204
                   .alias m13 -> r205
                   .alias m14 -> r206
                   .alias m15 -> r207
                   ; Processor special function registers (interrupt control registers)
                   ; in the description of the memory map these are byte address locations but we'll do some magic internally to use registers instead
                   ; at some point we could introduce optimizations to keep that data local in registers
                   .alias psfr0 -> r208
                   .alias psfr1 -> r209
                   .alias psfr2 -> r210
                   .alias psfr3 -> r211
                   .alias psfr4 -> r212
                   .alias psfr5 -> r213
                   .alias psfr6 -> r214
                   .alias psfr7 -> r215

                   ;-----------------------------------------------------------------------------
                   ; interal registers for decoding instructions
                   ;-----------------------------------------------------------------------------
                   .alias currinst -> r216	; full instruction
                   .alias prefix -> r217	; Instruction prefix (4-bits)
                   .alias opcode -> r218	; Corresponding opcode (if applicable)
                   ;-----------------------------------------------------------------------------
                   ; single operand arithmetic
                   ;-----------------------------------------------------------------------------
                   .alias bw -> r219		; Byte or Word?
                   .alias addr-mode -> r220 ; addressing mode
                   ;-----------------------------------------------------------------------------
                   ; Conditional jump; PC -> PC + 2 * offset
                   ;-----------------------------------------------------------------------------
                   .alias condition -> r221
                   .alias offset -> r222
                   ;-----------------------------------------------------------------------------
                   ; Two operand arithmetic
                   ;-----------------------------------------------------------------------------
                   .alias source -> r223
                   .alias register -> r223    ; used in single-operand arithmetic
                   .alias destination -> r224
                   .alias ad -> r225
                   ;-----------------------------------------------------------------------------
                   ; hardcoded instruction masks
                   ;-----------------------------------------------------------------------------
                   .alias prefix-mask -> r255
                   .alias opcode-mask-single-operand-arithmetic -> r254
                   .alias bw-mask -> r253
                   .alias as-mask -> r252
                   .alias condition-mask -> r251
                   .alias opcode-mask-two-operand-arithmetic -> r250
                   .alias ad-mask -> r249
                   .alias source-register-mask -> r248
                   .alias destination-register-mask -> r247
                   .alias signed-offset-mask -> r246
                   .alias register-single-operand-arithmetic-mask -> r245
                   .alias single-operand-arithmetic-check-mask -> r244
                   .alias single-bit-mask -> r243
                   ))

(defrule convert-old-school-aliases
         ?f <- (process .alias ?title -> ?register $?rest)
         =>
         (retract ?f)
         (printout t "(aliases " ?title " -> " ?register ")"crlf)
         (assert (process $?rest)))
(reset)
(run)
(exit)
         
