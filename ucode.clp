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
(defglobal MAIN
           ?*first* = 10000
           ?*last* = -10000
           ?*codeSegment* = "code"
           ?*dataSegment* = "data"
           ?*microcodeSegment* = "microcode"
           ?*callSegment* = "procedure"
           ?*stackSegment* = "stack")
(deftemplate stage
             (slot current
                   (type SYMBOL)
                   (default ?NONE))
             (multislot rest
                        (type SYMBOL)))

(deffacts stages 
          (stage (current setup-base)
                 (rest setup-custom)))
(defrule next-stage
         (declare (salience ?*last*))
         ?f <- (stage (rest ?next $?rest))
         =>
         (modify ?f 
                 (current ?next)
                 (rest ?rest)))


(deffunction setSegment 
             "Set the current segment"
             (?seg)
             (format t ".%s%n" ?seg))

(deffunction .code
             "set the current segment to be the code segment"
             ()
             (setSegment ?*codeSegment*))
(deffunction .data
             ()
             (setSegment ?*dataSegment*))
(deffunction .microcode
             ()
             (setSegment ?*microcodeSegment*))
(deffunction .procedure
             ()
             (setSegment ?*callSegment*))
(deffunction .stack
             ()
             (setSegment ?*stackSegment*))
(deffunction .word
             (?value)
             (printout t tab .word " " ?value crlf))
(deffunction hex-representation
             (?number)
             (format nil "#x%x" ?number))
(deffunction .string 
             (?str)
             (bind ?tmpFile 
                   (gensym*))
             (bind ?path 
                   "/tmp/strings.ucode")
             (if (open ?path ?tmpFile "w") then
               (printout ?tmpFile ?str)
               (close ?tmpFile)
               (if (open ?path ?tmpFile "r")  then
                 ; now read it back in, char by char until we hit EOF
                 (while (!= (bind ?curr (get-char ?tmpFile)) -1) do
                        (.word (hex-representation ?curr)))
                 (.word #0)
                 (close ?tmpFile)
                 (remove ?path)
                 else
                 (printout werror "Couldn't open " ?path " for reading! It is needed to translate strings!" crlf)
                 (exit 1))
               else
               (printout werror "Couldn't open " ?path " for writing! It is needed to translate strings!" crlf)
               (exit 1)))
(defclass register
  (is-a USER)
  (slot index
        (type INTEGER)
        (access initialize-only)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler resolve primary))
(defmessage-handler register resolve primary
                    () 
                    (str-cat r ?self:index))
(deffacts registers
          (make-registers r 0 255))

(defrule build-registers
         (stage (current setup-base))
         ?f <- (make-registers ?prefix ?from ?to)
         =>
         (retract ?f)
         (loop-for-count (?i ?from ?to) do
                         (make-instance (sym-cat ?prefix ?i) of register 
                                        (index ?i))))


(defclass alias
  (is-a USER)
  (slot target
        (type INSTANCE)
        (allowed-classes register
                         alias)
        (storage local)
        (visibility public)
        (default ?NONE))
  (message-handler resolve primary))

(defmessage-handler alias resolve primary
                    ()
                    (send ?self:target 
                          resolve))

(defgeneric .alias
            "build a new alias")
(defmethod .alias
  ((?name SYMBOL 
          (not (instance-existp (symbol-to-instance-name ?name))))
   (?register register))
  (make-instance ?name of alias
                 (target ?register)))
(defmethod .alias
  ((?name SYMBOL 
          (instance-existp (symbol-to-instance-name ?name)))
   (?register register))
  (printout werror "ERROR: attempted to redefine alias " ?name crlf)
  (exit 2))
(deftemplate alias-range
             (slot prefix
                   (type SYMBOL)
                   (default ?NONE))
             (slot from
                   (type INTEGER)
                   (default ?NONE))
             (slot to
                   (type INTEGER)
                   (default ?NONE)))
(deffacts basic-aliases
          (aliases zero false -> r0)
          (aliases one true -> r1)
          (aliases micro-ip -> r2)
          (aliases micro-sp -> r3)
          (aliases micro-pred -> r4)
          (aliases micro-cp -> r5)
          (aliases ifp -> r6)
          (aliases two -> r7)
          (aliases three -> r8)
          (aliases four -> r9)
          (aliases five -> r10)
          (aliases six -> r11)
          (aliases seven -> r12)
          (aliases fifteen -> r13)
          (alias-range (prefix o)
                       (from 32)
                       (to 63))
          (alias-range (prefix i)
                       (from 64)
                       (to 127))
          (alias-range (prefix l)
                       (from 128)
                       (to 191)))

(defrule parse-alias:single
         (declare (salience 1))
         (stage (current setup-base))
         ?f <- (aliases ?curr -> ?target)
         =>
         (retract ?f)
         (if (instance-existp (symbol-to-instance-name ?curr)) then
           (printout werror "Attempted to redefine " ?curr crlf)
           (exit 3)
           else
           (make-instance ?curr of alias
                          (target (symbol-to-instance-name ?target)))))

(defrule parse-alias:multiple
         (stage (current setup-base))
         ?f <- (aliases ?curr $?rest -> ?target)
         =>
         (retract ?f)
         (assert (aliases $?rest -> ?target))
         (if (instance-existp (symbol-to-instance-name ?curr)) then
           (printout werror "Attempted to redefine " ?curr crlf)
           (exit 3)
           else
           (make-instance ?curr of alias
                          (target (symbol-to-instance-name ?target)))))

(defrule make-alias:range
         (stage (current setup-base))
         ?c <- (alias-range (prefix ?p)
                            (from ?f)
                            (to ?t))
         =>
         (retract ?c)
         (loop-for-count (?i ?f ?t) do
                         (make-instance (sym-cat ?p (- ?i ?f)) of alias
                                        (target (symbol-to-instance-name (sym-cat r ?i))))))

