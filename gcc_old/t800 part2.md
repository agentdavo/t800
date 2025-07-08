;;------------------------------
;; umulsidi3
;;------------------------------

(define_expand "umulsidi3"
  [(set (match_operand:DI 0 "ABCreg_operand" "")
        (plus:DI
          (mult:DI (zero_extend:DI (match_operand:SI 1 "ABCreg_operand" ""))
                   (zero_extend:DI (match_operand:SI 2 "ABCreg_operand" "")))
          (zero_extend:DI (match_dup 3))))]
  ""
  "{operands[3] = force_reg (SImode, CONST0_RTX (SImode));
   /* to match _lmul */
}")

(define_insn "_lmul"
  [(set (match_operand:DI 0 "ABCreg_operand" "=a")
        (plus:DI
          (mult:DI (zero_extend:DI (match_operand:SI 1 "ABCreg_operand" "%b"))
                   (zero_extend:DI (match_operand:SI 2 "ABCreg_operand" "a")))
          (zero_extend:DI (match_operand:SI 3 "ABCreg_operand" "c"))))]
  "T800_DISTINCT_REGS (operands[1], operands[2])
   && T800_DISTINCT_REGS (operands[2], operands[3])
   && T800_DISTINCT_REGS (operands[1], operands[3])"
  "lmul"
  [(set (attr "popped_inputs") (const_int 14))])


;;------------------------------
;; divmodM4
;;------------------------------

/* This is available only for divsor == UNITS_PER_WORD. */

(define_expand "divmodsi4"
  [(parallel
    [(set (match_operand:SI 0 "ABCreg_operand" "")
          (div:SI (match_operand:SI 1 "ABCreg_operand" "")
                  (match_operand:SI 2 "nonmemory_operand" "")))
     (set (match_operand:SI 3 "ABCreg_operand" "")
          (mod:SI (match_dup 1)
                  (match_dup 2)))])]
  ""
  "{
  if (GET_CODE (operands[3]) != CONST_INT
      || INTVAL (operands[3]) != UNITS_PER_WORD)
    FAIL;
  /* to match _wcnt */
}")

(define_insn "_wcnt"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (div:SI (match_operand:SI 1 "ABCreg_operand" "0")
                (const_int 4)))
   (set (match_operand:SI 2 "ABCreg_operand" "=b")
        (mod:SI (match_dup 1)
                (const_int 4)))]
  ""
  "wcnt")


;;------------------------------
;; udivmodM4
;;------------------------------

(define_expand "udivmodsi4"
  [(parallel
    [(set (match_operand:SI 0 "ABCreg_operand" "")
          (truncate:SI
            (udiv:DI (match_operand:SI 1 "ABCreg_operand" "")
                     (zero_extend:DI
                       (match_operand:SI 2 "ABCreg_operand" "")))))
    (set (match_operand:SI 3 "ABCreg_operand" "")
         (truncate:SI (umod:DI (match_dup 1)
                               (zero_extend:DI (match_dup 2)))))])]
  ""
  "{
  operands[1] = convert_to_mode (DImode, operands[1], 1 /*unsigned_p*/);
  /* to match _ldiv */
}")

(define_insn "_ldiv"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (truncate:SI (udiv:DI (match_operand:DI 1 "ABCreg_operand" "b")
                              (zero_extend:DI
                                (match_operand:SI 2 "ABCreg_operand" "a")))))
   (set (match_operand:SI 3 "ABCreg_operand" "=b")
        (truncate:SI (umod:DI (match_dup 1)
                     (zero_extend:DI (match_dup 2)))))]
  "T800_DISTINCT_REGS (operands[1], operands[2])"
  "ldiv"
  [(set (attr "popped_inputs") (const_int 6))])


;;------------------------------
;; ashlM3
;;------------------------------
(define_expand "ashlsi3"
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (ashift:SI (match_operand:SI 1 "ABCreg_operand" "")
                   (match_operand:SI 2 "ABCreg_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (ashift:SI (match_operand:SI 1 "ABCreg_operand" "b")
                   (match_operand:SI 2 "ABCreg_operand" "a")))]
  "T800_DISTINCT_REGS (operands[1], operands[2])"
  "shl"
  [(set (attr "popped_inputs") (const_int 6))])

(define_expand "ashldi3"
  [(set (match_operand:DI 0 "ABCreg_operand" "")
        (ashift:DI (match_operand:DI 1 "ABCreg_operand" "")
                   (match_operand:SI 2 "ABCreg_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:DI 0 "ABCreg_operand" "=a")
        (ashift:DI (match_operand:DI 1 "ABCreg_operand" "b")
                   (match_operand:SI 2 "ABCreg_operand" "a")))]
  "T800_DISTINCT_REGS (operands[1], operands[2])"
  "lshl"
  [(set (attr "popped_inputs") (const_int 6))])


;;------------------------------
;; ashrM3
;;------------------------------

/* We use whatever_operand here to obtain the right operand evaluation
   order; this tends to save us some reg-stack shuffling aferwards.  */

(define_expand "ashrsi3"
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (ashiftrt:SI (match_operand:SI 1 "whatever_operand" "")
                     (match_operand:SI 2 "whatever_operand" "")))]
  ""
  "{
     rtx tmp = gen_reg_rtx(DImode);

     operands[1] = convert_to_mode (DImode, operands[1], 0/*!unsigned_p*/);

     if (! ABCreg_operand (operands[1], DImode))
       operands[1] = force_reg (DImode, operands[1]);

     if (! ABCreg_operand (operands[2], SImode))
       operands[2] = force_reg (SImode, operands[2]);

     /* Tried to avoid the final move insn by getting the output of
        _lshr into (SUBREG:DI operands[0]). This fails, since operands[0]
        is a SImode pseudo and gets a single hard register only; it seems
        illegal to assign to it in DImode (?).  */

     emit_insn (gen_ashrdi3 (tmp, operands[1], operands[2]));
     emit_move_insn (operands[0], gen_rtx (SUBREG, SImode, tmp, 0));
     DONE;
}")

(define_expand "ashrdi3"
  [(set (match_operand:DI 0 "ABCreg_operand" "")
        (lshiftrt:DI (match_operand:DI 1 "ABCreg_operand" "")
                     (match_operand:SI 2 "ABCreg_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:DI 0 "ABCreg_operand" "=a")
        (lshiftrt:DI (match_operand:DI 1 "ABCreg_operand" "b")
                     (match_operand:SI 2 "ABCreg_operand" "a")))]
  "T800_DISTINCT_REGS (operands[1], operands[2])"
  "lshr"
  [(set (attr "popped_inputs") (const_int 6))])

;;------------------------------
;; lshlM3
;;------------------------------
;; This is the same as ashlM3.  Needs not be defined if negative shift
;; count is not allowed.

;;------------------------------
;; lshrM3
;;------------------------------

(define_expand "lshrsi3"
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (lshiftrt:SI (match_operand:SI 1 "ABCreg_operand" "")
                     (match_operand:SI 2 "ABCreg_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (lshiftrt:SI (match_operand:SI 1 "ABCreg_operand" "b")
                     (match_operand:SI 2 "ABCreg_operand" "a")))]
  "T800_DISTINCT_REGS (operands[1], operands[2])"
  "shr"
  [(set (attr "popped_inputs") (const_int 6))])


;;------------------------------
;; rotlM3
;; rotrM3
;;------------------------------
(define_expand "rotlsi3"
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (rotate:SI (match_operand:SI 1 "whatever_operand" "")
                   (match_operand:SI 2 "whatever_operand" "")))]
  ""
  "{
     rtx tmp = gen_reg_rtx(DImode);

     /* Evaluate operands in the desired order */
     emit_move_insn (gen_rtx (SUBREG, SImode, tmp, 1), operands[1]);
     emit_move_insn (gen_rtx (SUBREG, SImode, tmp, 0), GEN_INT (0));

     if (! ABCreg_operand (operands[2], SImode))
       operands[2] = force_reg (SImode, operands[2]);

     emit_insn (gen_ashldi3 (tmp, tmp, operands[2]));
     emit_insn (gen_iorsi3 (operands[0], gen_rtx (SUBREG, SImode, tmp, 0),
                                         gen_rtx (SUBREG, SImode, tmp, 1)));
     DONE;
}")

(define_expand "rotrsi3"
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (rotatert:SI (match_operand:SI 1 "whatever_operand" "")
                     (match_operand:SI 2 "whatever_operand" "")))]
  ""
  "{
     rtx tmp = gen_reg_rtx(DImode);

     operands[1] = convert_to_mode (DImode, operands[1], 1/*unsigned_p*/);

     if (! ABCreg_operand (operands[1], DImode))
       operands[1] = force_reg (DImode, operands[1]);
     if (! ABCreg_operand (operands[2], SImode))
       operands[2] = force_reg (SImode, operands[2]);

     emit_insn (gen_ashrdi3 (tmp, operands[1], operands[2]));
     emit_insn (gen_iorsi3 (operands[0], gen_rtx (SUBREG, SImode, tmp, 0),
                                         gen_rtx (SUBREG, SImode, tmp, 1)));
     DONE;
}")


;;------------------------------
;; negM2
;;------------------------------

(define_expand "negsi2"
  [(set (match_dup 2)
        (not:SI (match_operand:SI 1 "ABCreg_operand" "")))
   (set (match_operand:SI 0 "ABCreg_operand" "")
        (plus:SI (match_dup 2)
                 (const_int 1)))]
  ""
  "operands[2] = gen_reg_rtx (SImode);
   /* to match `not; adc 1' */
")

/* Floating point negation.  It is now implemented as (0 - x), and so
   requires the operand to be in an fp register.

   ??? We could also do it with XOR, if the operand is in an integer
   reg or in memory.  */

(define_expand "negsf2"
  [(set (match_dup 2)
        (match_dup 3))
   (set (match_operand:SF 0 "FABCreg_operand" "")
        (minus:SF (match_dup 2)
                (match_operand:SF 1 "FABCreg_operand" "")))]
  "TARGET_HAVE_FPU"
  "operands[2] = gen_reg_rtx (SFmode);
   operands[3] = CONST0_RTX (SFmode);
   /* to match `fpldzerosn; fpsub' */
")

(define_expand "negdf2"
  [(set (match_dup 2)
        (match_dup 3))
   (set (match_operand:DF 0 "FABCreg_operand" "")
        (minus:DF (match_dup 2)
                (match_operand:DF 1 "FABCreg_operand" "")))]
  "TARGET_HAVE_FPU"
  "operands[2] = gen_reg_rtx (DFmode);
   operands[3] = CONST0_RTX (DFmode);
   /* to match `fpldzerosn; fpsub' */
")


;;------------------------------
;; absM2
;;------------------------------

(define_insn "abssf3"
  [(set (match_operand:SF 0 "FABCreg_operand" "")
        (abs:SF (match_operand:SF 1 "FABCreg_operand" "")))]
  "TARGET_HAVE_FPU"
  "")

(define_insn "absdf3"
  [(set (match_operand:DF 0 "FABCreg_operand" "")
        (abs:DF (match_operand:DF 1 "FABCreg_operand" "")))]
  "TARGET_HAVE_FPU"
  "")

(define_insn ""
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (abs:SF (match_operand:SF 1 "FABCreg_operand" "0")))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_HAVE_FPU && TARGET_HAVE_FPENTRY"
  "fpuabs")

(define_insn ""
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (abs:SF (match_operand:SF 1 "FABCreg_operand" "0")))]
  "TARGET_HAVE_FPU && ! TARGET_HAVE_FPENTRY"
  "fpabs")

(define_insn ""
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (abs:DF (match_operand:DF 1 "FABCreg_operand" "0")))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_HAVE_FPU && TARGET_HAVE_FPENTRY"
  "fpuabs")

(define_insn ""
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (abs:DF (match_operand:DF 1 "FABCreg_operand" "0")))]
  "TARGET_HAVE_FPU && ! TARGET_HAVE_FPENTRY"
  "fpabs")


;;------------------------------
;; sqrtM2
;;------------------------------

(define_expand "sqrtsf3"
  [(set (match_operand:SF 0 "FABCreg_operand" "")
        (sqrt:SF (match_operand:SF 1 "FABCreg_operand" "")))]
  "TARGET_HAVE_FPU"
  "if (TARGET_HAVE_FPENTRY) {
     emit_insn (gen__fpusqrt_sequence_sf (operands[0], operands[1]));
     DONE;
   }
")

(define_expand "sqrtdf3"
  [(set (match_operand:DF 0 "FABCreg_operand" "")
        (sqrt:DF (match_operand:DF 1 "FABCreg_operand" "")))]
  "TARGET_HAVE_FPU"
  "if (TARGET_HAVE_FPENTRY) {
     emit_insn (gen__fpusqrt_sequence_df (operands[0], operands[1]));
     DONE;
   }
")

(define_insn "_fpusqrt_sequence_sf"
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (sqrt:SF (match_operand:SF 1 "FABCreg_operand" "0")))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SF 4 "=&f"))
   (clobber (match_scratch:SF 5 "=&f"))]
  "TARGET_HAVE_FPU && TARGET_HAVE_FPENTRY"
  "fpusqrtfirst\;fpusqrtstep\;fpusqrtstep\;fpusqrtlast")

(define_insn ""
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (sqrt:SF (match_operand:SF 1 "FABCreg_operand" "0")))]
  "TARGET_HAVE_FPU && ! TARGET_HAVE_FPENTRY"
  "fpsqrt")

(define_insn "_fpusqrt_sequence_df"
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (sqrt:DF (match_operand:DF 1 "FABCreg_operand" "0")))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SF 4 "=&f"))
   (clobber (match_scratch:SF 5 "=&f"))]
  "TARGET_HAVE_FPU && TARGET_HAVE_FPENTRY"
  "fpusqrtfirst\;fpusqrtstep\;fpusqrtstep\;fpusqrtstep\;fpusqrtstep\;fpusqrtstep\;fpusqrtlast")

(define_insn ""
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (sqrt:DF (match_operand:DF 1 "FABCreg_operand" "0")))]
  "TARGET_HAVE_FPU && ! TARGET_HAVE_FPENTRY"
  "fpsqrt")


;;------------------------------
;; ffsM2
;;------------------------------

; bet this will never be used  :-)

(define_insn "ffssi2"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (ffs:SI (match_operand:DI 1 "ABCreg_operand" "a")))
   (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "bitrevword\;rev\;bitrevword\;norm\;diff\;rev\;adc 1\;ldc 65\;rem"
  [(set (attr "popped_inputs") (const_int 2))])

	
;;------------------------------
;; one_cmplM2
;;------------------------------

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (not:SI (match_operand:SI 1 "ABCreg_operand" "0")))]
  ""
  "not")

;;------------------------------
;; cmpM
;;------------------------------

/* For compare operations, we simply store the comparison operands and
   generate no insns.  The following bCC or sCC insn will output
   whatever is needed.  */

/* Compare in QImode can sometimes be better that convert to SImode
   plus compare in SImode.  For example, SImode unsigned comparisons
   are based on `ldiff', while in QImode a cheap `gt' is sufficient.
   Therefore we declare the QImode comparisons.  */

(define_expand "cmpqi"
  [(match_operand:QI 0 "general_operand" "")
   (match_operand:QI 1 "general_operand" "")]
  "TARGET_USE_cmpqi"
  "{
  t800_compare.op[0] = operands[0];
  t800_compare.op[1] = operands[1];
  t800_compare.fp = 0;
  DONE;
}")

(define_expand "cmphi"
  [(match_operand:HI 0 "general_operand" "")
   (match_operand:HI 1 "general_operand" "")]
  "TARGET_USE_cmpqi"
  "{
  t800_compare.op[0] = operands[0];
  t800_compare.op[1] = operands[1];
  t800_compare.fp = 0;
  DONE;
}")

(define_expand "cmpsi"
  [(match_operand:SI 0 "ABCreg_operand" "")
   (match_operand:SI 1 "nonmemory_operand" "")]
  ""
  "{
  t800_compare.op[0] = operands[0];
  t800_compare.op[1] = operands[1];
  t800_compare.fp = 0;
  DONE;
}")

(define_expand "cmpsf"
  [(match_operand:SF 0 "FABCreg_operand" "")
   (match_operand:SF 1 "FABCreg_operand" "")]
  "TARGET_HAVE_FPU"
  "{
  t800_compare.op[0] = operands[0];
  t800_compare.op[1] = operands[1];
  t800_compare.fp = 1;
  DONE;
}")

(define_expand "cmpdf"
  [(match_operand:DF 0 "FABCreg_operand" "")
   (match_operand:DF 1 "FABCreg_operand" "")]
  "TARGET_HAVE_FPU"
  "{
  t800_compare.op[0] = operands[0];
  t800_compare.op[1] = operands[1];
  t800_compare.fp = 1;
  DONE;
}")

;;------------------------------
;; tstM
;;------------------------------
;; Should not be defined as transputers do not have (cc0)

;;------------------------------
;; movstrM
;;------------------------------

(define_expand "movstrsi"
  [(set (match_operand:BLK 0 "general_operand" "")
        (match_operand:BLK 1 "general_operand" ""))
   (use (match_operand:SI 2 "general_operand" ""))
   (use (match_operand:SI 3 "" ""))]
  ""
  "{
    rtx src, dest, len;

    if (GET_CODE (operands[0]) != MEM || GET_CODE (operands[1]) != MEM)
      abort ();

    /* Don't use force_reg on addresses, because it wouldn't do
       anything with Wreg, while we need ABCreg */

    dest = XEXP (operands[0], 0);
    if (! ABCreg_operand (dest, VOIDmode))
	dest = copy_addr_to_reg (dest);

    src = XEXP (operands[1], 0);
    if (! ABCreg_operand (src, VOIDmode))
	src = copy_addr_to_reg (src);

    len = force_reg (SImode, operands[2]);

    emit_insn (gen__move (dest, src, len));
    DONE;
}")

(define_insn "_move"
  [(set (mem:BLK (match_operand:SI 0 "ABCreg_operand" "b"))
        (mem:BLK (match_operand:SI 1 "ABCreg_operand" "c")))
   (use (match_operand:SI 2 "ABCreg_operand" "a"))]
  ""
  "move"
  [(set (attr "popped_inputs") (const_int 7))])

;;------------------------------
;; cmpstrM
;;------------------------------
;; N/A

/* Conversions -----------------------------

Legend: 
    -   no conversion applies
    f   floatMN2
    fu  floatunsMN2
    x   fixMN2
    xu  fixunsMN2
    xt  fix_truncMN2
    xtu fixuns_truncMN2
    ft  ftruncM2
    t   truncMN
    e   extendMN
    z   zero-extendMN

All possible conversions:
        QI          SI          DI          SF          DF
QI      -           e,z         e,z         f           f
SI      t           -           e,z         f,fu        f,fu
DI      t           t           -           f,fu        f,fu
SF      x,xu,xt,xtu x,xu,xt,xtu x,xu,xt,xtu ft          e
DF      x,xu,xt,xtu x,xu,xt,xtu x,xu,xt,xtu t           ft

Available insns on transputer:
        QI          SI          DI          SF          DF
QI      -           e,(z)       -           -           -
SI      (t)         -           e,(z)       f,fu        f,fu
DI      -           (t)         -           (f)         (f)   
SF      -           x           (x)         ft          e
DF      -           x           (x)         t           ft

   e     (extendQISI):       xword
   e     (extendSIDI):       xdble
   t     (truncdfsf):        fpur64tor32
   e     (extendsfdf):       fpur32tor64
   ft    (ftrunc[sd]f):      fpint
   x     (fix[sd]fsi):       fpstnli32
   f     (floatsisf):        fpi32tor32
   f     (floatsidf):        fpi32tor64
   fu    (floatunssidf):     fpb32tor64
   (x)   (fix[sd]fdi):       <sequence>
   (f)   (floatdi[sd]f):     <sequence>
*/

;;------------------------------
;; floatMN2
;;------------------------------

/* Insns of this group on trabsputers are only able to take input from
   memory.  Therefore, when asked to take input from a register, we
   allocate a stack slot and copy the input register in there before
   the insn.  Thus, we have to have an expander for every insn of this
   group :-( */

(define_expand "floatsisf2"
  [(set (match_operand:SF 0 "FABCreg_operand" "")
        (float:SF (match_operand:SI 1 "general_operand" "")))]
  "TARGET_HAVE_FPU"
  "{
  rtx t800_temp_slot (enum machine_mode);
  rtx t800_force_nonlocal (rtx);
  
  if (! nonlocal_operand (operands[1], SImode))
    {
      if (GET_CODE (operands[1]) != MEM)
        {
          rtx temp = t800_temp_slot (SImode);
          emit_move_insn (temp, operands[1]);
          operands[1] = temp;
        }
      operands[1] = t800_force_nonlocal (operands[1]);
    }
  /* to match _fpi32tor32 */
}")

(define_insn "_fpi32tor32"
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (float:SF (match_operand:SI 1 "nonlocal_operand" "R")))]
  "TARGET_HAVE_FPU"
  "fpi32tor32"
  [(set (attr "popped_inputs") (const_int 2))])


(define_expand "floatsidf2"
  [(set (match_operand:DF 0 "FABCreg_operand" "")
        (float:DF (match_operand:SI 1 "general_operand" "")))]
  "TARGET_HAVE_FPU"
  "{
  rtx t800_temp_slot (enum machine_mode);
  rtx t800_force_nonlocal (rtx);
  
  if (! nonlocal_operand (operands[1], SImode))
    {
      if (GET_CODE (operands[1]) != MEM)
        {
          rtx temp = t800_temp_slot (SImode);
          emit_move_insn (temp, operands[1]);
          operands[1] = temp;
        }
      operands[1] = t800_force_nonlocal (operands[1]);
    }
  /* to match _fpi32tor64 */
}")

(define_insn "_fpi32tor64"
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (float:DF (match_operand:SI 1 "nonlocal_operand" "R")))]
  "TARGET_HAVE_FPU"
  "fpi32tor64"
  [(set (attr "popped_inputs") (const_int 2))])


(define_expand "floatdisf2"
  [(parallel
    [(set (match_operand:SF 0 "FABCreg_operand" "")
          (float:SF (match_operand:DI 1 "general_operand" "")))
     (clobber (match_scratch:SI 2 ""))
     (clobber (match_scratch:SF 3 ""))])]
  "TARGET_HAVE_FPU"
  "{
  rtx t800_temp_slot (enum machine_mode);
  rtx t800_force_nonlocal (rtx);
  
  if (! nonlocal_operand (operands[1], DImode))
    {
      if (GET_CODE (operands[1]) != MEM)
        {
          rtx temp = t800_temp_slot (DImode);
          emit_move_insn (temp, operands[1]);
          operands[1] = temp;
        }
      operands[1] = t800_force_nonlocal (operands[1]);
    }
  /* to match _INT64_to_REAL32 */
}")

/* INT64 to REAL32 sequence in Round-to-Nearest mode */

(define_insn ""
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (float:SF (match_operand:DI 1 "nonlocal_operand" "R")))
   (clobber (match_scratch:SI 2 "=&r"))
   (clobber (match_scratch:SF 3 "=&f"))]
  "TARGET_HAVE_FPU"
  "* return TARGET_HAVE_FPENTRY ?
     \"dup\;fpb32tor64\;fpunoround\;ldnlp 1\;fpi32tor64\;fpuexpinc32\;fpunoround\;fpadd\" :
     \"dup\;fpb32tor64\;ldnlp 1\;fpi32tor64\;fpexpinc32\;fpadddbsn\";"
  [(set (attr "popped_inputs") (const_int 2))])

(define_expand "floatdidf2"
  [(parallel
    [(set (match_operand:DF 0 "FABCreg_operand" "")
          (float:DF (match_operand:DI 1 "general_operand" "")))
     (clobber (match_scratch:SI 2 ""))
     (clobber (match_scratch:SF 3 ""))])]
  "TARGET_HAVE_FPU"
  "{
  rtx t800_temp_slot (enum machine_mode);
  rtx t800_force_nonlocal (rtx);
  
  if (! nonlocal_operand (operands[1], DImode))
    {
      if (GET_CODE (operands[1]) != MEM)
        {
          rtx temp = t800_temp_slot (DImode);
          emit_move_insn (temp, operands[1]);
          operands[1] = temp;
        }
      operands[1] = t800_force_nonlocal (operands[1]);
    }
  /* to match _INT64_to_REAL64 */
}")


/* INT64 to REAL64 sequence in Round-to-Nearest mode */

(define_insn "_INT64_to_REAL64"
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (float:DF (match_operand:DI 1 "nonlocal_operand" "R")))
   (clobber (match_scratch:SI 2 "=&r"))
   (clobber (match_scratch:SF 3 "=&f"))]
  "TARGET_HAVE_FPU"
  "* return TARGET_HAVE_FPENTRY ?
     \"dup\;fpb32tor64\;ldnlp 1\;fpi32tor64\;fpuexpinc32\;fpadd\" :
     \"dup\;fpb32tor64\;ldnlp 1\;fpi32tor64\;fpexpinc32\;fpadd\";"
  [(set (attr "popped_inputs") (const_int 2))])


;;------------------------------
;; floatunsMN2
;;------------------------------

(define_expand "floatunssidf2"
  [(set (match_operand:DF 0 "FABCreg_operand" "")
        (unsigned_float:DF (match_operand:SI 1 "general_operand" "")))]
  "TARGET_HAVE_FPU"
  "{
  rtx t800_temp_slot (enum machine_mode);
  rtx t800_force_nonlocal (rtx);
  
  if (! nonlocal_operand (operands[1], SImode))
    {
      if (GET_CODE (operands[1]) != MEM)
        {
          rtx temp = t800_temp_slot (SImode);
          emit_move_insn (temp, operands[1]);
          operands[1] = temp;
        }
      operands[1] = t800_force_nonlocal (operands[1]);
    }
  /* to match _fpb32tor64 */
}")

(define_insn "_fpb32tor64"
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (unsigned_float:DF (match_operand:SI 1 "nonlocal_operand" "R")))]
  "TARGET_HAVE_FPU"
  "fpb32tor64"
  [(set (attr "popped_inputs") (const_int 2))])


;;------------------------------
;; fixMN2
;;------------------------------

/* Insns of this group on transputers are only able to output to memory.
   Therefore, when asked to generate result in a register, we use a
   dedicated stack slot to perform conversion, and then copy the
   result into the specified register.  */

(define_expand "fixsfsi2"
  [(match_operand:SI 0 "general_operand" "")
   (match_operand:SF 1 "FABCreg_operand" "")]
  "TARGET_HAVE_FPU"
  "{
  rtx t800_temp_slot (enum machine_mode);
  rtx t800_force_nonlocal (rtx);

  rtx dest = operands[0];
  
  if (GET_CODE (dest) != MEM)
    dest = t800_temp_slot (SImode);

  emit_insn (gen__fpstnli32 (t800_force_nonlocal (dest), operands[1]));

  if (dest != operands[0])
    emit_move_insn (operands[0], dest);

  DONE;
}")

(define_expand "fixdfsi2"
  [(match_operand:SI 0 "general_operand" "")
   (match_operand:DF 1 "FABCreg_operand" "")]
  "TARGET_HAVE_FPU"
  "emit_insn (gen_fixsfsi2 (operands[0], operands[1])); DONE;")

(define_expand "fixsfdi2"
  [(match_operand:DI 0 "general_operand" "")
   (match_operand:SF 1 "FABCreg_operand" "")]
  "TARGET_HAVE_FPU"
  "{
  rtx t800_temp_slot (enum machine_mode);
  rtx t800_force_nonlocal (rtx);

  rtx dest = operands[0];
  
  if (GET_CODE (dest) != MEM)
    dest = t800_temp_slot (DImode);

  emit_insn (gen__REAL_to_INT64 (t800_force_nonlocal (dest), operands[1]));

  if (dest != operands[0])
    emit_move_insn (operands[0], dest);

  DONE;
}")

(define_expand "fixdfdi2"
  [(match_operand:DI 0 "general_operand" "")
   (match_operand:DF 1 "FABCreg_operand" "")]
  "TARGET_HAVE_FPU"
  "emit_insn (gen_fixsfdi2 (operands[0], operands[1])); DONE;")


(define_insn "_fpstnli32"
  [(set (match_operand:SI 0 "nonlocal_operand" "=R")
        (fix:SI (match_operand:SF 1 "FABCreg_operand" "t")))]
  "TARGET_HAVE_FPU"
  "fpstnli32"
  [(set (attr "popped_inputs") (const_int 3))])

(define_insn ""
  [(set (match_operand:SI 0 "nonlocal_operand" "=R")
        (fix:SI (match_operand:DF 1 "FABCreg_operand" "t")))]
  "TARGET_HAVE_FPU"
  "fpstnli32"
  [(set (attr "popped_inputs") (const_int 3))])


(define_insn "_REAL_to_INT64"
  [(set (match_operand:DI 0 "nonlocal_operand" "=R")
        (fix:DI (match_operand:SF 1 "FABCreg_operand" "t")))
   (clobber (match_scratch:SF 2 "=&f"))
   (clobber (match_scratch:SI 3 "=&r"))]
  "TARGET_HAVE_FPU"
  "* return TARGET_HAVE_FPENTRY ?
    \"fpdup\;dup\;fpstnli32\;ldnlp 1\;fpuexpdec32\;fpstnli32\" :
    \"fpdup\;dup\;fpstnli32\;ldnlp 1\;fpexpdec32\;fpstnli32\";
")

(define_insn ""
  [(set (match_operand:DI 0 "nonlocal_operand" "=R")
        (fix:DI (match_operand:DF 1 "FABCreg_operand" "t")))
   (clobber (match_scratch:SF 2 "=&f"))
   (clobber (match_scratch:SI 3 "=&r"))]
  "TARGET_HAVE_FPU"
  "* return TARGET_HAVE_FPENTRY ?
   \"fpdup\;dup\;fpstnli32\;ldnlp 1\;fpuexpdec32\;fpstnli32\" :
   \"fpdup\;dup\;fpstnli32\;ldnlp 1\;fpexpdec32\;fpstnli32\";"
  [(set (attr "popped_inputs") (const_int 3))])


;;------------------------------
;; fixunsMN2
;;------------------------------
;; N/A

;;------------------------------
;; ftruncM2
;;------------------------------

/* Note that the RTL code <fix:xF> represents a "round-to-zero"
   conversion.  Therefore we should override the default rounding mode
   of transputers, which is "round-to-nearest".  */

(define_expand "ftruncsf2"
  [(set (match_operand:SF 0 "FABCreg_operand" "")
        (fix:SF (match_operand:SF 1 "FABCreg_operand" "")))]
  "TARGET_HAVE_FPU"
  "if (TARGET_HAVE_FPENTRY) {
     emit_insn (gen__fpurz_fpint_sf (operands[0], operands[1]));
     DONE;
   }
")

(define_expand "ftruncdf2"
  [(set (match_operand:DF 0 "FABCreg_operand" "")
        (fix:DF (match_operand:DF 1 "FABCreg_operand" "")))]
  "TARGET_HAVE_FPU"
  "if (TARGET_HAVE_FPENTRY) {
     emit_insn (gen__fpurz_fpint_df (operands[0], operands[1]));
     DONE;
   }
")

(define_insn "_fpurz_fpint_sf"
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (fix:SF (match_operand:SF 1 "FABCreg_operand" "0")))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_HAVE_FPU && TARGET_HAVE_FPENTRY"
  "fpurz\;fpint")

(define_insn "_fpurz_fpint_df"
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (fix:DF (match_operand:DF 1 "FABCreg_operand" "0")))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_HAVE_FPU && TARGET_HAVE_FPENTRY"
  "fpurz\;fpint")

(define_insn ""
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (fix:SF (match_operand:SF 1 "FABCreg_operand" "0")))]
  "TARGET_HAVE_FPU && ! TARGET_HAVE_FPENTRY"
  "fprz\;fpint")

(define_insn ""
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (fix:DF (match_operand:DF 1 "FABCreg_operand" "0")))]
  "TARGET_HAVE_FPU && ! TARGET_HAVE_FPENTRY"
  "fprz\;fpint")


;;------------------------------
;; fix_truncMN2
;; fixuns_truncMN2
;;------------------------------
;; N/A


;;------------------------------
;; truncMN2
;;------------------------------

(define_expand "truncdfsf2"
  [(set (match_operand:SF 0 "FABCreg_operand" "")
        (float_truncate:SF (match_operand:DF 1 "FABCreg_operand" "")))]
  "TARGET_HAVE_FPU"
  "if (TARGET_HAVE_FPENTRY) {
     emit_insn (gen__fpur64tor32 (operands[0], operands[1]));
     DONE;
   }
")

(define_insn "_fpur64tor32"
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (float_truncate:SF (match_operand:DF 1 "FABCreg_operand" "0")))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_HAVE_FPU && TARGET_HAVE_FPENTRY"
  "fpur64tor32")

(define_insn "_fpr64tor32"
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (float_truncate:SF (match_operand:DF 1 "FABCreg_operand" "0")))]
  "TARGET_HAVE_FPU && ! TARGET_HAVE_FPENTRY"
  "fpr64tor32")


;;------------------------------
;; extendMN
;;------------------------------

(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (sign_extend:SI (match_operand:QI 1 "general_operand" "")))]
  ""
  "if (! TARGET_HAVE_XTEND) {
     emit_insn (gen__ldc0x80_xword (operands[0], operands[1]));
     DONE;
   }
")

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (sign_extend:SI (match_operand:HI 1 "general_operand" "")))]
  ""
  "if (! TARGET_HAVE_XTEND) {
     if (TARGET_HAVE_SIXTEEN) {
       emit_insn (gen__ls_ldc0x8000_xword (operands[0], operands[1]));
     }
     else {
       emit_insn (gen__ldc0x8000_xword (operands[0], 
                                        force_ABCreg (HImode, operands[1])));
     }
     DONE;
   }
")

/* xword requires its argument to be zero-extended to operate
   correctly.  Real trap!  We don't have an easy way to tell if our
   input has been zero-extended already, so we zero-extend here to be
   safe.  Of course, we may end up zero-extending twice; the most
   common case where this can happen is after an `lb', which already
   zero-extends the byte.  We catch these common cases by providing
   special alternatives for them --- works pretty well.  We still get
   superfluous zero extending sometimes, especially when indexing
   signed char arrays (920501-14.c, t800-unknown-parix), but such
   cases are rare.  And modern processors (T450) don't have this
   problem at all, using xbword. */

(define_insn "_ldc0x80_xword"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a,a,a")
        (sign_extend:SI (match_operand:QI 1 "general_operand" "?a,S,R")))
   (clobber (match_scratch:SI 2 "=&r,&r,&r"))]
  "! TARGET_HAVE_XTEND"
  "@
   ldc 0xff\;and\;ldc 0x80\;xword
   ldlp %w1\;lb\;ldc 0x80\;xword
   lb\;ldc 0x80\;xword"
  [(set (attr "popped_inputs") (const_int 2))])

(define_insn "_ls_ldc0x8000_xword"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a,a,a")
        (sign_extend:SI (match_operand:HI 1 "general_operand" "?a,S,R")))
   (clobber (match_scratch:SI 2 "=&r,&r,&r"))]
  "! TARGET_HAVE_XTEND && TARGET_HAVE_SIXTEEN"
  "@
   ldc 0xffff\;and\;ldc 0x8000\;xword
   ldlp %w1\;ls\;ldc 0x8000\;xword
   ls\;ldc 0x8000\;xword"
  [(set (attr "popped_inputs") (const_int 2))])

(define_insn "_ldc0x8000_xword"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (sign_extend:SI (match_operand:HI 1 "ABCreg_operand" "0")))
   (clobber (match_scratch:SI 2 "=&r"))]
  "! TARGET_HAVE_XTEND && ! TARGET_HAVE_SIXTEEN"
  "ldc 0xffff\;and\;ldc 0x8000\;xword")

/* I think we can go with general_operand in insns which load
   something onto regstack: reload should be able to use Areg (which
   is the dest of the insn) for input reload, if one is necessary, so
   this sloppiness won't cause extra spills.  */

(define_insn "_xbword_lbx"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a,a")
        (sign_extend:SI (match_operand:QI 1 "general_operand" "?a,R")))]
  "TARGET_HAVE_XTEND"
  "@
   xbword
   lbx"
  [(set (attr "popped_inputs") (const_int 2))])

(define_insn "_xsword_lsx"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a,a")
        (sign_extend:SI (match_operand:HI 1 "general_operand" "?a,R")))]
  "TARGET_HAVE_XTEND && TARGET_HAVE_SIXTEEN"
  "@
   xsword
   lsx"
  [(set (attr "popped_inputs") (const_int 2))])


(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "ABCreg_operand" "=a")
        (sign_extend:DI (match_operand:SI 1 "ABCreg_operand" "a")))]
  ""
  "xdble"
  [(set (attr "popped_inputs") (const_int 2))])


(define_expand "extendsfdf2"
  [(set (match_operand:DF 0 "FABCreg_operand" "")
        (float_extend:DF (match_operand:SF 1 "FABCreg_operand" "")))]
  "TARGET_HAVE_FPU"
  "if (TARGET_HAVE_FPENTRY) {
     emit_insn (gen__fpur32tor64 (operands[0], operands[1]));
     DONE;
   }
")

(define_insn "_fpur32tor64"
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (float_extend:DF (match_operand:SF 1 "FABCreg_operand" "0")))
   (clobber (match_scratch:SI 2 "=&r"))]
  "TARGET_HAVE_FPU && TARGET_HAVE_FPENTRY"
  "fpur32tor64")

(define_insn "_fpr32tor64"
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (float_extend:DF (match_operand:SF 1 "FABCreg_operand" "0")))]
  "TARGET_HAVE_FPU && ! TARGET_HAVE_FPENTRY"
  "fpr32tor64")


;;------------------------------
;; zero_extendMN
;;------------------------------

(define_expand "zero_extendqisi2"
  [(parallel
    [(set (match_operand:SI 0 "ABCreg_operand" "")
        (zero_extend:SI (match_operand:QI 1 "general_operand" "")))
     (clobber (match_scratch:SI 2 ""))])]
  ""
  "{
    if (GET_CODE (operands[1]) == MEM
        && ! local_operand (operands[1], QImode))
      {
        /* Loading from memory in QImode (`lb') zero-extends the
           loaded byte.  `lb' wants memory address in an ABCreg; Wreg
           and suchlike won't do.  */

        rtx addr = XEXP (operands[1], 0);

        if (! ABCreg_operand (addr, SImode))
          addr = copy_addr_to_reg (addr);

        emit_insn (gen__lb (operands[0], gen_rtx (MEM, QImode, addr)));
        DONE;
      }
    /* The pattern just below will match */
  }")


/* This pattern is capable of using `lb' if the source register fails
   to get a hard reg.  Note that local operand ('S') is always
   word-aligned, so we don't need to go into "ldlp;adc" */

(define_insn ""
  [(set (match_operand:SI 0 "ABCreg_operand" "=a,a,a,a")
; ?? general_operand seems to give slightly worse code.
;        (zero_extend:SI (match_operand:QI 1 "general_operand" "R,S,U,a")))
        (zero_extend:SI (match_operand:QI 1 "ABCreg_or_local_operand" "R,S,U,a")))
   (clobber (match_scratch:SI 2 "=X,X,X,&r"))]
  ""
  "@
   lb
   ldlp %w1\;lb
   ldnlp %w1\;lb
   ldc 255\;and"
  [(set (attr "popped_inputs") (const_int 2))])

(define_expand "zero_extendhisi2"
   [(set (match_operand:SI 0 "ABCreg_operand" "")
;         (and:SI (subreg:SI (match_operand:HI 1 "ABCreg_operand" "") 0)
         (and:SI (match_operand:HI 1 "ABCreg_operand" "")
                 (match_dup 2)))]
  ""
  "/* Tried to put subreg in the pattern, but that produced multiple
      subregs if operand1 is already a subreg.  Let gen_lowpart do it right. */
   operands[1] = gen_lowpart (SImode, operands[1]);
   operands[2] = force_reg (SImode, GEN_INT (65535));")


;;------------------------------
;; extv
;; extzv
;; insv
;;------------------------------
;; N/A


;;------------------------------
;; sCOND
;;------------------------------

(define_expand "seq"
  [(match_operand:SI 0 "ABCreg_operand" "")]
  ""
  "t800_expand_scond (EQ, operands[0]); DONE;")

(define_expand "sne"
  [(match_operand:SI 0 "ABCreg_operand" "")]
  ""
  "t800_expand_scond (NE, operands[0]); DONE;")

(define_expand "sle"
  [(match_operand:SI 0 "ABCreg_operand" "")]
  ""
  "t800_expand_scond (LE, operands[0]); DONE;")

(define_expand "slt"
  [(match_operand:SI 0 "ABCreg_operand" "")]
  ""
  "t800_expand_scond (LT, operands[0]); DONE;")

(define_expand "sge"
  [(match_operand:SI 0 "ABCreg_operand" "")]
  ""
  "t800_expand_scond (GE, operands[0]); DONE;")

(define_expand "sgt"
  [(match_operand:SI 0 "ABCreg_operand" "")]
  ""
  "t800_expand_scond (GT, operands[0]); DONE;")

(define_expand "sleu"
  [(match_operand:SI 0 "ABCreg_operand" "")]
  ""
  "t800_expand_scond (LEU, operands[0]); DONE;")

(define_expand "sltu"
  [(match_operand:SI 0 "ABCreg_operand" "")]
  ""
  "t800_expand_scond (LTU, operands[0]); DONE;")

(define_expand "sgeu"
  [(match_operand:SI 0 "ABCreg_operand" "")]
  ""
  "t800_expand_scond (GEU, operands[0]); DONE;")

(define_expand "sgtu"
  [(match_operand:SI 0 "ABCreg_operand" "")]
  ""
  "t800_expand_scond (GTU, operands[0]); DONE;")


/*
 * DEFINE_INSNs for compaisons
 */

(define_insn "_eqc"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (eq:SI (match_operand:SI 1 "ABCreg_operand" "0")
               (match_operand:SI 2 "const_int_operand" "")))]
  ""
  "eqc %2")


(define_insn "_gt"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (gt:SI (match_operand:SI 1 "ABCreg_operand" "b")
               (match_operand:SI 2 "ABCreg_operand" "a")))]
  "T800_DISTINCT_REGS (operands[1], operands[2])"
  "gt"
  [(set (attr "popped_inputs") (const_int 6))])

(define_insn "_gtu"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (gtu:SI (match_operand:SI 1 "ABCreg_operand" "b")
                (match_operand:SI 2 "ABCreg_operand" "a")))]
  "TARGET_HAVE_GTU
   && T800_DISTINCT_REGS (operands[1], operands[2])"
  "gtu"
  [(set (attr "popped_inputs") (const_int 6))])

/* Substract with borrow.  Used in implementation of unsigned
   comparisons on transputers that lack the `gtu' instruction */

(define_insn "_ldiff"
  [(set (match_operand:SI 0 "ABCreg_operand" "=b")
        (ltu:DI
          (zero_extend:DI (match_operand:SI 2 "ABCreg_operand" "b"))
          (plus:DI
            (zero_extend:DI (match_operand:SI 3 "ABCreg_operand" "a"))
            (zero_extend:DI (and:SI
                              (match_operand:SI 4 "ABCreg_operand" "c")
                              (const_int 1))))))
   (set (match_operand:SI 1 "ABCreg_operand" "=a")
        (truncate:SI
          (minus:DI (zero_extend:DI (match_dup 2))
                    (plus:DI (zero_extend:DI (match_dup 3))
                             (zero_extend:DI (and:SI (match_dup 4)
                                                      (const_int 1)))))))]
  "T800_DISTINCT_REGS (operands[2], operands[3])
   && T800_DISTINCT_REGS (operands[3], operands[4])
   && T800_DISTINCT_REGS (operands[2], operands[4])"
  "ldiff"
  [(set (attr "popped_inputs") (const_int 28))])


/* Used to have (eq:SF ...) here, but it would produce unrecognizable
   insn for gen__fpeq (result, DF_operand1, DF_operand2); */

(define_insn "_fpeq"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (eq (match_operand:SF 1 "FABCreg_operand" "%u")
            (match_operand:SF 2 "FABCreg_operand" "t")))]
  "TARGET_HAVE_FPU && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fpeq"
  [(set (attr "popped_inputs") (const_int 6))])

(define_insn ""
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (eq (match_operand:DF 1 "FABCreg_operand" "%u")
            (match_operand:DF 2 "FABCreg_operand" "t")))]
  "TARGET_HAVE_FPU && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fpeq"
  [(set (attr "popped_inputs") (const_int 6))])


(define_insn "_fpgt"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (gt (match_operand:SF 1 "FABCreg_operand" "u")
            (match_operand:SF 2 "FABCreg_operand" "t")))]
  "TARGET_HAVE_FPU && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fpgt"
  [(set (attr "popped_inputs") (const_int 6))])

(define_insn ""
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (gt (match_operand:DF 1 "FABCreg_operand" "u")
            (match_operand:DF 2 "FABCreg_operand" "t")))]
  "TARGET_HAVE_FPU && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fpgt"
  [(set (attr "popped_inputs") (const_int 6))])


(define_insn "_fpge"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (ge (match_operand:SF 1 "FABCreg_operand" "u")
            (match_operand:SF 2 "FABCreg_operand" "t")))]
  "TARGET_HAVE_FPU
   && TARGET_HAVE_FPGE
   && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fpge"
  [(set (attr "popped_inputs") (const_int 6))])

(define_insn ""
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (ge (match_operand:DF 1 "FABCreg_operand" "u")
            (match_operand:DF 2 "FABCreg_operand" "t")))]
  "TARGET_HAVE_FPU
   && TARGET_HAVE_FPGE
   && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fpge"
  [(set (attr "popped_inputs") (const_int 6))])


;;------------------------------
;; bCOND
;;------------------------------

(define_expand "beq"
  [(match_operand 0 "" "")]
  ""
  "t800_expand_bcond (EQ, operands[0]); DONE;")

(define_expand "bne"
  [(match_operand 0 "" "")]
  ""
  "t800_expand_bcond (NE, operands[0]); DONE;")

(define_expand "ble"
  [(match_operand 0 "" "")]
  ""
  "t800_expand_bcond (LE, operands[0]); DONE;")

(define_expand "blt"
  [(match_operand 0 "" "")]
  ""
  "t800_expand_bcond (LT, operands[0]); DONE;")

(define_expand "bge"
  [(match_operand 0 "" "")]
  ""
  "t800_expand_bcond (GE, operands[0]); DONE;")

(define_expand "bgt"
  [(match_operand 0 "" "")]
  ""
  "t800_expand_bcond (GT, operands[0]); DONE;")

(define_expand "bleu"
  [(match_operand 0 "" "")]
  ""
  "t800_expand_bcond (LEU, operands[0]); DONE;")

(define_expand "bltu"
  [(match_operand 0 "" "")]
  ""
  "t800_expand_bcond (LTU, operands[0]); DONE;")

(define_expand "bgeu"
  [(match_operand 0 "" "")]
  ""
  "t800_expand_bcond (GEU, operands[0]); DONE;")

(define_expand "bgtu"
  [(match_operand 0 "" "")]
  ""
  "t800_expand_bcond (GTU, operands[0]); DONE;")


/*
 * DEFINE_INSNs for conditional branches
 */

(define_insn "_cj"
  [(set (pc) (if_then_else (eq:SI (match_operand:SI 1 "ABCreg_operand" "a")
                                  (const_int 0))
                           (label_ref (match_operand 0 "" ""))
                           (pc)))]
  ""
  "cj %l0"
  [(set (attr "popped_inputs") (const_int 2))
   (set (attr "popped_inputs_on_jump") (const_int 0))])

/* Reverse-conditional branch pattern *must* be defined for
   invert_jump() to work properly. */

(define_insn "_inv_cj"
  [(set (pc) (if_then_else (eq:SI (match_operand:SI 1 "ABCreg_operand" "a")
                                  (const_int 0))
                           (pc)
                           (label_ref (match_operand 0 "" ""))))]
  ""
  "eqc 0\;cj %l0"
  [(set (attr "popped_inputs") (const_int 2))
   (set (attr "popped_inputs_on_jump") (const_int 0))])

;;------------------------------
;; call
;; call_value
;;------------------------------

/* operands[0] is the function to call
   operands[1] is the number of args passed on stack;
   operands[2] is next_arg_reg.
   operands[3] is stack_size_rtx  */

(define_expand "call"
  [(match_operand:QI 0 "memory_operand" "")
   (match_operand:SI 1 "" "")
   (match_operand:SI 2 "" "")
   (match_operand:SI 3 "" "")]
  ""
  "t800_expand_call(NULL_RTX, operands[0], operands[1], operands[2],
                    operands[3]); DONE;")

(define_expand "call_value"
; Operand 0 may be any register, including floating ones.
  [(match_operand  0 "register_operand" "")
   (match_operand:QI 1 "memory_operand" "")
   (match_operand:SI 2 "" "")
   (match_operand:SI 3 "" "")
   (match_operand:SI 4 "" "")]
  ""
  "t800_expand_call(operands[0], operands[1], operands[2], operands[3],
                    operands[4]); DONE;")

(define_insn "_call"
  [(set (match_operand 0 "ABCreg_operand" "=a")
        (call (mem:QI (match_operand:SI 1 "immediate_operand" ""))
              (match_operand:SI 2 "" "")))]
  ""
  "*
#ifdef T800_OUTPUT__CALL
  T800_OUTPUT__CALL;
#else
  return \"call %1\";
#endif
")

(define_insn ""
  [(set (match_operand 0 "FABCreg_operand" "=t")
        (call (mem:QI (match_operand:SI 1 "immediate_operand" ""))
              (match_operand:SI 2 "" "")))]
  ""
  "*
#ifdef T800_OUTPUT__CALL
  T800_OUTPUT__CALL;
#else
  return \"call %1\";
#endif
")

/* Unlike `call', `gcall' does not save the return address on the
   stack.  Therefore we do this by hand: calculate the return address
   and push it onto the stack.

   Further, one cannot pass args on registers to gcall, because this
   would leave no room for function address.  FUNCTION_ARG arranges
   for args being passed on stack; but this has its caveat.  When
   callee returns with `ret', it will pop return address and *three
   more words* off the stack; so we have to `ajw' after the call to
   compensate for this.  (Looks strange when this `ajw' happens just
   before `ajw +NNN; ret'.  Use peephole?)

   Note: sizeof(stl 0)==1, sizeof(gcall)==1, hence `ldc 2'.  We could
   do this reliably with labels, but some assemblers (Parix) don't
   handle label substraction well.

   `unspec' in `gcall' pattern makes it differ from the `call' pattern,
   so optimizers won't substitute the latter for the former when gcall
   is used with symbol_ref operand: such substitution would be wrong
   because `call' pushes registers, `gcall' does not.  */

(define_insn "_gcall"
  [(set (match_operand 0 "ABCreg_operand" "=a,a")
        (call (mem:QI (match_operand:SI 1 "nonmemory_operand" "a,i"))
              (match_operand:SI 2 "" "")))
   (unspec [(const_int 0)] 1)]
  ""
  "*
#ifdef T800_OUTPUT__GCALL
  T800_OUTPUT__GCALL;
#else
  return which_alternative ? \"ajw -1\;ldc 2\;ldpi\;stl 0\;j %1\;ajw -3\"
                           : \"ajw -1\;ldc 2\;ldpi\;stl 0\;gcall\;ajw -3\";
#endif
"
  [(set (attr "popped_inputs") (const_int 2))])

(define_insn ""
  [(set (match_operand 0 "FABCreg_operand" "=t,t")
        (call (mem:QI (match_operand:SI 1 "nonmemory_operand" "a,i"))
              (match_operand:SI 2 "" "")))
   (unspec [(const_int 0)] 1)]
  ""
  "*
#ifdef T800_OUTPUT__GCALL
  T800_OUTPUT__GCALL;
#else
  return which_alternative ? \"ajw -1\;ldc 2\;ldpi\;stl 0\;j %1\;ajw -3\"
                           : \"ajw -1\;ldc 2\;ldpi\;stl 0\;gcall\;ajw -3\";
#endif
"
  [(set (attr "popped_inputs") (const_int 2))])

/* _gcall_aggregate is a variation of normal _gcall used for models
   which pass struct return address in Areg, so that function address
   has to come in Breg instead. */

(define_insn "_gcall_aggregate"
  [(set (match_operand 0 "ABCreg_operand" "=a,a")
        (call:CC (mem:QI (match_operand:SI 1 "nonmemory_operand" "b,i"))
                 (match_operand:SI 2 "" "")))
   (unspec [(const_int 0)] 2)]
  ""
  "*
#ifdef T800_OUTPUT__GCALL
  T800_OUTPUT__GCALL;
#else
  return which_alternative ? \"ajw -1\;ldc 2\;ldpi\;stl 0\;j %1\;ajw -3\"
                           : \"ajw -1\;ldc 2\;ldpi\;stl 0\;gcall\;ajw -3\";
#endif
"
  [(set (attr "popped_inputs") (const_int 2))])


;;------------------------------
;; call_pop
;; call_value_pop
;;------------------------------
;; N/A


;;------------------------------
;; return
;;------------------------------

/* We have to use `return' pattern rather than jump to epilogue, as
   the jump insn clobbers all stack regs, including the one that
   carries a return value.  */

(define_insn "return"
  [(return)]
  ""
  "*
  T800_OUTPUT_RETURN;
")

;;------------------------------
;; nop
;;------------------------------

(define_insn "nop"
  [(const_int 0)]
  ""
  "*
#ifdef T800_OUTPUT_NOP
  T800_OUTPUT_NOP;
#else
  return \"pfix 0\";
#endif
")

;;------------------------------
;; jump
;;------------------------------

/* The `j' insn may cause descheduling of current process, which does
   not preserve any reg besides Iptr and Wreg. */

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))
   (clobber (reg:SI 0))
   (clobber (reg:SI 1))
   (clobber (reg:SI 2))
   (clobber (reg:SF 3))
   (clobber (reg:SF 4))
   (clobber (reg:SF 5))]
  ""
  "j %l0")

;;------------------------------
;; indirect_jump
;;------------------------------

/* JUMP_INSNs are not allowed to have output reloads. To be compliant
   with this requirement, we have to use

     (clobber (match_dup 0))  rather than
     (set (match_dup 0) (pc))

   The old value of (pc) isn't likely to be used anyway.  */

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "ABCreg_operand" "a"))
   (clobber (match_dup 0))]
  ""
  "gcall")

;;------------------------------
;; casesi
;;------------------------------

/* Some points to explain why tablejumping is not so easy on transputers.

   1. We cannot use absolute casetables because on Parix there are no
      load-time fixups: by definition .word Lxxx initializes the word
      with the *offset* from the beginning of the text segment to
      Lxxx, not with the absolute address of Lxxx.  So we must use
      relative casetables, and have `ldpi' in tablejump or equivalent
      to convert the offset fetched from the table into an address to
      `gcall' to.

   2. We must insert .align 4 before the table because word accesses
      only work at a word boundary.  Therefore we have a gap of
      uncertain length between `ldpi;gcall' and the table label.
      So the distance from ldpi to table_label is nonconstant (until
      assembling, of course, but that doesn't help).

   3. We could use `adc ldpi_label-table_label' before lpdi to
      compensate for the gap.  But Parix assembler dives a warning on
      label difference arguments to adc.

   4. Output reloads can break neighbourhood of ldpi and the
      code_label emitted just after it, if the label is made a
      separate insn.

   We use casesi instead of tablejump to be able to do a trick.  We
   want to use two labels instead of single table_label.  One label
   (table_label) is placed just before the table as usual, and is used
   to access the table.  The other label (table_rel_label) is the label
   which offsets are relative to, and it is placed after a ldpi in the
   jumping code.

   This two-label scheme saves us an adc -- in addition, an adc with a
   label-difference argument which causes a warning (!?) on certain
   assemblers... Parix, right.

   Here is what we want to generate:

        [normalize index, check bounds, scale to word offset,
         compute table elt address and then...]
	ldnl 0
        ldpi
table_rel_label:
        gcall
        .align 4
table_label:
        .word Lcase1-table_rel_label
        .word Lcase2-table_rel_label
	...

   Note that table_rel_label does not exist as a separate insn, but is
   generated as a permutation of table_label in final pass.  (Making
   table_rel_label a separate insn is not good because this would cut
   the tablejumping code in two separate basic blocks, which hinders
   optimization).  But hiding the table_rel_label, in turn, poses a
   problem: on some patological programs (920917-1.c is an example)
   optimizers may delete the tablejump insn (which is responsible for
   outputting table_rel_label) but leaving the table intact.  The
   resulting code is not assemblable because the table refers to
   table_rel_label, which does not exist.

   We work around this problem by having the tablejump output routine
   recording the label number in t800_expect_table_label, and
   suppressing ASM_OUTPUT_ADDR_DIFF_ELT if the recorded label number
   doesn't match one of the table being output.  The proper fix would
   be to make GCC optimize away either both table jump and the table,
   or none of them... but I don't feel brave enough at the moment. ;-)  */

(define_expand "casesi"
  [(match_operand:SI 0 "ABCreg_operand" "a")	; index
   (match_operand:SI 1 "const_int_operand" "")	; lower bound
   (match_operand:SI 2 "const_int_operand" "")	; highest index in table
   (match_operand:SI 3 "" "")			; table_label
   (match_operand:SI 4 "" "")]			; default_label
  ""
  "{
#define INDEX operands[0]
#define LOWER_BOUND operands[1]
#define HIGHEST_INDEX operands[2]
#define TABLE_LABEL operands[3]
#define DEFAULT_LABEL operands[4]

  rtx table_index = gen_reg_rtx (SImode);

  emit_insn (gen__adc (table_index, INDEX, GEN_INT (- INTVAL (LOWER_BOUND))));

  /* mimic two lines from do_tablejump */
  emit_cmp_insn (table_index, HIGHEST_INDEX, GTU, NULL_RTX, SImode, 1, 0);
  emit_jump_insn (gen_bgtu (DEFAULT_LABEL));

  emit_jump_insn (gen__my_fancy_tablejump (table_index, TABLE_LABEL));

  DONE;
#undef INDEX
#undef LOWER_BOUND
#undef HIGHEST_INDEX
#undef TABLE_LABEL
#undef DEFAULT_LABEL
}")

(define_insn "_my_fancy_tablejump"
  [(set (pc)
        (plus:SI (label_ref:SI (match_operand 1 "" ""))
                 (mem:SI
                   (plus:SI (label_ref:SI (match_dup 1))
                            (mult:SI (match_operand:SI 0 "ABCreg_operand" "a")
                                     (const_int 4))))))
; The clobber below would help us to be 100% correct: this insn
; actually replaces its arg with... hmmm... some other value, and we
; should let the compiler know about this.  But adding this clobber
; scares cse and it fails to optimize the switching code in the case
; of index expression being a constant.  Therefore I leave the clobber
; out; I cannot imagine a situation where the compiler could try to
; reuse the table index which is allegedly left in Areg.
;   (clobber (match_dup 0))

   (clobber (match_scratch:SI 2 "=&r"))
  ]
  ""
  "*{
  /* Leave a note for ASM_OUTPUT_ADDR_DIFF_ELT */
  extern int t800_expected_table_label;
  t800_expected_table_label = CODE_LABEL_NUMBER (operands[1]);

#ifdef T800_LDC_SYMBOL_PC_RELATIVE
  return \"ldc %1-2\;ldpi\;wsub\;ldnl 0\;ldpi\\n%1a:\;gcall\";
#else
  return \"ldc %1-LF%=\;ldpi\\nLF%=:\;wsub\;ldnl 0\;ldpi\\n%1a:\;gcall\";
#endif
}")

;;------------------------------
;; tablejump
;;------------------------------

; we use casesi now

;;------------------------------
;; allocate_stack
;;------------------------------

(define_expand "allocate_stack"
  [(match_operand:SI 0 "general_operand" "")]
  ""
  "
#ifdef T800_EXPAND_ALLOCATE_STACK
  T800_EXPAND_ALLOCATE_STACK;
#else /* not T800_EXPAND_ALLOCATE_STACK */
{
    rtx size;

    /* Increase the adjustment requested by the space necessary for
       (1) firmware reserved area below Wreg (2) save slot where the
       normal value of Wreg is saved during calls and (3) outgoing args */
    if (GET_CODE (operands[0]) == CONST_INT) {
      size = GEN_INT (INTVAL (operands[0])
                      + WORKSPACE_RESERVED_BYTES
                      + UNITS_PER_WORD  /* save slot */
                      + WORD_ROUND (current_function_outgoing_args_size));
    }
    else {
      size = gen_rtx (PLUS, Pmode,
                      operands[0],
                      GEN_INT (WORKSPACE_RESERVED_BYTES
                               + UNITS_PER_WORD  /* save slot */
                               + WORD_ROUND (current_function_outgoing_args_size)));
    }
    anti_adjust_stack (size);
    DONE;
}
#endif /* not T800_EXPAND_ALLOCATE_STACK */
")

;;--------------------------------------------------------------------------
;; peephole optimization definitions
;;--------------------------------------------------------------------------

/* get rid of unnecessary stack reordering sometimes generated by
   stack-reg converter.  We could try to make the converter smarter to
   avoid generating this, but it seems simpler to leave it as it is and
   remove the redundancy with peephole optimizer.  */

/* dup; ldc NNN; stl -1; rev; ldl -1  =>  dup; ldc NNN */

(define_peephole
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (match_operand:SI 1 "ABCreg_operand" ""))
   (set (match_operand:SI 2 "ABCreg_operand" "")
        (match_operand:SI 3 "const_int_operand" ""))
   (set (mem:SI (plus:SI (reg:SI 6) (const_int -4)))
        (match_operand:SI 4 "ABCreg_operand" ""))
   (parallel[(set (match_operand:SI 5 "ABCreg_operand" "")
                  (match_operand:SI 6 "ABCreg_operand" ""))
             (set (match_dup 6)
                  (match_dup 5))])
   (set (match_operand:SI 7 "ABCreg_operand" "")
        (mem:SI (plus:SI (reg:SI 6) (const_int -4))))]
  ""
  "*return INTVAL (operands[1]) == 0x80000000
      ? \"dup\;mint\"
      : \"dup\;ldc %3\";
")

/* dup; ldl NNN; stl -1; rev; ldl -1  =>  dup; ldl NNN */

(define_peephole
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (match_operand:SI 1 "ABCreg_operand" ""))
   (set (match_operand:SI 2 "ABCreg_operand" "")
        (match_operand:SI 3 "local_operand" ""))
   (set (mem:SI (plus:SI (reg:SI 6) (const_int -4)))
        (match_operand:SI 4 "ABCreg_operand" ""))
   (parallel[(set (match_operand:SI 5 "ABCreg_operand" "")
                  (match_operand:SI 6 "ABCreg_operand" ""))
             (set (match_dup 6)
                  (match_dup 5))])
   (set (match_operand:SI 7 "ABCreg_operand" "")
        (mem:SI (plus:SI (reg:SI 6) (const_int -4))))]
  ""
  "dup\;ldl %w3")

/* ldc NNN; stl -1; rev; ldl -1  =>  rev; ldc NNN */

(define_peephole
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (match_operand:SI 1 "const_int_operand" ""))
   (set (mem:SI (plus:SI (reg:SI 6) (const_int -4)))
        (match_operand:SI 2 "ABCreg_operand" ""))
   (parallel[(set (match_operand:SI 3 "ABCreg_operand" "")
                  (match_operand:SI 4 "ABCreg_operand" ""))
             (set (match_dup 4)
                  (match_dup 3))])
   (set (match_operand:SI 5 "ABCreg_operand" "")
        (mem:SI (plus:SI (reg:SI 6) (const_int -4))))]
  ""
  "*return INTVAL (operands[1]) == 0x80000000
      ? \"rev\;mint\"
      : \"rev\;ldc %1\";
")

/* ldl NNN; stl -1; rev; ldl -1  =>  rev; ldl NNN */

(define_peephole
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (match_operand:SI 1 "local_operand" ""))
   (set (mem:SI (plus:SI (reg:SI 6) (const_int -4)))
        (match_operand:SI 2 "ABCreg_operand" ""))
   (parallel[(set (match_operand:SI 3 "ABCreg_operand" "")
                  (match_operand:SI 4 "ABCreg_operand" ""))
             (set (match_dup 4)
                  (match_dup 3))])
   (set (match_operand:SI 5 "ABCreg_operand" "")
        (mem:SI (plus:SI (reg:SI 6) (const_int -4))))]
  ""
  "rev\;ldl %w1")
