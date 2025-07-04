;; GCC machine description for INMOS transputer family
;; Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
;;
;; Written by Yury Shevchuk <sizif@botik.ru>
;;
;; This file is part of GNU CC.
;;
;; GNU CC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU CC; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.  */


/* This file is divided into sections that correspond to the standard
   pattern names.  Sections are ordered along the manual (Using and
   Porting GNU CC, "14.7 Standard Names for Patterns Used in Generation").  */

/* Now that this file wants to be run through CPP, we use C style
   comments: CPP doesn't honor LISP-style comments and so gets upset
   by unpaired ', like the one above. */

/* Operand classes for the register allocator:
     'r' any register of integer reg-stack
     'a' Areg (reg 0)
     'b' Breg (reg 1)
     'c' Creg (reg 2)
     'f' any register of floating reg-stack
     't' FAreg (reg 3)
     'u' FBreg (reg 4)
     'v' FCreg (reg 5)  */

/* Output template additional format letters are:
     'w' convert byte offset to word offset (divide by UNITS_PER_WORD);
     the offset is checked to be a multiple of UNITS_PER_WORD;

     'q' result of division of the operand by UNITS_PER_WORD; this is
     the same as 'w' except that this will not abort if given an
     operand which is not a multiple of UNITS_PER_WORD;

     'r' remainder from division the operand by UNITS_PER_WORD;  */

/* Notes:

 * `fpu...' instructions are doubly indirect; opcode for them
should be pushed onto the integer reg-stack and then `fpentry'
command executed. This is done implicitly by assembler; however, we
should ensure that there is room in the reg-stack for push to be safe.
We achieve this by adding (clobber (match_scratch:SI "=&r")) in
patterns for such commands.

(T9000 does not need this)

 * Using (clobber (reg ...)) should be avoided, especially when
this reg is also an input for the insn: an ugliest code results. Use
(clobber (match_scratch:SI N "=&r")) instead.  */


/* POPPED_INPUTS attribute shows whether the insn pops some input
   register. Its value is the bit mask for operand numbers; e.g.
   get_attr_popped_inputs() == 6 means that registers that are operand
   1 and operand 2 are implicitly popped by this insn.  This is used
   from INSN_CLOBBERS_REGNO_P macro.  */

(define_attr "popped_inputs" "" (const_int 0))

/* POPPED_INPUTS_ON_JUMP attribute shows what inputs are popped by a
   jump insn when the jump is taken. The behaviour in the case when
   the jump is not taken is described by POPPED_INPUTS attribute.  If
   you do not use POPPED_INPUTS_ON_JUMP attribute at all, jump insns
   are supposed to pop identically in either case.  */

(define_attr "popped_inputs_on_jump" "" (const_int 0))


;;------------------------------
;; movM
;;------------------------------

/* Instructions used for SImode moves 

 src\dest             REG     local mem    nonlocal mem
REG                   dup      stl          stnl
CONST_INT             ldc       -            -
SYMBOL_REF, LABEL_REF ldpi      -            -
local memory          ldl       -            -
nonlocal memory       ldnl      -            -
Wreg                  ldlp      -            -

- Loading Wreg unsupported, being within the competence of
  prologue/epilogue only;
- Moving SImode to/from float-point registers unsupported;
  HARD_REGNO_MODE_OK rejects MODE_INT for float-point registers.  */


(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
        (match_operand:SI 1 "general_operand" ""))]
  ""
  "{
    /* transputers can move to memory only from a general reg */
    if (GET_CODE (operands[0]) == MEM)
      {
	if (! ABCreg_operand (operands[1], VOIDmode))
          operands[1] = copy_to_mode_reg (SImode, operands[1]);
      }

    /* In dataseg-by-pointer model, if src is a SYMBOL_REF/v
       (i.e. referring to data segment), it needs to be added the
       dataseg start pointer */

    if (TARGET_DATASEG_BY_POINTER
        && ((GET_CODE (operands[1]) == SYMBOL_REF
             && SYMBOL_REF_FLAG (operands[1]) == 1)
            || (GET_CODE (operands[1]) == CONST
                && t800_dataseg_symrefs_mentioned_p (XEXP (operands[1], 0)))))
        {
	  rtx temp;

          /* Don't use pseudo for temp when dest is a hard register,
             to make the generated sequence reload-safe. */

	  if (reload_in_progress
              || ((GET_CODE (operands[0]) == REG
                   && REGNO (operands[0]) <= R_CREG)
                  || (GET_CODE (operands[0]) == SUBREG
                      && GET_CODE (SUBREG_REG (operands[0])) == REG
                      && REGNO (SUBREG_REG (operands[0])) < R_CREG)))
            temp = operands[0];
          else
            temp = gen_reg_rtx (Pmode);

          emit_move_insn (temp, T800_DATASEG_START_RTX);
	  operands[1] = gen_rtx (PLUS, Pmode, temp, operands[1]);
	  /* to match ldnlp or adc */
	}

    /* If dst is a hard register prior to reload, we must be loading
       register arguments before a call or return.  The compiler
       assumes that the insns we generate for this never need
       reloading; being unaware about what is already loaded in
       registers, reload pass can use one of them for reloading, thus
       breaking the loaded value.  So take care to generate
       reload-safe insns here, sigh.  */

    else if (!reload_in_progress && !reload_completed
             && ((GET_CODE (operands[0]) == REG
                  && REGNO (operands[0]) <= R_CREG)
                 || (GET_CODE (operands[0]) == SUBREG
                     && GET_CODE (SUBREG_REG (operands[0])) == REG
                     && REGNO (SUBREG_REG (operands[0])) < R_CREG))
             && nonlocal_plus_operand (operands[1], SImode))
      {

        /* We make sure the insn will need no reloading by copying the
           nonlocal memory address register into a *hard* register --
           the one we are asked to load into.  */

	rtx temp = operands[0];

	if (GET_CODE (temp) == SUBREG)
          temp = alter_subreg (temp);

        if (GET_CODE (XEXP (operands[1], 0)) == REG)
          {
            emit_move_insn (temp, XEXP (operands[1], 0));
            operands[1] = gen_rtx (MEM, SImode, temp);
          }
        else if (GET_CODE (XEXP (operands[1], 0)) == PLUS)
          {
            emit_move_insn (temp, XEXP (XEXP (operands[1], 0), 0));
            operands[1] = gen_rtx (MEM, SImode,
                                   gen_rtx (PLUS, SImode,
                                            temp,
                                            XEXP (XEXP (operands[1], 0), 1)));
          }
        else
          abort ();
      }

    /* No special care is taken for the case when either of operands
       is a pseudo and reload is in progress (which means the pseudo
       is actually a stack slot).  We're sure the default pattern we
       generate will match either `ldl' or `stl', because in the moves
       generated by reload one of the operands is always a hard register.  */
}")


(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
        (match_operand:HI 1 "general_operand" ""))]
  ""
  "if (TARGET_HAVE_SIXTEEN) {

    /* Transputers can move to memory only from a reg */
    if (GET_CODE (operands[0]) == MEM)
      {
        if (! reg_p (operands[1]))
          {
            rtx temp = gen_reg_rtx (HImode);
            emit_move_insn (temp, operands[1]);
            operands[1] = temp;
          }

        /* Now source is a HImode REG, dest is a HImode MEM.
           This will match `ss', but ensure that destination
           MEM address is an ABCreg.  */
        if (! ABCreg_operand (XEXP (operands[0], 0), SImode))
          operands[0] = gen_rtx (MEM, HImode,
                                 copy_addr_to_reg (XEXP (operands[0], 0)));
      }
    else
      {
        /* Destination is a REG. All loading insns yield a SImode
           value, so change dest to SImode.  */

        operands[0] = gen_lowpart (SImode, operands[0]);

        if (GET_CODE (operands[1]) == MEM)
          {
            rtx addr = XEXP (operands[1], 0);

            /* If we are loading a local half-word aligned on a word
               boundary, we may use `ldl' instead of `ls'.  */

            if (local_operand_address (addr, SImode))
              operands[1] = change_address (operands[1], SImode, addr);
            else
              /* Otherwise, arrange to match `ls'.  */
              {
                if (! ABCreg_operand (addr, Pmode))
                  {
                    rtx temp = reload_in_progress? operands[0]
                                                 : gen_reg_rtx (Pmode);
                    emit_move_insn (temp, addr);
                    operands[1] = change_address (operands[1], VOIDmode, temp);
                  }
                operands[1] = gen_rtx (ZERO_EXTEND, SImode, operands[1]);
              }
          }
        else
          {
            /* Src is CONST_INT or REG; just change src to SImode, and
               word insns will match */
            operands[1] = gen_lowpart (SImode, operands[1]);
            /* to match `ldc' or `dup' */
          }

        /* Failed pseudos during reload end up with
             (set (reg:SI hard) (subreg:SI (reg:HI pseudo))),
           which matches `ldl', or with
             (set (subreg:SI (reg:HI pseudo)) (reg:SI hard)),
           which matches `stl'.  */
      }
   }
   else {
    if (GET_CODE (operands[0]) == MEM)
      {
        rtx addr = XEXP (operands[0], 0);
        rtx addr2, store_byte, addr_plus_one;

	/* Storing HImode.  On transputers without 16-bit support, we
	   have to do this by shifts and byte stores. */

        /* Examine src first.  If it is CONST_INT, we can shift it in
           compile time; otherwise, we force it to register and generate
           runtime shift later.  */

        if (GET_CODE (operands[1]) != CONST_INT
       	    && ! ABCreg_operand (operands[1], HImode))
          operands[1] = copy_to_reg (operands[1]);

	/* Get the address of destination MEM into ABCreg, if it is
           not in one.  We don't try to keep symbol_ref addresses to
           compute address of the second byte in compile time, because
           loading symbol_ref is more expensive than run-time constant
           addition.  */

        if ( ! ABCreg_operand (addr, Pmode))
          addr = copy_to_reg (addr);

        /* Make a copy of the address in yet another pseudo for use in
           second `sb'; the first one will be popped by the first `sb' */

        addr2 = copy_to_reg (addr);

        /* Get src into a reg now.  We don't attempt to make a copy of
          it like we did for the dest address, because there's no enough
          room on integer reg-stack so it would go to stack slot
          anyway. */

        if (GET_CODE (operands[1]) == CONST_INT)
          {
            /* We mask the constant to make it cheaper. */
            int val = INTVAL (operands[1]) & 0xff;
            store_byte = copy_to_mode_reg (QImode, GEN_INT (val));
          }
        else
          store_byte = gen_lowpart (QImode, operands[1]);

        /* Emit the first `sb' */

        emit_move_insn (change_address (operands[0], QImode, addr2),
                        store_byte);

        /* Prepare the value for second `sb' by shifting the original
           src right by 8 bits. */

        if (GET_CODE (operands[1]) == CONST_INT)
          {
            /* We mask the constant to make it cheaper. */
            int val = (INTVAL (operands[1]) >> 8) & 0xff;
            store_byte = copy_to_mode_reg (QImode, GEN_INT (val));
          }
        else
          {
            rtx temp = gen_reg_rtx (SImode);
            emit_insn (gen_lshrsi3 (temp,
                                    gen_lowpart (SImode, operands[1]),
                                    copy_to_mode_reg (SImode, GEN_INT (8))));
            store_byte = gen_lowpart (QImode, temp);
          }

        /* Prepare address for 2nd `sb' by incrementing address by 1 */
      
        addr_plus_one = gen_reg_rtx (SImode);
        emit_insn (gen__adc (addr_plus_one, addr, GEN_INT (1)));

        /* Emit the 2nd `sb' */

        emit_move_insn (change_address (operands[0], QImode, addr_plus_one),
                        store_byte);
        DONE;
      }
    else
      {
        /* Destination is a REG. All loading insns yield a SImode
           value, so make it into SImode subreg */

        operands[0] = gen_lowpart (SImode, operands[0]);

        if (GET_CODE (operands[1]) == MEM)
          {
            rtx addr = XEXP (operands[1], 0);

            /* If we are loading a local halfword aligned on the word
               boundary, we may use `ldl'.  */

            if (local_operand_address (addr, SImode))
              {
                operands[1] = change_address (operands[1], SImode, addr);
                /* to match `ldl' */
              }

            /* If we are loading a halfword addressed by symbol_ref,
               we can go with word load (ldnl) as well, because we
               assume symbol_refs to be word-aligned.

               ??? should accept word-aligned CONSTs, too, but it
               requires a predicate to distingwish them from unaligned
               CONSTs.  */

            else if (GET_CODE (addr) == SYMBOL_REF)
              {
                operands[1] = change_address (operands[1], SImode,
                                              copy_to_reg (addr));
                /* to match `ldnl' */
              }

            /* Otherwise, do the load in two `lb's */

            else
              {
                rtx byte1 = gen_reg_rtx (SImode);
                rtx byte2 = gen_reg_rtx (SImode);
                rtx temp  = gen_reg_rtx (SImode);
                rtx addr_plus_one;
                rtx addr2;

		/* Get the address of destination MEM into ABCreg, if
                   it is not in one.  We don't try to keep symbol_ref
                   addresses to compute address of the second byte in
                   compile time, because loading symbol_ref is more
                   expensive than run-time constant addition.  */

		if ( ! ABCreg_operand (addr, Pmode))
		  addr = copy_to_reg (addr);

		/* Make a copy of the address in yet another pseudo
                   for use in second `lb'; the first one will be
                   popped by the first `lb' */

		addr2 = copy_to_reg (addr);

		emit_insn (gen__lb (byte1,
                  change_address (operands[1], QImode, addr2)));
                addr_plus_one = gen_reg_rtx (SImode);
		emit_insn (gen__adc (addr_plus_one, addr, GEN_INT (1)));
                emit_insn (gen__lb (byte2,
                  change_address (operands[1], QImode, addr_plus_one)));
                emit_insn (gen_ashlsi3 (temp, byte2,
                  copy_to_mode_reg (SImode, GEN_INT (8))));
                emit_insn (gen_iorsi3 (gen_lowpart (SImode, operands[0]),
                                       temp, byte1));
		DONE;
              }
          }
        else
          {
            /* Src is CONST_INT or REG; just change src to SImode, and
               word insns will match */
            operands[1] = gen_lowpart (SImode, operands[1]);
            /* to match `ldc' or `dup' */
          }

        /* Failed pseudos during reload end up with
             (set (reg:SI hard) (subreg:SI (reg:HI pseudo))),
           which matches `ldl', or with
             (set (subreg:SI (reg:HI pseudo)) (reg:SI hard)),
           which matches `stl'.  */
      }
  }
")


/* Instructions used for QImode moves

                      REG              local mem    nonlocal mem
REG                   dup    (SI>SI)     sb (QI>QI)   sb (QI>QI)
CONST_INT             ldc    (VOID>SI)   -            -
SYMBOL_REF, LABEL_REF ldpi   (SI>SI)     -            -
local memory          lb/ldl (QI/SI>SI)  -            -
nonlocal memory       lb     (QI>SI)     -            -

*/

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
        (match_operand:QI 1 "general_operand" ""))]
  ""
  "{
    /* transputers can move to memory only from a reg */
    if (GET_CODE (operands[0]) == MEM)
      {
        if (! reg_p (operands[1]))
          {
            rtx temp = gen_reg_rtx (QImode);
            emit_move_insn (temp, operands[1]);
            operands[1] = temp;
          }

        /* Now source is a QImode REG, dest is a QImode MEM.
           This will match `sb', but ensure that destination
           MEM address is an ABCreg.  */
        if (! ABCreg_operand (XEXP (operands[0], 0), SImode))
          operands[0] = gen_rtx (MEM, QImode,
                                 copy_addr_to_reg (XEXP (operands[0], 0)));
      }
    else
      {
        /* Destination is a REG. All loading insns yield a SImode
           value, so change dest to SImode.  */

        operands[0] = gen_lowpart (SImode, operands[0]);

        if (GET_CODE (operands[1]) == MEM)
          {
            rtx addr = XEXP (operands[1], 0);

            /* If we are loading a local byte aligned on a word
               boundary, we may use `ldl' instead of `lb'.  */
            if (local_operand_address (addr, Pmode))
              operands[1] = change_address (operands[1], SImode, addr);
            else
              /* Otherwise, arrange to match `lb'.  */
              {
                if (! ABCreg_operand (addr, Pmode))
                  {
                    rtx temp = reload_in_progress? operands[0]
                                                 : gen_reg_rtx (Pmode);
                    emit_move_insn (temp, addr);
                    operands[1] = change_address (operands[1], VOIDmode, temp);
                  }
                operands[1] = gen_rtx (ZERO_EXTEND, SImode, operands[1]);
              }
          }
        else
          {
            /* Src is CONST_INT or REG; just change src to SImode, and
               word insns will match */
            operands[1] = gen_lowpart (SImode, operands[1]);
            /* to match `ldc' or `dup' */
          }

        /* Failed pseudos during reload end up with
             (set (reg:SI hard) (subreg:SI (reg:QI pseudo))),
           which matches `ldl', or with
             (set (subreg:SI (reg:QI pseudo)) (reg:SI hard)),
           which matches `stl'.  */
      }
}")


/* Instructions for FP moves

 src\dest             REG              nonlocal mem
REG                   fpdup             fpstnl{sn,db}
CONST_DOUBLE (== 0)   fpldzero{sn,db}   -
nonlocal memory       fpldnl{sn,db}[i]  -     */

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
        (match_operand:SF 1 "general_operand" ""))]
  ""
  "{
    rtx t800_temp_slot (enum machine_mode);
    rtx t800_force_nonlocal (rtx);

    switch (t800_fp_reg_p (operands[0]) | t800_fp_reg_p (operands[1]) << 1)
      {
      case 0:

        /* Neither operand is an fp register. Do the move with SImode
           insn(s) */

        emit_move_insn (operand_subword (operands[0], 0, 1, SFmode),
                        operand_subword (operands[1], 0, 1, SFmode));
        DONE;

      case 1:

        /* dest is an fp register, src is not.  See if src is an
           integer register: there is no insn to do such move
           directly, so use a temporary stack slot.  */

        if (REG_P (operands[1]))
          {
            rtx temp = t800_temp_slot (SFmode);

            emit_move_insn (temp, operands[1]);
            operands[1] = temp;
          }
	
        /* if src is a MEM, make sure it is a valid nonlocal MEM
           (i.e. address in a general reg) */

	if (GET_CODE (operands[1]) == MEM)
          operands[1] = t800_force_nonlocal (operands[1]);

        break;

      case 2:

        /* src is an fp register, dest is not.  Likewise...  */
        
        if (REG_P (operands[0]))
          {
	    emit_insn (gen_sf_to_ABCreg (operands[0], operands[1],
                                         t800_temp_slot (SFmode)));
            DONE;
          }
	else if (GET_CODE (operands[0]) == MEM)
          operands[0] = t800_force_nonlocal (operands[0]);
        break;

      case 3:

        /* both src and dst are fp registers.  Default pattern will do... */

        break;
      }

    /* Now it should be OK with the default pattern. */
}")

(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
        (match_operand:DF 1 "general_operand" ""))]
  ""
  "{
    rtx t800_temp_slot (enum machine_mode);
    rtx t800_force_nonlocal (rtx);

    switch (t800_fp_reg_p (operands[0]) | t800_fp_reg_p (operands[1]) << 1)
      {
      case 0:

        /* Neither operand is an fp register. Do the move with SImode
           insn(s) */

        emit_move_insn (operand_subword (operands[0], 0, 1, DFmode),
                        operand_subword (operands[1], 0, 1, DFmode));
        emit_move_insn (operand_subword (operands[0], 1, 1, DFmode),
                        operand_subword (operands[1], 1, 1, DFmode));
        DONE;

      case 1:

        /* dest is an fp register, src is not.  See if src is an
           integer register: there is no insn to do such move
           directly, so use a temporary stack slot.  */

        if (REG_P (operands[1]))
          {
            rtx temp = t800_temp_slot (DFmode);

            emit_move_insn (temp, operands[1]);
            operands[1] = temp;
          }
	
        /* if src is a MEM, make sure it is a valid nonlocal MEM
           (i.e. address in a general reg) */

	if (GET_CODE (operands[1]) == MEM)
          {
            operands[1] = t800_force_nonlocal (operands[1]);
          }
        break;

      case 2:

        /* src is an fp register, dest is not. */
        
        if (REG_P (operands[0]))
          {
	    emit_insn (gen_df_to_ABCreg (operands[0], operands[1],
                                         t800_temp_slot (DFmode)));
            DONE;
          }
	else if (GET_CODE (operands[0]) == MEM)
          {
            operands[0] = t800_force_nonlocal (operands[0]);
          }
        break;
      }

    /* Now it should be OK with the default pattern. */
}")




/* ldc */

/* {symbol,label}_ref's are not loaded by this, since it would yield
   utterly position-dependent code with a lot of load-time patches.

   ??? rewrite to make more use from mint */

(define_insn "_ldc"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (match_operand 1 "const_int_operand" ""))]
  ""
  "*return INTVAL (operands[1]) == 0x80000000
      ? \"mint\"
      : \"ldc %1\";")

(define_insn ""
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (match_operand 1 "const_offset_operand" ""))]
  ""
  "ldc %1")

/* The following two patterns are never generated by anything in
   mov_optab, but they can occasionally pop up as a result of fixing
   rtl after putting a variable into stack (fixup_var_refs_1()).  It
   seems better to kludge aroung the problem here than to hack
   function.c with risk to disturb other machines.

   (The problem exposes on enquire.c when compiling with -O). */

(define_insn ""
  [(set (match_operand:HI 0 "ABCreg_operand" "=a")
        (match_operand 1 "const_int_operand" ""))]
  ""
  "ldc %1")

(define_insn ""
  [(set (match_operand:QI 0 "ABCreg_operand" "=a")
        (match_operand 1 "const_int_operand" ""))]
  ""
  "ldc %1")


/* ldpi */

(define_insn "_ldpi"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (match_operand:SI 1 "ldpi_operand" ""))]
  ""
  "*{
#ifdef T800_LDC_SYMBOL_PC_RELATIVE
  return \"ldc %1-2\;ldpi\";
#else
  return \"ldc %1-LF%=\;ldpi\\nLF%=:\";
#endif
}")

(define_insn "_ldpi2"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (plus:SI (match_operand:SI 1 "ABCreg_operand" "0")
                 (match_operand:SI 2 "ldpi_operand" "")))]
  "T800_AS_ADC_LABELDIFF_OK"
  "adc %2-LF%=\;ldpi\\nLF%=:")


/* `ldl' and `stl' should come ahead of `dup'; otherwise a move
   between a pseudo and hard reg when reload_in_progress could match
   `dup' because ABCreg_operand accepts pseudos even then.  */

/* ldl */

(define_insn "_ldl"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (match_operand:SI 1 "local_operand" ""))]
  ""
  "ldl %w1")


/* stl */

(define_insn "_stl"
  [(set (match_operand:SI 0 "local_operand" "")
        (match_operand:SI 1 "ABCreg_operand" "a"))]
  ""
  "stl %w0"
  [(set (attr "popped_inputs") (const_int 2))])


/* dup */

/* This matches any inter-general-reg move.

  ??? `&' constraint for operand 0 is currently required by reg-stack2.c
  to present for any output if the insn has non-popped inputs. (*** &)

  Now, _dup is allowed to have no earlyclobber by INSN_OK_FOR_RULE_3.
  It is actually safe, since the case when the input and output are the same
  register, having a potential of a reg-stack overflow for other insns,
  is just a no-op move here that is removed at all in reg-stack2.c.  */

(define_insn "_dup"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (match_operand:SI 1 "ABCreg_operand" "a"))]
  ""
  "dup")

(define_insn "_rev"
  [(set (match_operand:SI 0 "ABCreg_operand" "+a")
        (match_operand:SI 1 "ABCreg_operand" "+b"))
   (set (match_dup 1)
        (match_dup 0))]
  ""
  "rev")

/* This insn is only generated by the reg-stack converter. */

(define_insn "_pop"
  [(unspec [(match_operand:SI 0 "ABCreg_operand" "a")] 0)]
  "TARGET_HAVE_POP"
  "pop"
  [(set (attr "popped_inputs") (const_int 1))])

/* Unlike the hardware ldlp, this pattern takes an arbitrary offset
   and handles it with an additional adc, if needed.  */

(define_insn "_ldlp"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a,a")
	(plus:SI (match_operand:SI 2 "Wreg_operand" "")
                 (match_operand:SI 1 "const_int_operand" "I,?n")))]
  ""
  "@
   ldlp %q1
   ldlp %q1\;adc %r1")

(define_insn ""
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (match_operand:SI 1 "Wreg_operand" ""))]
  ""
  "ldlp 0")

/* `ajw' should come ahead of `adc'. */

(define_insn "_ajw"
  [(set (match_operand:SI 0 "Wreg_operand" "")
        (match_operand:SI 1 "local_operand_address" ""))]
  ""
  "ajw %w1")

(define_insn "_gajw"
  [(set (match_operand:SI 0 "Wreg_operand" "")
        (match_operand:SI 1 "ABCreg_operand" "a"))
   (set (match_dup 1)
        (match_dup 0))]
  ""
  "gajw")

(define_insn "_ldnl"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (match_operand:SI 1 "nonlocal_plus_operand" "U"))]
  ""
  "ldnl %w1"
  [(set (attr "popped_inputs") (const_int 2))])

(define_insn "_stnl"
  [(set (match_operand:SI 0 "nonlocal_plus_operand" "=U")
        (match_operand:SI 1 "ABCreg_operand" "b"))]
  ""
  "stnl %w0"
  [(set (attr "popped_inputs") (const_int 3))])

(define_insn "_ldnlp"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (plus:SI (match_operand:SI 1 "ABCreg_operand" "0")
                 (match_operand:SI 2 "word_offset_operand" "")))]
  ""
  "ldnlp %w2")

/* ldnlp 0 is a no-op -- omitted */


(define_insn "_ls"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (zero_extend:SI (match_operand:HI 1 "nonlocal_operand" "R")))]
  "TARGET_HAVE_SIXTEEN"
  "ls"
  [(set (attr "popped_inputs") (const_int 2))])                       

(define_insn "_ss"
  [(set (match_operand:HI 0 "nonlocal_operand" "=R")
        (match_operand:HI 1 "ABCreg_operand" "b"))]
  "TARGET_HAVE_SIXTEEN"
  "ss"
  [(set (attr "popped_inputs") (const_int 3))])


(define_insn "_lb"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (zero_extend:SI (match_operand:QI 1 "nonlocal_operand" "R")))]
  ""
  "lb"
  [(set (attr "popped_inputs") (const_int 2))])                       

(define_insn "_sb"
  [(set (match_operand:QI 0 "nonlocal_operand" "=R")
        (match_operand:QI 1 "ABCreg_operand" "b"))]
  ""
  "sb"
  [(set (attr "popped_inputs") (const_int 3))])


/* This strange pattern plays an important part of making it clear to
   expr.c that we CAN load a value from memory in QImode. We support
   QImode loads with (define_expand "movqi"), but the insn that
   actually does the loading (`lb') yields a SImode value, so that
   init_expr_once() fails to detect it.

   Without it we got restricted to SImode loads, which leads to extra
   pseudos and worse code.

   We prevent this pattern from matching ever except in the test
   mentioned by using `t800_init_once_completed' in the condition.  */

(define_insn ""
  [(set (reg:QI 0) (mem:QI (reg:SI 6)))]
  "! t800_init_once_completed"
  "*abort ();")



(define_insn "_fpdup"
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (match_operand:SF 1 "FABCreg_operand" "t"))]
  "TARGET_HAVE_FPU"
  "fpdup")

(define_insn ""
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (match_operand:DF 1 "FABCreg_operand" "t"))]
  "TARGET_HAVE_FPU"
  "fpdup")

(define_insn "_fprev"
  [(set (match_operand:SF 0 "FABCreg_operand" "+t")
        (match_operand:SF 1 "FABCreg_operand" "+u"))
   (set (match_dup 1)
        (match_dup 0))]
  "TARGET_HAVE_FPU"
  "fprev")

(define_insn ""
  [(set (match_operand:DF 0 "FABCreg_operand" "+t")
        (match_operand:DF 1 "FABCreg_operand" "+u"))
   (set (match_dup 1)
        (match_dup 0))]
  "TARGET_HAVE_FPU"
  "fprev")


(define_insn "_fpldzerosn"
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (match_operand:SF 1 "zero_operand" ""))]
  "TARGET_HAVE_FPU"
  "fpldzerosn")

(define_insn "_fpldzerodb"
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (match_operand:DF 1 "zero_operand" ""))]
  "TARGET_HAVE_FPU"
  "fpldzerodb")


(define_insn "_fpldnlsn"
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (match_operand:SF 1 "nonlocal_operand" "R"))]
  "TARGET_HAVE_FPU"
  "fpldnlsn"
  [(set (attr "popped_inputs") (const_int 2))])

(define_insn "_fpldnldb"
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (match_operand:DF 1 "nonlocal_operand" "R"))]
  "TARGET_HAVE_FPU"
  "fpldnldb"
  [(set (attr "popped_inputs") (const_int 2))])


(define_insn "_fpldnlsni"
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (mem:SF (plus:SI (mult:SI (match_operand:SI 1 "ABCreg_operand" "b")
                                  (const_int 4))
                         (match_operand:SI 2 "ABCreg_operand" "a"))))]
  "TARGET_HAVE_FPU && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fpldnlsni"
  [(set (attr "popped_inputs") (const_int 6))])

(define_insn "_fpldnldbi"
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (mem:DF (plus:SI (mult:SI (match_operand:SI 1 "ABCreg_operand" "b")
                                  (const_int 8))
                         (match_operand:SI 2 "ABCreg_operand" "a"))))]
  "TARGET_HAVE_FPU && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fpldnldbi"
  [(set (attr "popped_inputs") (const_int 6))])


(define_insn "_fpstnlsn"
  [(set (match_operand:SF 0 "nonlocal_operand" "=R")
        (match_operand:SF 1 "FABCreg_operand" "t"))]
  "TARGET_HAVE_FPU"
  "fpstnlsn"
  [(set (attr "popped_inputs") (const_int 3))])

(define_insn "_fpstnldb"
  [(set (match_operand:DF 0 "nonlocal_operand" "=R")
        (match_operand:DF 1 "FABCreg_operand" "t"))]
  "TARGET_HAVE_FPU"
  "fpstnldb"
  [(set (attr "popped_inputs") (const_int 3))])


; Move from floating to integer register.  We have this redundant
; pattern for such moves, as it lets us to avoid reloading for such
; moves -- important because reloading is harmful in register function
; arguments preloading sequence.
;
(define_insn "sf_to_ABCreg"
  [(set (match_operand:SF 0 "ABCreg_operand" "=a,a,a")
        (match_operand:SF 1 "FABCreg_operand" "t,m,a"))
   (use (match_operand:SF 2 "local_operand" ""))]
  "TARGET_HAVE_FPU"
  "@
   ldlp %w2\;fpstnlsn\;ldl %w2
   ldl %w1
   OOPS -- this insn should have been deleted in reg-stack pass"
  [(set (attr "popped_inputs") (const_int 2))])

(define_insn "df_to_ABCreg"
  [(set (match_operand:DF 0 "ABCreg_operand" "=a,a,a")
        (match_operand:DF 1 "FABCreg_operand" "t,m,a"))
   (use (match_operand:DF 2 "local_operand" ""))]
  "TARGET_HAVE_FPU"
  "@
   ldlp %w2\;fpstnldb\;ldl %w2+1\;ldl %w2
   ldl %w1+1\;ldl %w1
   OOPS -- this insn should have been deleted in reg-stack pass"
  [(set (attr "popped_inputs") (const_int 2))])


;;------------------------------
;; reload_inM
;; reload_outM
;;------------------------------

/* We have to use secondary reloads when we are asked to reload a
   failed pseudo to/from an FP stack register, or from an integer
   reg-stack register in QImode.  These operations (fpldnlsn,
   fpldnldb, fpstnlsn, fpldnldb, sb) require a general register for
   memory address to reload to/from.  Input reloads in QImode are
   implemented with SImode load instruction (ldl) and therefore do not
   need an additional register.  Moreover, QImode pseudos get
   word-wide stack slot allocated, so we can always do QImode reloads
   with SImode insns which don't need scratch registers. */

;/* Sometimes, very rarely, we may need to do output reload from a
;   general register and into nonlocal memory.  The problem is that
;   default handling of this case in gen_reload() doesn't account for
;   the fact that stnl pops its address argument off the regstack, and
;   so a later insn that needs this address causes abort in reg-stack
;   converter.  So we define reload_outM to be able to copy the address
;   before stnl.
;
;   ??? Handling this for HImode is processor dependent and so pushed
;   aside for now.  */
;
;(define_expand "reload_outqi"
;  [(set (match_operand:QI 0 "nonlocal_operand" "=m")
;        (match_operand:QI 1 "ABCreg_operand" "r"))
;   (clobber (match_operand:SI 2 "" "=&r"))]
;  ""
;  "{
;    /* dup memory address to the scratch reg */
;    emit_insn (gen_rtx (SET, VOIDmode, operands[2], XEXP (operands[0],0)));
;    emit_insn (gen__sb (gen_rtx (MEM, QImode, operands[2]), operands[1]));
;    DONE;
;}")
;
;(define_expand "reload_outsi"
;  [(set (match_operand:SI 0 "nonlocal_operand" "=m")
;        (match_operand:SI 1 "ABCreg_operand" "r"))
;   (clobber (match_operand:SI 2 "" "=&r"))]
;  ""
;  "{
;    /* dup memory address to the scratch reg */
;    emit_insn (gen_rtx (SET, VOIDmode, operands[2], XEXP (operands[0],0)));
;    emit_insn (gen__stnl (gen_rtx (MEM, SImode, operands[2]), operands[1]));
;    DONE;
;}")


/* operand1 can be a stack slot
     (mem (Wreg)) or (mem (plus Wreg const_int))
   or memory constant
     (mem (symbol_ref))
   or even ABCreg

   We don't have a predicate handy to recognize all -- and probably we
   can do without a predicate, as we always want those patterns to be
   used when SECONDARY_RELOAD_CLASS says we need a secondary register.
   Same for reload_indf.  */

(define_expand "reload_insf"
  [(set (match_operand:SF 0 "FABCreg_operand" "=f")
        (match_operand:SF 1 "" "m"))
   (clobber (match_operand:SI 2 "" "=&r"))]
  "TARGET_HAVE_FPU"
  "{
    rtx base;

    /* If src is an ABCreg, we cannot move directly, so go
       through an intermediate stack slot. */

    if (t800_ABCreg_p (operands[1]))
      {
        rtx t800_temp_slot (enum machine_mode);
        rtx temp = t800_temp_slot (SFmode);
        emit_move_insn (temp, operands[1]);
        base = XEXP (temp, 0);
      }
    else
      {
        base = t800_get_reloaded_address (operands[1]);
      }
    emit_insn (gen_rtx (SET, VOIDmode, operands[2], base));
    emit_insn (gen__fpldnlsn (operands[0], gen_rtx (MEM, SFmode, operands[2])));
    DONE;
}")

(define_expand "reload_outsf"
  [(set (match_operand:SF 0 "" "=m")
        (match_operand:SF 1 "FABCreg_operand" "f"))
   (clobber (match_operand:SI 2 "" "=&r"))]
  "TARGET_HAVE_FPU"
  "{
    rtx temp = 0;
    rtx base;

    /* If dst is an ABCreg, we cannot move directly, so go through an
       intermediate stack slot.  We cannot use ABCreg_operand here, because it accepts pseudos*/

    if (t800_ABCreg_p (operands[0], SFmode))
      {
        rtx t800_temp_slot (enum machine_mode);
        temp = t800_temp_slot (SFmode);
        base = XEXP (temp, 0);
      }
    else
      {
        base = t800_get_reloaded_address (operands[0]);
      }

    emit_insn (gen_rtx (SET, VOIDmode, operands[2], base));
    emit_insn (gen__fpstnlsn (gen_rtx (MEM, SFmode, operands[2]), operands[1]));

    if (temp != 0)
      emit_move_insn (operands[0], temp);

    DONE;
}")

(define_expand "reload_indf"
  [(set (match_operand:DF 0 "FABCreg_operand" "=f")
        (match_operand:DF 1 "" "m"))
   (clobber (match_operand:SI 2 "" "=&r"))]
  "TARGET_HAVE_FPU"
  "{
    rtx base;

    /* If src is an ABCreg, we cannot move directly, so go
       through an intermediate stack slot. */

    if (t800_ABCreg_p (operands[1], DFmode))
      {
        rtx t800_temp_slot (enum machine_mode);
        rtx temp = t800_temp_slot (DFmode);
        emit_move_insn (temp, operands[1]);
        base = XEXP (temp, 0);
      }
    else
      {
        base = t800_get_reloaded_address (operands[1]);
      }
    emit_insn (gen_rtx (SET, VOIDmode, operands[2], base));
    emit_insn (gen__fpldnldb (operands[0], gen_rtx (MEM, DFmode, operands[2])));
    DONE;
}")

(define_expand "reload_outdf"
  [(set (match_operand:DF 0 "" "=m")
        (match_operand:DF 1 "FABCreg_operand" "f"))
   (clobber (match_operand:SI 2 "" "=&r"))]
  "TARGET_HAVE_FPU"
  "{
    rtx temp = 0;
    rtx base;

    /* If dst is an ABCreg, we cannot move directly, so go
       through an intermediate stack slot. */

    if (t800_ABCreg_p (operands[0], DFmode))
      {
        rtx t800_temp_slot (enum machine_mode);
        temp = t800_temp_slot (DFmode);
        base = XEXP (temp, 0);
      }
    else
      {
        base = t800_get_reloaded_address (operands[0]);
      }

    emit_insn (gen_rtx (SET, VOIDmode, operands[2], base));
    emit_insn (gen__fpstnldb (gen_rtx (MEM, DFmode, operands[2]), operands[1]));

    if (temp != 0)
      emit_move_insn (operands[0], temp);

    DONE;
}")


;;------------------------------
;; movstrictM
;;------------------------------
;; N/A


;;------------------------------
;; addM3
;;------------------------------

(define_expand "addsi3"
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (plus:SI (match_operand:SI 1 "general_operand" "")
                 (match_operand:SI 2 "general_operand" "")))]
  ""
  "
	/* In dataseg-by-pointer model, if src is a SYMBOL_REF/v
	   (i.e. referring to data segment), it needs to be added the
	   dataseg start pointer first. */

	if (TARGET_DATASEG_BY_POINTER
            && ((GET_CODE (operands[2]) == SYMBOL_REF
                 && SYMBOL_REF_FLAG (operands[2]) == 1)
                || (GET_CODE (operands[2]) == CONST
                    && t800_dataseg_symrefs_mentioned_p (XEXP (operands[2], 0)))))
          {
	    rtx temp = gen_reg_rtx (Pmode);
	    rtx temp2 = gen_reg_rtx (Pmode);

	    emit_move_insn (temp, T800_DATASEG_START_RTX);
	    emit_insn (gen_rtx (SET, VOIDmode, temp2,
	                        gen_rtx (PLUS, Pmode, temp, operands[2])));
	       /* will match ldnlp or adc */
            operands[2] = temp2;
          }

	if (GET_CODE (operands[2]) == CONST_INT)
          {
            /* This is going to be either adc or ldlp; operands[1] should
               be ABCreg or Wreg respectively.

               _ldlp pattern accepts arbitrary constants (not
               word_offset only), so we can play simple here.  */

            operands[1] = force_reg (SImode, operands[1]);
          }
        else
          {
            operands[1] = force_ABCreg (SImode, operands[1]);
	    if (HAVE__ldpi2 && ldpi_operand (operands[2], SImode))
              ;  /* ldpi2 */
            else if (word_offset_operand (operands[2], SImode))
              ;  /* ldnlp */
            else
              operands[2] = force_ABCreg (SImode, operands[2]);
          }
")
/* to match _adc/_ldlp/ldnlp/_ldpi2/_add */


(define_insn "_adc"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (plus:SI (match_operand:SI 1 "ABCreg_operand" "0")
                 (match_operand:SI 2 "adc_operand" "")))]
  ""
  "adc %2")


/* Use `add' rather than `sum' since it is shorter (opcode #05 vs. #52).
   We couldn't keep error flag from being set occasionally anyway,
   since we want to use `adc', which does checking...  */

(define_insn "_add"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (plus:SI (match_operand:SI 1 "ABCreg_operand" "%b")
                 (match_operand:SI 2 "ABCreg_operand" "a")))]
  "T800_DISTINCT_REGS (operands[1], operands[2])"
  "add"
  [(set (attr "popped_inputs") (const_int 6))])


/* wsub, wsubdb, (if available) ssub */

(define_insn "_wsub"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (plus:SI (mult:SI (match_operand:SI 2 "ABCreg_operand" "b")
                          (match_operand:SI 3 "wsub_scale_operand" ""))
                 (match_operand:SI 1 "ABCreg_operand" "a")))]
  "T800_DISTINCT_REGS (operands[1], operands[2])"
  "*return
    INTVAL (operands[3]) == UNITS_PER_WORD    ? \"wsub\"  :
    INTVAL (operands[3]) == UNITS_PER_WORD*2  ? \"wsubdb\":
    TARGET_HAVE_SIXTEEN
      && INTVAL (operands[3]) == UNITS_PER_WORD/2  ? \"ssub\":
    (char *) abort();"
  [(set (attr "popped_inputs") (const_int 6))])


/* expand_binop tries to commute input operands to make the first one a
   register, so we don't need to expect nonlocal_operand at operands[1].  */

(define_expand "addsf3"
  [(set (match_operand:SF 0 "FABCreg_operand" "")
        (plus:SF (match_operand:SF 1 "FABCreg_operand" "")
                 (match_operand:SF 2 "FABCreg_or_nonlocal_operand" "")))]
  "TARGET_HAVE_FPU"
  "")

(define_insn ""
  [(set (match_operand:SF 0 "FABCreg_operand" "=t,t")
        (plus:SF (match_operand:SF 1 "FABCreg_operand" "%u,t")
                 (match_operand:SF 2 "FABCreg_or_nonlocal_operand" "t,R")))]
  "TARGET_HAVE_FPU && T800_DISTINCT_REGS (operands[1], operands[2])"
  "@
   fpadd
   fpldnladdsn"
  [(set (attr "popped_inputs") (const_int 6))])

(define_expand "adddf3"
  [(set (match_operand:DF 0 "FABCreg_operand" "")
        (plus:DF (match_operand:DF 1 "FABCreg_operand" "")
                 (match_operand:DF 2 "FABCreg_or_nonlocal_operand" "")))]
  "TARGET_HAVE_FPU"
  "")

(define_insn ""
  [(set (match_operand:DF 0 "FABCreg_operand" "=t,t")
        (plus:DF (match_operand:DF 1 "FABCreg_operand" "%u,t")
                 (match_operand:DF 2 "FABCreg_or_nonlocal_operand" "t,R")))]
  "TARGET_HAVE_FPU && T800_DISTINCT_REGS (operands[1], operands[2])"
  "@
   fpadd
   fpldnladddb"
  [(set (attr "popped_inputs") (const_int 6))])


;;------------------------------
;; subM3
;;------------------------------

/* `diff' and `sub' are both short, and both take 1 cycle. */

(define_expand "subsi3"
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (minus:SI (match_operand:SI 1 "ABCreg_operand" "")
                  (match_operand:SI 2 "ABCreg_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (minus:SI (match_operand:SI 1 "ABCreg_operand" "b")
                  (match_operand:SI 2 "ABCreg_operand" "a")))]
  "T800_DISTINCT_REGS (operands[1], operands[2])"
  "diff"
  [(set (attr "popped_inputs") (const_int 6))])


(define_expand "subsf3"
  [(set (match_operand:SF 0 "FABCreg_operand" "")
        (minus:SF (match_operand:SF 1 "FABCreg_operand" "")
                  (match_operand:SF 2 "FABCreg_operand" "")))]
  "TARGET_HAVE_FPU"
  "")

(define_insn ""
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (minus:SF (match_operand:SF 1 "FABCreg_operand" "u")
                  (match_operand:SF 2 "FABCreg_operand" "t")))]
  "TARGET_HAVE_FPU && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fpsub"
  [(set (attr "popped_inputs") (const_int 6))])

(define_expand "subdf3"
  [(set (match_operand:DF 0 "FABCreg_operand" "")
        (minus:DF (match_operand:DF 1 "FABCreg_operand" "")
                  (match_operand:DF 2 "FABCreg_operand" "")))]
  "TARGET_HAVE_FPU"
  "")

(define_insn ""
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (minus:DF (match_operand:DF 1 "FABCreg_operand" "u")
                  (match_operand:DF 2 "FABCreg_operand" "t")))]
  "TARGET_HAVE_FPU && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fpsub"
  [(set (attr "popped_inputs") (const_int 6))])


;;------------------------------
;; mulM3
;;------------------------------

(define_expand "mulsi3"
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (mult:SI (match_operand:SI 1 "ABCreg_operand" "")
                  (match_operand:SI 2 "nonmemory_operand" "")))]
  ""
  "{
    if (GET_CODE (operands[2]) == CONST_INT
        && INTVAL (operands[2]) == UNITS_PER_WORD)
      {
        emit_insn (gen__bcnt (operands[0],operands[1]));
        DONE;
      }
    else if (! ABCreg_operand (operands[2], VOIDmode))
      operands[2] = force_reg (SImode, operands[2]);
    /* to match _prod */
}")

(define_insn "_prod"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (mult:SI (match_operand:SI 1 "ABCreg_operand" "%b")
                  (match_operand:SI 2 "ABCreg_operand" "a")))]
  "T800_DISTINCT_REGS (operands[1], operands[2])"
  "prod"
  [(set (attr "popped_inputs") (const_int 6))])

(define_insn "_bcnt"
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (mult:SI (match_operand:SI 1 "ABCreg_operand" "0")
                 (const_int 4)))]
  ""
  "bcnt")


(define_expand "mulsf3"
  [(set (match_operand:SF 0 "FABCreg_operand" "")
        (mult:SF (match_operand:SF 1 "FABCreg_operand" "")
                 (match_operand:SF 2 "whatever_operand" "")))]
  "TARGET_HAVE_FPU"
  "{
  if (fp_specval_operand (operands[2], SFmode))
    {
      emit_insn (gen__fpmul_by_specval (operands[0], operands[1], operands[2]));
      DONE;
    }
  if (! FABCreg_operand (operands[2], SFmode))
    operands[2] = copy_to_reg (operands[2]);

  /* to match _fpmul_fpldnlmulsn */
}")

(define_expand "muldf3"
  [(set (match_operand:DF 0 "FABCreg_operand" "")
        (mult:DF (match_operand:DF 1 "FABCreg_operand" "")
                 (match_operand:DF 2 "whatever_operand" "")))]
  "TARGET_HAVE_FPU"
  "{
  if (fp_specval_operand (operands[2], DFmode))
    {
      emit_insn (gen__fpmul_by_specval_df (operands[0], operands[1],
                                           operands[2]));
      DONE;
    }
  if (! FABCreg_operand (operands[2], DFmode))
    operands[2] = copy_to_reg (operands[2]);

  /* to match _fpmul_fpldnlmuldb */
}")

(define_insn "_fpmul_fpldnlmulsn"
  [(set (match_operand:SF 0 "FABCreg_operand" "=t,t")
        (mult:SF (match_operand:SF 1 "FABCreg_operand" "%u,t")
                 (match_operand:SF 2 "FABCreg_or_nonlocal_operand" "t,R")))]
  "TARGET_HAVE_FPU && T800_DISTINCT_REGS (operands[1], operands[2])"
  "@
   fpmul
   fpldnlmulsn"
  [(set (attr "popped_inputs") (const_int 6))])

(define_insn "_fpmul_fpldnlmuldb"
  [(set (match_operand:DF 0 "FABCreg_operand" "=t,t")
        (mult:DF (match_operand:DF 1 "FABCreg_operand" "%u,t")
                 (match_operand:DF 2 "FABCreg_or_nonlocal_operand" "t,R")))]
  "TARGET_HAVE_FPU && T800_DISTINCT_REGS (operands[1], operands[2])"
  "@
   fpmul
   fpldnlmuldb"
  [(set (attr "popped_inputs") (const_int 6))])

(define_insn "_fpmul_by_specval"
  [(set (match_operand:SF 0 "FABCreg_operand" "=t,t")
        (mult:SF (match_operand:SF 1 "FABCreg_operand" "t,t")
                 (match_operand:SF 2 "fp_specval_operand" "G,H")))
   (clobber (match_scratch:SI 3 "=&r,&r"))]
  "TARGET_HAVE_FPU && TARGET_HAVE_FPENTRY"
  "@
   fpumulby2
   fpuexpinc32"
  [(set (attr "popped_inputs") (const_int 2))])

(define_insn ""
  [(set (match_operand:SF 0 "FABCreg_operand" "=t,t")
        (mult:SF (match_operand:SF 1 "FABCreg_operand" "t,t")
                 (match_operand:SF 2 "fp_specval_operand" "G,H")))]
  "TARGET_HAVE_FPU && ! TARGET_HAVE_FPENTRY"
  "@
   fpmulby2
   fpexpinc32"
  [(set (attr "popped_inputs") (const_int 2))])

(define_insn "_fpmul_by_specval_df"
  [(set (match_operand:DF 0 "FABCreg_operand" "=t,t")
        (mult:DF (match_operand:DF 1 "FABCreg_operand" "t,t")
                 (match_operand:DF 2 "fp_specval_operand" "G,H")))
   (clobber (match_scratch:SI 3 "=&r,&r"))]
  "TARGET_HAVE_FPU && TARGET_HAVE_FPENTRY"
  "@
   fpumulby2
   fpuexpinc32"
  [(set (attr "popped_inputs") (const_int 2))])

(define_insn ""
  [(set (match_operand:DF 0 "FABCreg_operand" "=t,t")
        (mult:DF (match_operand:DF 1 "FABCreg_operand" "t,t")
                 (match_operand:DF 2 "fp_specval_operand" "G,H")))]
  "TARGET_HAVE_FPU && ! TARGET_HAVE_FPENTRY"
  "@
   fpmulby2
   fpexpinc32"
  [(set (attr "popped_inputs") (const_int 2))])

;;------------------------------
;; divM3
;;------------------------------

(define_expand "divsi3"
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (div:SI (match_operand:SI 1 "ABCreg_operand" "")
                (match_operand:SI 2 "ABCreg_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (div:SI (match_operand:SI 1 "ABCreg_operand" "b")
                (match_operand:SI 2 "ABCreg_operand" "a")))]
  "T800_DISTINCT_REGS (operands[1], operands[2])"
  "div"
  [(set (attr "popped_inputs") (const_int 6))])


(define_expand "divsf3"
  [(set (match_operand:SF 0 "FABCreg_operand" "")
        (div:SF (match_operand:SF 1 "FABCreg_operand" "")
                 (match_operand:SF 2 "whatever_operand" "")))]
  "TARGET_HAVE_FPU"
  "{
  if (fp_specval_operand (operands[2], SFmode))
    {
      emit_insn (gen__fpdiv_by_specval (operands[0], operands[1], operands[2]));
      DONE;
    }
  if (! FABCreg_operand (operands[2], SFmode))
    operands[2] = copy_to_reg (operands[2]);

  /* to match _fpdiv */
}")

(define_insn "_fpdiv"
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (div:SF (match_operand:SF 1 "FABCreg_operand" "u")
                 (match_operand:SF 2 "FABCreg_operand" "t")))]
  "TARGET_HAVE_FPU && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fpdiv"
  [(set (attr "popped_inputs") (const_int 6))])

(define_insn "_fpdiv_by_specval"
  [(set (match_operand:SF 0 "FABCreg_operand" "=t,t")
        (div:SF (match_operand:SF 1 "FABCreg_operand" "t,t")
                (match_operand:SF 2 "fp_specval_operand" "G,H")))
   (clobber (match_scratch:SI 3 "=&r,&r"))]
  "TARGET_HAVE_FPU && TARGET_HAVE_FPENTRY"
  "@
   fpudivby2
   fpuexpdec32"
  [(set (attr "popped_inputs") (const_int 2))])

(define_insn ""
  [(set (match_operand:SF 0 "FABCreg_operand" "=t,t")
        (div:SF (match_operand:SF 1 "FABCreg_operand" "t,t")
                (match_operand:SF 2 "fp_specval_operand" "G,H")))]
  "TARGET_HAVE_FPU && ! TARGET_HAVE_FPENTRY"
  "@
   fpdivby2
   fpexpdec32"
  [(set (attr "popped_inputs") (const_int 2))])


(define_expand "divdf3"
  [(set (match_operand:DF 0 "FABCreg_operand" "")
        (div:DF (match_operand:DF 1 "FABCreg_operand" "")
                 (match_operand:DF 2 "whatever_operand" "")))]
  "TARGET_HAVE_FPU"
  "{
  if (fp_specval_operand (operands[2], DFmode))
    {
      emit_insn (gen__fpdiv_by_specval_df (operands[0], operands[1],
                                           operands[2]));
      DONE;
    }
  if (! FABCreg_operand (operands[2], DFmode))
    operands[2] = copy_to_reg (operands[2]);

  /* to match _fpdiv_df */
}")

(define_insn "_fpdiv_df"
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (div:DF (match_operand:DF 1 "FABCreg_operand" "u")
                 (match_operand:DF 2 "FABCreg_operand" "t")))]
  "TARGET_HAVE_FPU && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fpdiv"
  [(set (attr "popped_inputs") (const_int 6))])

(define_insn "_fpdiv_by_specval_df"
  [(set (match_operand:DF 0 "FABCreg_operand" "=t,t")
        (div:DF (match_operand:DF 1 "FABCreg_operand" "t,t")
                (match_operand:DF 2 "fp_specval_operand" "G,H")))
   (clobber (match_scratch:SI 3 "=&r,&r"))]
  "TARGET_HAVE_FPU && TARGET_HAVE_FPENTRY"
  "@
   fpudivby2
   fpuexpdec32"
  [(set (attr "popped_inputs") (const_int 2))])

(define_insn ""
  [(set (match_operand:DF 0 "FABCreg_operand" "=t,t")
        (div:DF (match_operand:DF 1 "FABCreg_operand" "t,t")
                (match_operand:DF 2 "fp_specval_operand" "G,H")))]
  "TARGET_HAVE_FPU && ! TARGET_HAVE_FPENTRY"
  "@
   fpdivby2
   fpexpdec32"
  [(set (attr "popped_inputs") (const_int 2))])

;;------------------------------
;; udivM3
;;------------------------------
;; udivmodM4 is used instead


;;------------------------------
;; modM3
;;------------------------------

(define_expand "modsi3"
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (mod:SI (match_operand:SI 1 "ABCreg_operand" "")
                (match_operand:SI 2 "ABCreg_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (mod:SI (match_operand:SI 1 "ABCreg_operand" "b")
                (match_operand:SI 2 "ABCreg_operand" "a")))]
  "T800_DISTINCT_REGS (operands[1], operands[2])"
  "rem"
  [(set (attr "popped_inputs") (const_int 6))])


(define_expand "modsf3"
  [(set (match_operand:SF 0 "FABCreg_operand" "")
        (mod:SF (match_operand:SF 1 "FABCreg_operand" "")
                (match_operand:SF 2 "FABCreg_operand" "")))]
  "TARGET_HAVE_FPU"
  "")

(define_insn ""
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (mod:SF (match_operand:SF 1 "FABCreg_operand" "u")
                (match_operand:SF 2 "FABCreg_operand" "t")))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=&r"))
   (clobber (match_scratch:SI 5 "=&r"))
   (clobber (match_scratch:SF 6 "=&f"))]
  "TARGET_HAVE_FPU
   && TARGET_HAVE_FPENTRY
   && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fpremfirst
	eqc 0
	cj LF%=2
LF%=1
	fpremstep
	cj LF%=1
LF%=2"
  [(set (attr "popped_inputs") (const_int 6))])


(define_expand "moddf3"
  [(set (match_operand:DF 0 "FABCreg_operand" "")
        (mod:DF (match_operand:DF 1 "FABCreg_operand" "")
                (match_operand:DF 2 "FABCreg_operand" "")))]
  "TARGET_HAVE_FPU"
  "")

(define_insn ""
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (mod:DF (match_operand:DF 1 "FABCreg_operand" "u")
                (match_operand:DF 2 "FABCreg_operand" "t")))
   (clobber (match_scratch:SI 3 "=&r"))
   (clobber (match_scratch:SI 4 "=&r"))
   (clobber (match_scratch:SI 5 "=&r"))
   (clobber (match_scratch:SF 6 "=&f"))]
  "TARGET_HAVE_FPU
   && TARGET_HAVE_FPENTRY
   && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fpremfirst
	eqc 0
	cj LF%=2
LF%=1
	fpremstep
	cj LF%=1
LF%=2"
  [(set (attr "popped_inputs") (const_int 6))])

(define_insn ""
  [(set (match_operand:SF 0 "FABCreg_operand" "=t")
        (mod:SF (match_operand:SF 1 "FABCreg_operand" "u")
                (match_operand:SF 2 "FABCreg_operand" "t")))]
  "TARGET_HAVE_FPU
   && ! TARGET_HAVE_FPENTRY
   && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fprem"
  [(set (attr "popped_inputs") (const_int 6))])

(define_insn ""
  [(set (match_operand:DF 0 "FABCreg_operand" "=t")
        (mod:DF (match_operand:DF 1 "FABCreg_operand" "u")
                (match_operand:DF 2 "FABCreg_operand" "t")))]
  "TARGET_HAVE_FPU
   && ! TARGET_HAVE_FPENTRY
   && T800_DISTINCT_REGS (operands[1], operands[2])"
  "fprem"
  [(set (attr "popped_inputs") (const_int 6))])


;;------------------------------
;; umodM3
;;------------------------------
;; udivmodM4 is used instead


;;------------------------------
;; sminM3
;; smaxM3
;; uminM3
;; umaxM3
;;------------------------------
;; N/A

;;------------------------------
;; andM3
;;------------------------------

(define_expand "andsi3"
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (and:SI (match_operand:SI 1 "ABCreg_operand" "")
                (match_operand:SI 2 "ABCreg_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (and:SI (match_operand:SI 1 "ABCreg_operand" "%b")
                (match_operand:SI 2 "ABCreg_operand" "a")))]
  "T800_DISTINCT_REGS (operands[1], operands[2])"
  "and"
  [(set (attr "popped_inputs") (const_int 6))])


;;------------------------------
;; iorM3
;;------------------------------

(define_expand "iorsi3"
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (ior:SI (match_operand:SI 1 "ABCreg_operand" "")
                (match_operand:SI 2 "ABCreg_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (ior:SI (match_operand:SI 1 "ABCreg_operand" "%b")
                (match_operand:SI 2 "ABCreg_operand" "a")))]
  "T800_DISTINCT_REGS (operands[1], operands[2])"
  "or"
  [(set (attr "popped_inputs") (const_int 6))])


;;------------------------------
;; xorM3
;;------------------------------

(define_expand "xorsi3"
  [(set (match_operand:SI 0 "ABCreg_operand" "")
        (xor:SI (match_operand:SI 1 "ABCreg_operand" "")
                (match_operand:SI 2 "ABCreg_operand" "")))]
  ""
  "")

(define_insn ""
  [(set (match_operand:SI 0 "ABCreg_operand" "=a")
        (xor:SI (match_operand:SI 1 "ABCreg_operand" "%b")
                (match_operand:SI 2 "ABCreg_operand" "a")))]
  "T800_DISTINCT_REGS (operands[1], operands[2])"
  "xor"
  [(set (attr "popped_inputs") (const_int 6))])


;;------------------------------
;; mulhisi3
;; mulqihi3
;;------------------------------
;; N/A


;;------------------------------
;; mulsidi3
;;------------------------------

(define_expand "mulsidi3"
  [(set (match_operand:DI 0 "ABCreg_operand" "")
        (plus:DI
          (mult:DI (sign_extend:DI (match_operand:SI 1 "ABCreg_operand" ""))
                   (sign_extend:DI (match_operand:SI 2 "ABCreg_operand" "")))
          (sign_extend:DI (match_dup 3))))]
  "TARGET_HAVE_SLMUL"
  "{operands[3] = force_reg (SImode, CONST0_RTX (SImode));
   /* to match _slmul */
}")

(define_insn "_slmul"
  [(set (match_operand:DI 0 "ABCreg_operand" "=a")
        (plus:DI
          (mult:DI (sign_extend:DI (match_operand:SI 1 "ABCreg_operand" "%b"))
                   (sign_extend:DI (match_operand:SI 2 "ABCreg_operand" "a")))
          (sign_extend:DI (match_operand:SI 3 "ABCreg_operand" "c"))))]
  "TARGET_HAVE_SLMUL
   && T800_DISTINCT_REGS (operands[1], operands[2])
   && T800_DISTINCT_REGS (operands[2], operands[3])
   && T800_DISTINCT_REGS (operands[1], operands[3])"
  "slmul"
  [(set (attr "popped_inputs") (const_int 14))])

/* sulmul is not directly usable for code generation.  Yeah, tell GCC
   about it and see if it will manage to use it... */

(define_insn "_sulmul"
  [(set (match_operand:DI 0 "ABCreg_operand" "=a")
        (plus:DI
          (mult:DI (zero_extend:DI (match_operand:SI 1 "ABCreg_operand" "b"))
                   (sign_extend:DI (match_operand:SI 2 "ABCreg_operand" "a")))
          (sign_extend:DI (match_operand:SI 3 "ABCreg_operand" "c"))))]
  "TARGET_HAVE_SLMUL
   && T800_DISTINCT_REGS (operands[1], operands[2])
   && T800_DISTINCT_REGS (operands[2], operands[3])
   && T800_DISTINCT_REGS (operands[1], operands[3])"
  "sulmul"
  [(set (attr "popped_inputs") (const_int 14))])


;;------------------------------
;; umulhisi3
;; umulqihi3
;;------------------------------
;; N/A
