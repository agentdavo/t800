Mnemonic	Description
fprev		Reverses (swaps) the top two floating-point registers (FPAreg, FPBreg).
fpdup		Duplicates the value in FPAreg, pushing it onto the floating-point stack.
fpldnlsn	Load non-local single-precision value.
fpldnldb	Load non-local double-precision value.
fpldnlsni	Load non-local indexed single-precision value.
fpldnldbi	Load non-local indexed double-precision value.
fpldzerosn	Load single-precision zero (0.0).
fpldzerodb	Load double-precision zero (0.0).
fpstnlsn	Store non-local single-precision value.
fpstnldb	Store non-local double-precision value.
fpadd		Floating-point add.
fpsub		Floating-point subtract.
fpmul		Floating-point multiply.
fpdiv		Floating-point divide.
fprem		Floating-point remainder (IEEE REM).
fprange		Floating-point range reduce (provides remainder and quotient).
fpabs		Floating-point absolute value.
fpsqrt		Floating-point square root.
fpmulby2	Floating-point multiply by 2.0.
fpdivby2	Floating-point divide by 2.0.
fpexpinc32	Floating-point multiply by 2^32.
fpexpdec32	Floating-point divide by 2^32.
fpldnladdsn	Load non-local single and add.
fpldnladddb	Load non-local double and add.
fpldnlmulsn	Load non-local single and multiply.
fpldnlmuldb	Load non-local double and multiply.
fpeq		Floating-point equals.
fpgt		Floating-point greater than.
fpge		Floating-point greater than or equals.
fplg		Floating-point less than or greater than (not equal).
fpordered	Checks if operands are ordered (not NaNs).
fpr32tor64	Convert REAL32 to REAL64.
fpr64tor32	Convert REAL64 to REAL32.
fpint		Round floating-point number to floating-point integer.
fprtoi32	Convert REAL to INT32 (composite instruction).
fpi32tor64	Convert INT32 to REAL64.
fpi32tor32	Convert INT32 to REAL32.
fpb32tor64	Convert BIT32 (unsigned) to REAL64.
fpadddbsn	Add two double-precision numbers, producing a single-precision result.
fprn		Set rounding mode to 'round to nearest'.
fprz		Set rounding mode to 'round to zero'.
fprp		Set rounding mode to 'round to plus infinity'.
fprm		Set rounding mode to 'round to minus infinity'.
fpstall		Store all FPU state (registers and status).
fpldall		Load all FPU state.
fpnan		Test if value is a NaN (Not-a-Number).
fpnotfinite	Test if value is not finite (is a NaN or infinity).