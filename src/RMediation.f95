MODULE constants_NSWC
! Contains the NSWC functions IPMPAR, SPMPAR, DPMPAR, EPSLN, DEPSLN,
! EXPARG & DXPARG
!-----------------------------------------------------------------------
!     WRITTEN using F90 intrinsics by
!        Alan Miller
!        CSIRO Mathematical & Information Sciences
!        CLAYTON, VICTORIA, AUSTRALIA 3169
!     Latest revision - 1 February 1997
!-----------------------------------------------------------------------

IMPLICIT NONE
INTEGER, PARAMETER     :: dp = SELECTED_REAL_KIND(15, 60)

CONTAINS

FUNCTION ipmpar (i) RESULT(fn_val)
!-----------------------------------------------------------------------

!     IPMPAR PROVIDES THE INTEGER MACHINE CONSTANTS FOR THE COMPUTER
!     THAT IS USED. IT IS ASSUMED THAT THE ARGUMENT I IS AN INTEGER
!     HAVING ONE OF THE VALUES 1-10. IPMPAR(I) HAS THE VALUE ...

!  INTEGERS.

!     ASSUME INTEGERS ARE REPRESENTED IN THE N-DIGIT, BASE-A FORM

!               SIGN ( X(N-1)*A**(N-1) + ... + X(1)*A + X(0) )

!               WHERE 0 .LE. X(I) .LT. A FOR I=0,...,N-1.

!     IPMPAR(1) = A, THE BASE (radix).

!     IPMPAR(2) = N, THE NUMBER OF BASE-A DIGITS (digits).

!     IPMPAR(3) = A**N - 1, THE LARGEST MAGNITUDE (huge).

!  FLOATING-POINT NUMBERS.

!     IT IS ASSUMED THAT THE SINGLE AND DOUBLE PRECISION FLOATING
!     POINT ARITHMETICS HAVE THE SAME BASE, SAY B, AND THAT THE
!     NONZERO NUMBERS ARE REPRESENTED IN THE FORM

!               SIGN (B**E) * (X(1)/B + ... + X(M)/B**M)

!               WHERE X(I) = 0,1,...,B-1 FOR I=1,...,M,
!               X(1) .GE. 1, AND EMIN .LE. E .LE. EMAX.

!     IPMPAR(4) = B, THE BASE.

!  SINGLE-PRECISION

!     IPMPAR(5) = M, THE NUMBER OF BASE-B DIGITS.

!     IPMPAR(6) = EMIN, THE SMALLEST EXPONENT E.

!     IPMPAR(7) = EMAX, THE LARGEST EXPONENT E.

!  DOUBLE-PRECISION

!     IPMPAR(8) = M, THE NUMBER OF BASE-B DIGITS.

!     IPMPAR(9) = EMIN, THE SMALLEST EXPONENT E.

!     IPMPAR(10) = EMAX, THE LARGEST EXPONENT E.

!-----------------------------------------------------------------------

IMPLICIT NONE
INTEGER, INTENT(IN) :: i
INTEGER             :: fn_val

SELECT CASE(i)
  CASE( 1)
    fn_val = RADIX(i)
  CASE( 2)
    fn_val = DIGITS(i)
  CASE( 3)
    fn_val = HUGE(i)
  CASE( 4)
    fn_val = RADIX(1.0)
  CASE( 5)
    fn_val = DIGITS(1.0)
  CASE( 6)
    fn_val = MINEXPONENT(1.0)
  CASE( 7)
    fn_val = MAXEXPONENT(1.0)
  CASE( 8)
    fn_val = DIGITS(1.0D0)
  CASE( 9)
    fn_val = MINEXPONENT(1.0D0)
  CASE(10)
    fn_val = MAXEXPONENT(1.0D0)
  CASE DEFAULT
    RETURN
END SELECT

RETURN
END FUNCTION ipmpar



FUNCTION spmpar (i) RESULT(fn_val)
!-----------------------------------------------------------------------

!     SPMPAR PROVIDES THE SINGLE PRECISION MACHINE CONSTANTS FOR
!     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT
!     I IS AN INTEGER HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE
!     SINGLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND
!     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN

!        SPMPAR(1) = B**(1 - M), THE MACHINE PRECISION,

!        SPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,

!        SPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE.
!-----------------------------------------------------------------------

IMPLICIT NONE
INTEGER, INTENT(IN) :: i
REAL                :: fn_val

! Local variable
REAL                :: one = 1.0

SELECT CASE (i)
  CASE (1)
    fn_val = EPSILON(one)
  CASE (2)
    fn_val = TINY(one)
  CASE (3)
    fn_val = HUGE(one)
END SELECT

RETURN
END FUNCTION spmpar



FUNCTION dpmpar (i) RESULT(fn_val)
!-----------------------------------------------------------------------

!     DPMPAR PROVIDES THE DOUBLE PRECISION MACHINE CONSTANTS FOR
!     THE COMPUTER BEING USED. IT IS ASSUMED THAT THE ARGUMENT
!     I IS AN INTEGER HAVING ONE OF THE VALUES 1, 2, OR 3. IF THE
!     DOUBLE PRECISION ARITHMETIC BEING USED HAS M BASE B DIGITS AND
!     ITS SMALLEST AND LARGEST EXPONENTS ARE EMIN AND EMAX, THEN

!        DPMPAR(1) = B**(1 - M), THE MACHINE PRECISION,

!        DPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,

!        DPMPAR(3) = B**EMAX*(1 - B**(-M)), THE LARGEST MAGNITUDE.
!-----------------------------------------------------------------------

IMPLICIT NONE
INTEGER, INTENT(IN) :: i
REAL (dp)           :: fn_val

! Local variable
REAL (dp)    :: one = 1._dp

SELECT CASE (i)
  CASE (1)
    fn_val = EPSILON(one)
  CASE (2)
    fn_val = TINY(one)
  CASE (3)
    fn_val = HUGE(one)
END SELECT

RETURN
END FUNCTION dpmpar


FUNCTION epsln () RESULT(fn_val)
!--------------------------------------------------------------------
!     THE EVALUATION OF LN(EPS) WHERE EPS IS THE SMALLEST NUMBER
!     SUCH THAT 1.0 + EPS .GT. 1.0 .  L IS A DUMMY ARGUMENT.
!--------------------------------------------------------------------
IMPLICIT NONE
REAL                :: fn_val

! Local variable
REAL                :: one = 1.0

fn_val = LOG( EPSILON(one) )
RETURN
END FUNCTION epsln


FUNCTION exparg (l) RESULT(fn_val)
!--------------------------------------------------------------------
!     IF L = 0 THEN  EXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
!     EXP(W) CAN BE COMPUTED.
!
!     IF L IS NONZERO THEN  EXPARG(L) = THE LARGEST NEGATIVE W FOR
!     WHICH THE COMPUTED VALUE OF EXP(W) IS NONZERO.
!
!     NOTE... ONLY AN APPROXIMATE VALUE FOR EXPARG(L) IS NEEDED.
!--------------------------------------------------------------------
IMPLICIT NONE
INTEGER, INTENT(IN) :: l
REAL                :: fn_val

! Local variable
REAL                :: one = 1.0

IF (l == 0) THEN
  fn_val = LOG( HUGE(one) )
ELSE
  fn_val = LOG( TINY(one) )
END IF
RETURN
END FUNCTION exparg


FUNCTION depsln () RESULT(fn_val)
!--------------------------------------------------------------------
!     THE EVALUATION OF LN(EPS) WHERE EPS IS THE SMALLEST NUMBER
!     SUCH THAT 1.D0 + EPS .GT. 1.D0 .  L IS A DUMMY ARGUMENT.
!--------------------------------------------------------------------
IMPLICIT NONE
REAL (dp)           :: fn_val

! Local variable
REAL (dp)    :: one = 1._dp

fn_val = LOG( EPSILON(one) )
RETURN
END FUNCTION depsln


FUNCTION dxparg (l) RESULT(fn_val)
!--------------------------------------------------------------------
!     IF L = 0 THEN  DXPARG(L) = THE LARGEST POSITIVE W FOR WHICH
!     DEXP(W) CAN BE COMPUTED.
!
!     IF L IS NONZERO THEN  DXPARG(L) = THE LARGEST NEGATIVE W FOR
!     WHICH THE COMPUTED VALUE OF DEXP(W) IS NONZERO.
!
!     NOTE... ONLY AN APPROXIMATE VALUE FOR DXPARG(L) IS NEEDED.
!--------------------------------------------------------------------
IMPLICIT NONE
INTEGER, INTENT(IN) :: l
REAL (dp)           :: fn_val

! Local variable
REAL (dp)    :: one = 1._dp

IF (l == 0) THEN
  fn_val = LOG( HUGE(one) )
ELSE
  fn_val = LOG( TINY(one) )
END IF
RETURN
END FUNCTION dxparg

END MODULE constants_NSWC
!=================================
! End of Module constants_NSWC
!=================================

!=================================
! Begining of module adapt_quad
!=================================
MODULE adapt_quad
! Module for adaptive quadrature.
! Adapted from TOMS algorithm 691.
! This version uses the ELF90 subset of Fortran 90.
! Conversion by Alan Miller
! amiller @ bigpond.net.au

CONTAINS

SUBROUTINE qpsrt(limit, last, maxerr, ermax, elist, iord, nrmax)
!     ..................................................................

! 1.   QPSRT
!      ORDERING ROUTINE
!         STANDARD FORTRAN SUBROUTINE
!         REAL VERSION

! 2.   PURPOSE
!         THIS ROUTINE MAINTAINS THE DESCENDING ORDERING IN THE LIST OF THE
!         LOCAL ERROR ESTIMATES RESULTING FROM THE INTERVAL SUBDIVISION
!         PROCESS.  AT EACH CALL TWO ERROR ESTIMATES ARE INSERTED USING THE
!         SEQUENTIAL SEARCH METHOD, TOP-DOWN FOR THE LARGEST ERROR ESTIMATE
!         AND BOTTOM-UP FOR THE SMALLEST ERROR ESTIMATE.

! 3.   CALLING SEQUENCE
!         CALL QPSRT(LIMIT, LAST, MAXERR, ERMAX, ELIST, IORD, NRMAX)

!      PARAMETERS (MEANING AT OUTPUT)
!         LIMIT  - INTEGER
!                  MAXIMUM NUMBER OF ERROR ESTIMATES THE LIST CAN CONTAIN

!         LAST   - INTEGER
!                  NUMBER OF ERROR ESTIMATES CURRENTLY IN THE LIST

!         MAXERR - INTEGER
!                  MAXERR POINTS TO THE NRMAX-TH LARGEST ERROR ESTIMATE
!                  CURRENTLY IN THE LIST

!         ERMAX  - REAL
!                  NRMAX-TH LARGEST ERROR ESTIMATE
!                  ERMAX = ELIST(MAXERR)

!         ELIST  - REAL
!                  VECTOR OF DIMENSION LAST CONTAINING THE ERROR ESTIMATES

!         IORD   - INTEGER
!                  VECTOR OF DIMENSION LAST, THE FIRST K ELEMENTS OF
!                  WHICH CONTAIN POINTERS TO THE ERROR ESTIMATES,
!                  SUCH THAT ELIST(IORD(1)), ... , ELIST(IORD(K))
!                  FORM A DECREASING SEQUENCE, WITH K = LAST IF
!                  LAST <= (LIMIT/2+2), AND K = LIMIT+1-LAST OTHERWISE

!         NRMAX  - INTEGER
!                  MAXERR = IORD(NRMAX)

! 4.   NO SUBROUTINES OR FUNCTIONS NEEDED

!     ..................................................................

USE constants_NSWC
IMPLICIT NONE

INTEGER, INTENT(IN)                 :: limit, last
REAL (dp), DIMENSION(:), INTENT(IN) :: elist
INTEGER, INTENT(IN OUT)             :: nrmax
INTEGER, DIMENSION(:), INTENT(OUT)  :: iord
INTEGER, INTENT(OUT)                :: maxerr
REAL (dp), INTENT(OUT)              :: ermax

REAL (dp) :: errmax, errmin
INTEGER   :: i, ibeg, ido, isucc, j, jbnd, jupbn, k

!           CHECK WHETHER THE LIST CONTAINS MORE THAN TWO ERROR ESTIMATES.

!***FIRST EXECUTABLE STATEMENT  QPSRT
IF(last > 2) GO TO 10
iord(1) = 1
iord(2) = 2
GO TO 90

!           THIS PART OF THE ROUTINE IS ONLY EXECUTED IF,
!           DUE TO A DIFFICULT INTEGRAND, SUBDIVISION INCREASED
!           THE ERROR ESTIMATE.   IN THE NORMAL CASE THE INSERT PROCEDURE
!           SHOULD START AFTER THE NRMAX-TH LARGEST ERROR ESTIMATE.

10 errmax = elist(maxerr)
IF(nrmax == 1) GO TO 30
ido = nrmax-1
DO i = 1, ido
  isucc = iord(nrmax-1)
! ***JUMP OUT OF DO-LOOP
  IF(errmax <= elist(isucc)) EXIT
  iord(nrmax) = isucc
  nrmax = nrmax-1
END DO

!           COMPUTE THE NUMBER OF ELEMENTS IN THE LIST TO BE MAINTAINED
!           IN DESCENDING ORDER.  THIS NUMBER DEPENDS ON THE NUMBER OF
!           SUBDIVISIONS STILL ALLOWED.

30 jupbn = last
IF(last > (limit/2+2)) jupbn = limit+3-last
errmin = elist(last)

!           INSERT ERRMAX BY TRAVERSING THE LIST TOP-DOWN,
!           STARTING COMPARISON FROM THE ELEMENT ELIST(IORD(NRMAX+1)).

jbnd = jupbn-1
ibeg = nrmax+1
DO i=ibeg, jbnd
  isucc = iord(i)
! ***JUMP OUT OF DO-LOOP
  IF(errmax >= elist(isucc)) GO TO 60
  iord(i-1) = isucc
END DO
iord(jbnd) = maxerr
iord(jupbn) = last
GO TO 90

!           INSERT ERRMIN BY TRAVERSING THE LIST BOTTOM-UP.

60 iord(i-1) = maxerr
k = jbnd
DO j=i, jbnd
  isucc = iord(k)
! ***JUMP OUT OF DO-LOOP
  IF(errmin < elist(isucc)) GO TO 80
  iord(k+1) = isucc
  k = k-1
END DO
iord(i) = last
GO TO 90
80 iord(k+1) = last

!           SET MAXERR AND ERMAX.

90 maxerr = iord(nrmax)
ermax = elist(maxerr)
RETURN
END SUBROUTINE qpsrt


SUBROUTINE qelg (n, epstab, result, abserr, res3la, nres, epmach, oflow)
!-----------------------------------------------------------------------

! 1.   PURPOSE
!         THE ROUTINE DETERMINES THE LIMIT OF A GIVEN SEQUENCE OF
!         APPROXIMATIONS, BY MEANS OF THE EPSILON ALGORITHM OF P. WYNN.
!         AN ESTIMATE OF THE ABSOLUTE ERROR IS ALSO GIVEN.
!         THE CONDENSED EPSILON TABLE IS COMPUTED. ONLY THOSE ELEMENTS
!         NEEDED FOR THE COMPUTATION OF THE NEXT DIAGONAL ARE PRESERVED.

! 2.   PARAMETERS
!         N      - INTEGER
!                  EPSTAB(N) CONTAINS THE NEW ELEMENT IN THE
!                  FIRST COLUMN OF THE EPSILON TABLE.

!         EPSTAB - REAL
!                  VECTOR OF DIMENSION 52 CONTAINING THE ELEMENTS OF THE
!                  TWO LOWER DIAGONALS OF THE TRIANGULAR EPSILON TABLE.
!                  THE ELEMENTS ARE NUMBERED STARTING AT THE RIGHT-HAND
!                  CORNER OF THE TRIANGLE.

!         RESULT - REAL
!                  RESULTING APPROXIMATION TO THE INTEGRAL

!         ABSERR - REAL
!                  ESTIMATE OF THE ABSOLUTE ERROR COMPUTED FROM
!                  RESULT AND THE 3 PREVIOUS RESULTS

!         RES3LA - REAL
!                  VECTOR OF DIMENSION 3 CONTAINING THE LAST 3 RESULTS

!         NRES   - INTEGER
!                  NUMBER OF CALLS TO THE ROUTINE
!                  (SHOULD BE ZERO AT FIRST CALL)

!         EPMACH - REAL
!                  THE RELATIVE PRECISION OF THE FLOATING ARITHMETIC
!                  BEING USED.

!         OFLOW  - REAL
!                  THE LARGEST POSITIVE MAGNITUDE.

! 3.   NO SUBROUTINES OR FUNCTIONS USED

!-----------------------------------------------------------------------
USE constants_NSWC
IMPLICIT NONE

INTEGER, INTENT(IN OUT)                 :: n, nres
REAL (dp), INTENT(IN)                   :: epmach, oflow
REAL (dp), INTENT(OUT)                  :: abserr, result
REAL (dp), DIMENSION(:), INTENT(IN OUT) :: epstab, res3la
!---------------------

!      LIST OF MAJOR VARIABLES
!      -----------------------

!      E0     - THE 4 ELEMENTS ON WHICH THE
!      E1       COMPUTATION OF A NEW ELEMENT IN
!      E2       THE EPSILON TABLE IS BASED
!      E3                 E0
!                   E3    E1    NEW
!                         E2
!      NEWELM - NUMBER OF ELEMENTS TO BE COMPUTED IN THE NEW DIAGONAL
!      ERROR  - ERROR = ABS(E1-E0)+ABS(E2-E1)+ABS(NEW-E2)
!      RESULT - THE ELEMENT IN THE NEW DIAGONAL WITH LEAST VALUE OF ERROR

!      LIMEXP IS THE MAXIMUM NUMBER OF ELEMENTS THE EPSILON
!      TABLE CAN CONTAIN. IF THIS NUMBER IS REACHED, THE UPPER
!      DIAGONAL OF THE EPSILON TABLE IS DELETED.

REAL (dp) :: delta1, delta2, delta3, epsinf, error, err1, err2, err3, e0, &
             e1, e1abs, e2, e3, res, ss, tol1, tol2, tol3
INTEGER   :: i, ib, ib2, ie, indx, k1, k2, k3, limexp, newelm, num

nres = nres + 1
abserr = oflow
result = epstab(n)
IF (n < 3) GO TO 100
limexp = 50
epstab(n + 2) = epstab(n)
newelm = (n - 1)/2
epstab(n) = oflow
num = n
k1 = n
DO i = 1, newelm
  k2 = k1 - 1
  k3 = k1 - 2
  res = epstab(k1 + 2)
  e0 = epstab(k3)
  e1 = epstab(k2)
  e2 = res
  e1abs = ABS(e1)
  delta2 = e2 - e1
  err2 = ABS(delta2)
  tol2 = MAX(ABS(e2),e1abs)*epmach
  delta3 = e1 - e0
  err3 = ABS(delta3)
  tol3 = MAX(e1abs,ABS(e0))*epmach
  IF (err2 > tol2 .OR. err3 > tol3) GO TO 10
  
!      IF E0, E1 AND E2 ARE EQUAL TO WITHIN MACHINE
!      ACCURACY, CONVERGENCE IS ASSUMED.
!      RESULT = E2
!      ABSERR = ABS(E1-E0) + ABS(E2-E1)
  
  result = res
  abserr = err2 + err3
! ***JUMP OUT OF DO-LOOP
  GO TO 100
  10 e3 = epstab(k1)
  epstab(k1) = e1
  delta1 = e1 - e3
  err1 = ABS(delta1)
  tol1 = MAX(e1abs,ABS(e3))*epmach
  
!      IF TWO ELEMENTS ARE VERY CLOSE TO EACH OTHER, OMIT
!      A PART OF THE TABLE BY ADJUSTING THE VALUE OF N
  
  IF (err1 <= tol1 .OR. err2 <= tol2 .OR. err3 <= tol3) GO TO 20
  ss = 1.0D0/delta1 + 1.0D0/delta2 - 1.0D0/delta3
  epsinf = ABS(ss*e1)
  
!      TEST TO DETECT IRREGULAR BEHAVIOUR IN THE TABLE, AND EVENTUALLY
!      OMIT A PART OF THE TABLE ADJUSTING THE VALUE OF N.
  
  IF (epsinf > 0.1D-03) GO TO 30
  20 n = i + i - 1
! ***JUMP OUT OF DO-LOOP
  GO TO 50
  
!      COMPUTE A NEW ELEMENT AND EVENTUALLY ADJUST THE VALUE OF RESULT.
  
  30 res = e1 + 1.0D0/ss
  epstab(k1) = res
  k1 = k1 - 2
  error = err2 + ABS(res - e2) + err3
  IF (error > abserr) CYCLE
  abserr = error
  result = res
END DO

!      SHIFT THE TABLE.

50 IF (n == limexp) n = 2*(limexp/2) - 1
ib = 1
IF ((num/2)*2 == num) ib = 2
ie = newelm + 1
DO i = 1, ie
  ib2 = ib + 2
  epstab(ib) = epstab(ib2)
  ib = ib2
END DO
IF (num == n) GO TO 80
indx = num - n + 1
DO i = 1, n
  epstab(i) = epstab(indx)
  indx = indx + 1
END DO
80 IF (nres >= 4) GO TO 90
res3la(nres) = result
abserr = oflow
GO TO 100

!      COMPUTE ERROR ESTIMATE

90 abserr = ABS(result - res3la(3)) + ABS(result - res3la(2)) +  &
            ABS(result - res3la(1))
res3la(1) = res3la(2)
res3la(2) = res3la(3)
res3la(3) = result
100 abserr = MAX(abserr, 5.0D0*epmach*ABS(result))
RETURN
END SUBROUTINE qelg


SUBROUTINE qxgs (f, a, b, epsabs, epsrel, result, abserr, ier, limit, last)

!     THE ROUTINE CALCULATES AN APPROXIMATION RESULT TO A GIVEN
!     DEFINITE INTEGRAL  I = INTEGRAL OF F OVER (A,B),
!     HOPEFULLY SATISFYING FOLLOWING CLAIM FOR ACCURACY
!     ABS(I-RESULT) <= MAX(EPSABS, EPSREL*ABS(I)).

! PARAMETERS
!  ON ENTRY
!     F      - REAL
!              FUNCTION SUBPROGRAM DEFINING THE INTEGRAND
!              FUNCTION F(X). THE ACTUAL NAME FOR F NEEDS TO BE
!              DECLARED E X T E R N A L IN THE DRIVER PROGRAM.

!     A      - REAL
!              LOWER LIMIT OF INTEGRATION

!     B      - REAL
!              UPPER LIMIT OF INTEGRATION

!     EPSABS - REAL
!              ABSOLUTE ACCURACY REQUESTED

!     EPSREL - REAL
!              RELATIVE ACCURACY REQUESTED

!  ON RETURN
!     RESULT - REAL
!              APPROXIMATION TO THE INTEGRAL

!     ABSERR - REAL
!              ESTIMATE OF THE MODULUS OF THE ABSOLUTE ERROR,
!              WHICH SHOULD EQUAL OR EXCEED ABS(I-RESULT)

!     IER    - INTEGER
!              IER = 0 NORMAL AND RELIABLE TERMINATION OF THE ROUTINE.
!                      IT IS ASSUMED THAT THE REQUESTED ACCURACY HAS
!                      BEEN ACHIEVED.
!              IER > 0 ABNORMAL TERMINATION OF THE ROUTINE
!                      THE ESTIMATES FOR INTEGRAL AND ERROR ARE
!                      LESS RELIABLE. IT IS ASSUMED THAT THE
!                      REQUESTED ACCURACY HAS NOT BEEN ACHIEVED.
!     ERROR MESSAGES
!              IER = 1 MAXIMUM NUMBER OF SUBDIVISIONS ALLOWED HAS BEEN
!                      ACHIEVED.  ONE CAN ALLOW MORE SUB-DIVISIONS BY
!                      INCREASING THE VALUE OF LIMIT (AND TAKING THE ACCORDING
!                      DIMENSION ADJUSTMENTS INTO ACCOUNT.  HOWEVER, IF THIS
!                      YIELDS NO IMPROVEMENT IT IS ADVISED TO ANALYZE THE
!                      INTEGRAND IN ORDER TO DETERMINE THE INTEGRATION
!                      DIFFICULTIES.  IF THE POSITION OF A LOCAL DIFFICULTY
!                      CAN BE DETERMINED (E.G. SINGULARITY, DISCONTINUITY
!                      WITHIN THE INTERVAL) ONE WILL PROBABLY GAIN FROM
!                      SPLITTING UP THE INTERVAL AT THIS POINT AND CALLING THE
!                      INTEGRATOR ON THE SUBRANGES.  IF POSSIBLE, AN
!                      APPROPRIATE SPECIAL-PURPOSE INTEGRATOR SHOULD BE USED,
!                      WHICH IS DESIGNED FOR HANDLING THE TYPE OF DIFFICULTY
!                      INVOLVED.
!                  = 2 THE OCCURRENCE OF ROUNDOFF ERROR IS DETECTED,
!                      WHICH PREVENTS THE REQUESTED TOLERANCE FROM BEING
!                      ACHIEVED.
!                      THE ERROR MAY BE UNDER-ESTIMATED.
!                  = 3 EXTREMELY BAD INTEGRAND BEHAVIOUR
!                      OCCURS AT SOME POINTS OF THE INTEGRATION INTERVAL.
!                  = 4 THE ALGORITHM DOES NOT CONVERGE.
!                      ROUNDOFF ERROR IS DETECTED IN THE EXTRAPOLATION TABLE.
!                      IT IS PRESUMED THAT THE REQUESTED TOLERANCE CANNOT BE
!                      ACHIEVED, AND THAT THE RETURNED RESULT IS THE BEST
!                      WHICH CAN BE OBTAINED.
!                  = 5 THE INTEGRAL IS PROBABLY DIVERGENT, OR SLOWLY CONVERGENT.
!                      IT MUST BE NOTED THAT DIVERGENCE CAN OCCUR WITH ANY
!                      OTHER VALUE OF IER.
!                  = 6 THE INPUT IS INVALID BECAUSE EPSABS OR EPSREL IS
!                      NEGATIVE, LIMIT < 1, LENW < 46*LIMIT, OR LENIW < 3*LIMIT.
!                      RESULT, ABSERR, LAST ARE SET TO ZERO.
!                      EXCEPT WHEN LIMIT OR LENW OR LENIW IS INVALID, IWORK(1),
!                      WORK(LIMIT*2+1) AND WORK(LIMIT*3+1) ARE SET TO ZERO,
!                      WORK(1) IS SET TO A, AND WORK(LIMIT+1) TO B.

!  DIMENSIONING PARAMETERS
!     LIMIT - INTEGER
!             LIMIT DETERMINES THE MAXIMUM NUMBER OF SUBINTERVALS IN THE
!             PARTITION OF THE GIVEN INTEGRATION INTERVAL (A,B), LIMIT >= 1.
!             IF LIMIT < 1, THE ROUTINE WILL END WITH IER = 6.

!     LAST  - INTEGER
!             ON RETURN, LAST EQUALS THE NUMBER OF SUBINTERVALS PRODUCED
!             IN THE SUBDIVISION PROCESS, WHICH DETERMINES THE NUMBER OF
!             SIGNIFICANT ELEMENTS ACTUALLY IN THE WORK ARRAYS.

USE constants_NSWC
IMPLICIT NONE

REAL (dp), INTENT(IN)  :: a, b, epsabs, epsrel
REAL (dp), INTENT(OUT) :: result, abserr
INTEGER, INTENT(IN)    :: limit
INTEGER, INTENT(OUT)   :: ier, last

INTERFACE
  FUNCTION f(x) RESULT(fx)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN) :: x
    REAL (dp)             :: fx
  END FUNCTION f
END INTERFACE

!         CHECK VALIDITY OF LIMIT

ier = 6
last = 0
result = 0.0D0
abserr = 0.0D0
IF (limit < 1) RETURN

!         PREPARE CALL FOR QXGSE.

CALL qxgse(f, a, b, epsabs, epsrel, limit, result, abserr, ier, last)

RETURN
END SUBROUTINE qxgs


SUBROUTINE qxgse(f, a, b, epsabs, epsrel, limit, result, abserr, ier, last)

!       THE ROUTINE CALCULATES AN APPROXIMATION RESULT TO A
!       DEFINITE INTEGRAL I = INTEGRAL OF F OVER (A,B),
!       HOPEFULLY SATISFYING FOLLOWING CLAIM FOR ACCURACY
!       ABS(I-RESULT).LE.MAX(EPSABS,EPSREL*ABS(I)).

!   PARAMETERS
!    ON ENTRY
!       F      - REAL
!                FUNCTION SUBPROGRAM DEFINING THE INTEGRAND
!                FUNCTION F(X). THE ACTUAL NAME FOR F NEEDS TO BE
!                DECLARED E X T E R N A L IN THE DRIVER PROGRAM.

!       A      - REAL
!                LOWER LIMIT OF INTEGRATION

!       B      - REAL
!                UPPER LIMIT OF INTEGRATION

!       EPSABS - REAL
!                ABSOLUTE ACCURACY REQUESTED

!       EPSREL - REAL
!                RELATIVE ACCURACY REQUESTED

!       LIMIT  - INTEGER
!                GIVES AN UPPERBOUND ON THE NUMBER OF SUBINTERVALS
!                IN THE PARTITION OF (A,B)

!    ON RETURN
!       RESULT - REAL
!                APPROXIMATION TO THE INTEGRAL

!       ABSERR - REAL
!                ESTIMATE OF THE MODULUS OF THE ABSOLUTE ERROR,
!                WHICH SHOULD EQUAL OR EXCEED ABS(I-RESULT)

!       IER    - INTEGER
!                IER = 0 NORMAL AND RELIABLE TERMINATION OF THE
!                        ROUTINE. IT IS ASSUMED THAT THE REQUESTED
!                        ACCURACY HAS BEEN ACHIEVED.
!                IER > 0 ABNORMAL TERMINATION OF THE ROUTINE
!                        THE ESTIMATES FOR INTEGRAL AND ERROR ARE
!                        LESS RELIABLE. IT IS ASSUMED THAT THE
!                        REQUESTED ACCURACY HAS NOT BEEN ACHIEVED.
!       ERROR MESSAGES
!                    = 1 MAXIMUM NUMBER OF SUBDIVISIONS ALLOWED
!                        HAS BEEN ACHIEVED. ONE CAN ALLOW MORE SUB-
!                        DIVISIONS BY INCREASING THE VALUE OF LIMIT
!                        (AND TAKING THE ACCORDING DIMENSION
!                        ADJUSTMENTS INTO ACCOUNT). HOWEVER, IF
!                        THIS YIELDS NO IMPROVEMENT IT IS ADVISED
!                        TO ANALYZE THE INTEGRAND IN ORDER TO
!                        DETERMINE THE INTEGRATION DIFFICULTIES. IF
!                        THE POSITION OF A LOCAL DIFFICULTY CAN BE
!                        DETERMINED (E.G. SINGULARITY,
!                        DISCONTINUITY WITHIN THE INTERVAL) ONE
!                        WILL PROBABLY GAIN FROM SPLITTING UP THE
!                        INTERVAL AT THIS POINT AND CALLING THE
!                        INTEGRATOR ON THE SUBRANGES. IF POSSIBLE,
!                        AN APPROPRIATE SPECIAL-PURPOSE INTEGRATOR
!                        SHOULD BE USED, WHICH IS DESIGNED FOR
!                        HANDLING THE TYPE OF DIFFICULTY INVOLVED.
!                    = 2 THE OCCURRENCE OF ROUNDOFF ERROR IS DETEC-
!                        TED, WHICH PREVENTS THE REQUESTED
!                        TOLERANCE FROM BEING ACHIEVED.
!                        THE ERROR MAY BE UNDER-ESTIMATED.
!                    = 3 EXTREMELY BAD INTEGRAND BEHAVIOUR OCCURS AT
!                        SOME POINTS OF THE INTEGRATION INTERVAL.
!                    = 4 THE ALGORITHM DOES NOT CONVERGE.
!                        ROUNDOFF ERROR IS DETECTED IN THE
!                        EXTRAPOLATION TABLE.
!                        IT IS PRESUMED THAT THE REQUESTED TOLERANCE
!                        CANNOT BE ACHIEVED, AND THAT THE RETURNED RESULT
!                        IS THE BEST WHICH CAN BE OBTAINED.
!                    = 5 THE INTEGRAL IS PROBABLY DIVERGENT, OR SLOWLY
!                        CONVERGENT.   IT MUST BE NOTED THAT DIVERGENCE
!                        CAN OCCUR WITH ANY OTHER VALUE OF IER.
!                    = 6 THE INPUT IS INVALID BECAUSE EPSABS OR
!                        EPSREL IS NEGATIVE. RESULT, ABSERR,
!                        LAST, RLIST(1), IORD(1), AND ELIST(1)
!                        ARE SET TO ZERO. ALIST(1) AND BLIST(1)
!                        ARE SET TO A AND B RESPECTIVELY.

!       ALIST  - REAL
!                VECTOR OF DIMENSION AT LEAST LIMIT, THE FIRST
!                 LAST  ELEMENTS OF WHICH ARE THE LEFT END POINTS
!                OF THE SUBINTERVALS IN THE PARTITION OF THE
!                GIVEN INTEGRATION RANGE (A,B)

!       BLIST  - REAL
!                VECTOR OF DIMENSION AT LEAST LIMIT, THE FIRST
!                 LAST  ELEMENTS OF WHICH ARE THE RIGHT END POINTS
!                OF THE SUBINTERVALS IN THE PARTITION OF THE GIVEN
!                INTEGRATION RANGE (A,B)

!       RLIST  - REAL
!                VECTOR OF DIMENSION AT LEAST LIMIT, THE FIRST `LAST'
!                ELEMENTS OF WHICH ARE THE INTEGRAL APPROXIMATIONS ON
!                THE SUBINTERVALS

!       ELIST  - REAL
!                VECTOR OF DIMENSION AT LEAST LIMIT, THE FIRST
!                 LAST  ELEMENTS OF WHICH ARE THE MODULI OF THE
!                ABSOLUTE ERROR ESTIMATES ON THE SUBINTERVALS

!       IORD   - INTEGER
!                VECTOR OF DIMENSION AT LEAST LIMIT, THE FIRST K ELEMENTS
!                OF WHICH ARE POINTERS TO THE ERROR ESTIMATES OVER THE
!                SUBINTERVALS, SUCH THAT ELIST(IORD(1)), ..., ELIST(IORD(K))
!                FORM A DECREASING SEQUENCE, WITH K = LAST IF
!                LAST <= (LIMIT/2+2), AND K = LIMIT+1-LAST OTHERWISE

!       LAST   - INTEGER
!                NUMBER OF SUBINTERVALS ACTUALLY PRODUCED IN THE
!                SUBDIVISION PROCESS

!       VALP   - REAL
!       VALN     ARRAYS OF DIMENSION AT LEAST (21,LIMIT) USED TO
!                SAVE THE FUNCTIONAL VALUES

!       LP     - INTEGER
!       LN       VECTORS OF DIMENSION AT LEAST LIMIT, USED TO
!                STORE THE ACTUAL NUMBER OF FUNCTIONAL VALUES
!                SAVED IN THE CORRESPONDING COLUMN OF VALP,VALN

!***ROUTINES CALLED  F, SPMPAR, QELG, QXLQM, QPSRT, QXRRD, QXCPY

USE constants_NSWC
IMPLICIT NONE

REAL (dp), INTENT(IN)   :: a, b, epsabs, epsrel
REAL (dp), INTENT(OUT)  :: result, abserr
INTEGER, INTENT(IN)     :: limit
INTEGER, INTENT(OUT)    :: ier, last

INTERFACE
  FUNCTION f(x) RESULT(fx)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN) :: x
    REAL (dp)             :: fx
  END FUNCTION f
END INTERFACE

REAL (dp) :: abseps, alist(limit), area, area1, area12, area2, a1, a2, b1, &
             b2, blist(limit), correc, defab1, defab2, dres, elist(limit), &
             epmach, erlarg, erlast, errbnd, errmax, error1, error2,       &
             erro12, errsum, ertest, oflow, rerr, resabs, reseps, res3la(3), &
             rlist(limit), rlist2(52), small, t, uflow, valp(21,limit),    &
             valn(21,limit), vp1(21), vp2(21), vn1(21), vn2(21), defabs
INTEGER   :: id, ierro, iord(limit), iroff1, iroff2, iroff3, jupbnd, k,  &
             ksgn, lp(limit), ln(limit), ktmin, maxerr, nres, nrmax, numrl2, &
             lp1, lp2, ln1, ln2
LOGICAL   :: extrap, noext

!            MACHINE DEPENDENT CONSTANTS
!            ---------------------------

!          EPMACH IS THE LARGEST RELATIVE SPACING.
!          UFLOW IS THE SMALLEST POSITIVE MAGNITUDE.
!          OFLOW IS THE LARGEST POSITIVE MAGNITUDE.

epmach = dpmpar(1)
uflow = dpmpar(2)
oflow = dpmpar(3)

!            TEST ON VALIDITY OF PARAMETERS
!            ------------------------------
last = 0
result = 0.0D0
abserr = 0.0D0
alist(1) = a
blist(1) = b
rlist(1) = 0.0D0
elist(1) = 0.0D0
ier = 6
IF (epsabs < 0.0D0 .OR. epsrel < 0.0D0) GO TO 999
ier = 0
rerr = MAX(epsrel, 50.0D0*epmach)

!      FIRST APPROXIMATION TO THE INTEGRAL
!      -----------------------------------

ierro = 0
lp(1) = 1
ln(1) = 1
valp(1,1) = f((a + b)*0.5D0)
valn(1,1) = valp(1,1)
CALL qxlqm(f, a, b, result, abserr, resabs, defabs, valp(1:,1), valn(1:,1), &
           lp(1), ln(1), 2, epmach, uflow, oflow)

!      TEST ON ACCURACY.

dres = ABS(result)
errbnd = MAX(epsabs,rerr*dres)
last = 1
rlist(1) = result
elist(1) = abserr
iord(1) = 1
IF (abserr <= 100.0D0*epmach*defabs .AND. abserr > errbnd) ier = 2
IF (limit == 1) ier = 1
IF (ier /= 0 .OR. (abserr <= errbnd .AND. abserr /= resabs) .OR.  &
abserr == 0.0D0) GO TO 999

!      INITIALIZATION
!      --------------

rlist2(1) = result
errmax = abserr
maxerr = 1
area = result
errsum = abserr
abserr = oflow
nrmax = 1
nres = 0
numrl2 = 2
ktmin = 0
extrap = .false.
noext = .false.
iroff1 = 0
iroff2 = 0
iroff3 = 0
ksgn = -1
IF (dres >= (1.0D0 - 50.0D0*epmach)*defabs) ksgn = 1
t = 1.0D0 + 100.0D0*epmach

!      MAIN DO-LOOP
!      ------------

DO last = 2, limit
  
!      BISECT THE SUBINTERVAL WITH THE NRMAX-TH LARGEST ERROR ESTIMATE.
  
  a1 = alist(maxerr)
  b1 = 0.5D0*(alist(maxerr) + blist(maxerr))
  a2 = b1
  b2 = blist(maxerr)
  erlast = errmax
  CALL qxrrd(f, valn(1:,maxerr), ln(maxerr), b1, a1, vn1, vp1, ln1, lp1)
  CALL qxlqm(f, a1, b1, area1, error1, resabs, defab1, vp1, vn1, lp1, ln1,  &
             2, epmach, uflow, oflow)
  CALL qxrrd(f, valp(1:,maxerr), lp(maxerr), a2, b2, vp2, vn2, lp2, ln2)
  CALL qxlqm(f, a2, b2, area2, error2, resabs, defab2, vp2, vn2, lp2, ln2,  &
             2, epmach, uflow, oflow)
  
!      IMPROVE PREVIOUS APPROXIMATIONS TO INTEGRAL
!      AND ERROR AND TEST FOR ACCURACY.
  
  area12 = area1 + area2
  erro12 = error1 + error2
  errsum = errsum + erro12 - errmax
  area = area + area12 - rlist(maxerr)
  IF (defab1 == error1 .OR. defab2 == error2) GO TO 15
  IF (ABS(rlist(maxerr) - area12) > 0.1D-04*ABS(area12)  &
        .OR. erro12 < 0.99D0*errmax) GO TO 10
  IF (extrap) iroff2 = iroff2 + 1
  IF (.NOT.extrap) iroff1 = iroff1 + 1
  10 IF (last > 10 .AND. erro12 > errmax) iroff3 = iroff3 + 1
  15 rlist(maxerr) = area1
  rlist(last) = area2
  errbnd = MAX(epsabs,rerr*ABS(area))
  
!      TEST FOR ROUNDOFF ERROR AND EVENTUALLY SET ERROR FLAG.
  
  IF (iroff1 + iroff2 >= 10 .OR. iroff3 >= 20) ier = 2
  IF (iroff2 >= 5) ierro = 3
  
!      SET ERROR FLAG IN THE CASE THAT THE NUMBER OF SUBINTERVALS EQUALS LIMIT.
  
  IF (last == limit) ier = 1
  
!      SET ERROR FLAG IN THE CASE OF BAD INTEGRAND BEHAVIOUR
!      AT A POINT OF THE INTEGRATION RANGE.
  
  IF (MAX(ABS(a1),ABS(b2)) <= t*(ABS(a2) + 1.d+03*uflow)) ier = 4
  
!      APPEND THE NEWLY-CREATED INTERVALS TO THE LIST.
  
  IF (error2 > error1) GO TO 20
  alist(last) = a2
  blist(maxerr) = b1
  blist(last) = b2
  elist(maxerr) = error1
  elist(last) = error2
  CALL qxcpy(valp(1:,maxerr), vp1, lp1)
  lp(maxerr) = lp1
  CALL qxcpy(valn(1:,maxerr), vn1, ln1)
  ln(maxerr) = ln1
  CALL qxcpy(valp(1:,last), vp2, lp2)
  lp(last) = lp2
  CALL qxcpy(valn(1:,last), vn2, ln2)
  ln(last) = ln2
  GO TO 30
  
  20 alist(maxerr) = a2
  alist(last) = a1
  blist(last) = b1
  rlist(maxerr) = area2
  rlist(last) = area1
  elist(maxerr) = error2
  elist(last) = error1
  CALL qxcpy(valp(1:,maxerr), vp2, lp2)
  lp(maxerr) = lp2
  CALL qxcpy(valn(1:,maxerr), vn2, ln2)
  ln(maxerr) = ln2
  CALL qxcpy(valp(1:,last), vp1, lp1)
  lp(last) = lp1
  CALL qxcpy(valn(1:,last), vn1, ln1)
  ln(last) = ln1
  
!      CALL SUBROUTINE QPSRT TO MAINTAIN THE DESCENDING ORDERING
!      IN THE LIST OF ERROR ESTIMATES AND SELECT THE SUBINTERVAL
!      WITH NRMAX-TH LARGEST ERROR ESTIMATE (TO BE BISECTED NEXT).
  
  30 CALL qpsrt(limit, last, maxerr, errmax, elist, iord, nrmax)
! ***JUMP OUT OF DO-LOOP
  IF(errsum <= errbnd) GO TO 115
! ***JUMP OUT OF DO-LOOP
  IF (ier /= 0) GO TO 100
  IF (last == 2) GO TO 80
  IF (noext) CYCLE
  erlarg = erlarg - erlast
  IF (ABS(b1 - a1) > small) erlarg = erlarg + erro12
  IF (extrap) GO TO 40
  
!      TEST WHETHER THE INTERVAL TO BE BISECTED NEXT IS THE SMALLEST INTERVAL.
  
  IF (ABS(blist(maxerr) - alist(maxerr)) > small) CYCLE
  extrap = .true.
  nrmax = 2
  
!      THE BOUND 0.3*ERTEST HAS BEEN INTRODUCED TO PERFORM A
!      MORE CAUTIOUS EXTRAPOLATION THAN IN THE ORIGINAL DQAGSE ROUTINE
  
  40 IF (ierro == 3 .OR. erlarg <= 0.3D0*ertest) GO TO 60
  
!      THE SMALLEST INTERVAL HAS THE LARGEST ERROR.
!      BEFORE BISECTING DECREASE THE SUM OF THE ERRORS OVER THE
!      LARGER INTERVALS (ERLARG) AND PERFORM EXTRAPOLATION.
  
  id = nrmax
  jupbnd = last
  IF (last > (2 + limit/2)) jupbnd = limit + 3 - last
  DO k = id, jupbnd
    maxerr = iord(nrmax)
    errmax = elist(maxerr)
! ***JUMP OUT OF DO-LOOP
    IF(ABS(blist(maxerr) - alist(maxerr)) > small) CYCLE
    nrmax = nrmax + 1
  END DO
  
!      PERFORM EXTRAPOLATION.
  
  60 numrl2 = numrl2 + 1
  rlist2(numrl2) = area
  CALL qelg (numrl2, rlist2, reseps, abseps, res3la, nres, epmach, oflow)
  ktmin = ktmin + 1
  IF (ktmin > 5 .AND. abserr < 0.1D-02*errsum) ier = 5
  IF (abseps >= abserr) GO TO 70
  ktmin = 0
  abserr = abseps
  result = reseps
  correc = erlarg
  ertest = MAX(epsabs,rerr*ABS(reseps))
! ***JUMP OUT OF DO-LOOP
  IF (abserr <= ertest) GO TO 100
  
!      PREPARE BISECTION OF THE SMALLEST INTERVAL.
  
  70 IF (numrl2 == 1) noext = .true.
  IF (ier == 5) GO TO 100
  maxerr = iord(1)
  errmax = elist(maxerr)
  nrmax = 1
  extrap = .false.
  small = small*0.5D0
  erlarg = errsum
  CYCLE
  80 small = ABS(b - a)*0.375D0
  erlarg = errsum
  ertest = errbnd
  rlist2(2) = area
END DO

!      SET FINAL RESULT AND ERROR ESTIMATE.
!      ------------------------------------

100 IF (abserr == oflow) GO TO 115
IF (ier + ierro == 0) GO TO 110
IF (ierro == 3) abserr = abserr + correc
IF (ier == 0) ier = 3
IF (result /= 0.0D0 .AND. area /= 0.0D0) GO TO 105
IF (abserr > errsum) GO TO 115
IF (area == 0.0D0) GO TO 130
GO TO 110
105 IF (abserr/ABS(result) > errsum/ABS(area)) GO TO 115

!      TEST ON DIVERGENCE.

110 IF(ksgn == (-1) .AND. MAX(ABS(result),ABS(area)) <=  &
defabs*0.1D-01) GO TO 130
IF(0.1D-01 > (result/area) .OR. (result/area) > 0.1D+03  &
   .OR. errsum > ABS(area)) ier = 6
GO TO 130

!      COMPUTE GLOBAL INTEGRAL SUM.

115 result = SUM( rlist(1:last) )
abserr = errsum
130 IF (ier > 2) ier = ier - 1
999 RETURN
END SUBROUTINE qxgse


SUBROUTINE qxcpy (a, b, l)

!  TO COPY THE REAL VECTOR B OF LENGTH L   I N T O
!          THE REAL VECTOR A OF LENGTH L

USE constants_NSWC
IMPLICIT NONE

INTEGER, INTENT(IN)   :: l
REAL (dp), DIMENSION(:), INTENT(IN)  :: b
REAL (dp), DIMENSION(:), INTENT(OUT) :: a

a(1:l) = b(1:l)
RETURN
END SUBROUTINE qxcpy


SUBROUTINE qxlqm (f, a, b, result, abserr, resabs, resasc, vr, vs, lr, ls,  &
                  key, epmach, uflow, oflow)

!    TO COMPUTE I = INTEGRAL OF F OVER (A, B), WITH ERROR ESTIMATE
!               J = INTEGRAL OF ABS(F) OVER (A,B)

!   PARAMETERS
!    ON ENTRY
!      F      - REAL
!               FUNCTION SUBPROGRAM DEFINING THE INTEGRAND
!               FUNCTION F(X). THE ACTUAL NAME FOR F NEEDS TO BE
!               DECLARED E X T E R N A L IN THE DRIVER PROGRAM.

!      A      - REAL
!               LOWER LIMIT OF INTEGRATION

!      B      - REAL
!               UPPER LIMIT OF INTEGRATION

!      VR     - REAL
!               VECTOR OF LENGTH LR CONTAINING THE
!               SAVED  FUNCTIONAL VALUES OF POSITIVE ABSCISSAS

!      VS     - REAL
!               VECTOR OF LENGTH LS CONTAINING THE
!               SAVED  FUNCTIONAL VALUES OF NEGATIVE ABSCISSAS

!      LR     - INTEGER
!      LS       NUMBER OF ELEMENTS IN VR,VS RESPECTIVELY

!    KEY    - INTEGER
!             KEY FOR CHOICE OF LOCAL INTEGRATION RULE
!             RMS FORMULAS ARE USED WITH
!              13 - 19               POINTS IF KEY < 1,
!              13 - 19 - (27)        POINTS IF KEY = 1,
!              13 - 19 - (27) - (41) POINTS IF KEY = 2,
!                   19 -  27  - (41) POINTS IF KEY = 3,
!                         27  -  41  POINTS IF KEY > 3.

!             (RULES) USED IF THE FUNCTION APPEARS REGULAR ENOUGH

!      EPMACH - REAL
!               THE RELATIVE PRECISION OF THE FLOATING
!               ARITHMETIC BEING USED.

!      UFLOW  - REAL
!               THE SMALLEST POSITIVE MAGNITUDE.

!      OFLOW  - REAL
!               THE LARGEST POSITIVE MAGNITUDE.

!    ON RETURN
!      RESULT - REAL
!               APPROXIMATION TO THE INTEGRAL I

!      ABSERR - REAL
!               ESTIMATE OF THE MODULUS OF THE ABSOLUTE ERROR,
!               WHICH SHOULD NOT EXCEED ABS(I-RESULT)

!      RESABS - REAL
!               APPROXIMATION TO THE INTEGRAL J

!      RESASC - REAL
!               APPROXIMATION TO THE INTEGRAL OF ABS(F-I/(B-A)) OVER (A,B)

!      VR     - REAL
!               VECTOR OF LENGTH LR CONTAINING THE
!               SAVED  FUNCTIONAL VALUES OF POSITIVE ABSCISSAS

!      VS     - REAL
!               VECTOR OF LENGTH LS CONTAINING THE
!               SAVED  FUNCTIONAL VALUES OF NEGATIVE ABSCISSAS

!      LR     - INTEGER
!      LS       NUMBER OF ELEMENTS IN VR,VS RESPECTIVELY

!***ROUTINES CALLED  QXRUL

USE constants_NSWC
IMPLICIT NONE

REAL (dp), INTENT(IN)                   :: a, b, epmach, oflow, uflow
INTEGER, INTENT(IN)                     :: key
REAL (dp), INTENT(OUT)                  :: result, abserr, resabs, resasc
REAL (dp), DIMENSION(:), INTENT(IN OUT) :: vr, vs
INTEGER, INTENT(IN OUT)                 :: lr, ls

REAL (dp) :: t, resg, resk, errold
INTEGER   :: k, k0, k1, k2, key1

INTERFACE
  FUNCTION f(x) RESULT(fx)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN) :: x
    REAL (dp)             :: fx
  END FUNCTION f
END INTERFACE

key1 = MAX(key ,  0)
key1 = MIN(key1,  4)
k0   = MAX(key1-2,0)
k1   = k0 + 1
k2   = MIN(key1+1,3)

CALL qxrul (f, a, b, resg, resabs, resasc, k0, k1, vr, vs, lr, ls)
errold = oflow
t = 10.0D0*epmach
DO k = k1, k2
  CALL qxrul (f, a, b, resk, resabs, resasc, k, k1, vr, vs, lr, ls)
  result = resk
  abserr = ABS(resk - resg)
  IF (resasc /= 0.0D0 .AND. abserr /= 0.0D0)  &
  abserr = resasc*MIN(1.0D0,(200.0D0*abserr/resasc)**1.5D0)
  IF (resabs > uflow/t) abserr = MAX(t*resabs,abserr)
  resg = resk
  IF (abserr > errold*0.16D0) EXIT
  IF (abserr < 1000.0D0*epmach*resabs) CYCLE
  errold = abserr
END DO

RETURN
END SUBROUTINE qxlqm


SUBROUTINE qxrul (f, xl, xu, y, ya, ym, ke, k1, fv1, fv2, l1, l2)

!    TO COMPUTE I = INTEGRAL OF F OVER (A,B), WITH ERROR ESTIMATE
!    AND CONDITIONALLY COMPUTE
!               J = INTEGRAL OF ABS(F) OVER (A,B)
!               BY USING AN  RMS RULE
!   PARAMETERS
!    ON ENTRY
!      F      - REAL
!               FUNCTION SUBPROGRAM DEFINING THE INTEGRAND
!               FUNCTION F(X). THE ACTUAL NAME FOR F NEEDS TO BE
!               DECLARED E X T E R N A L IN THE DRIVER PROGRAM.

!      XL     - REAL
!               LOWER LIMIT OF INTEGRATION

!      XU     - REAL
!               UPPER LIMIT OF INTEGRATION

!      KE     - INTEGER
!             KEY FOR CHOICE OF LOCAL INTEGRATION RULE
!             AN RMS RULE IS USED WITH
!                 13      POINTS IF KE  = 2,
!                 19      POINTS IF KE  = 3,
!                 27      POINTS IF KE  = 4,
!                 42      POINTS IF KE  = 5

!      K1     INTEGER
!             VALUE OF KEY FOR WHICH THE ADDITIONAL ESTIMATES
!             YA, YM ARE TO BE COMPUTED

!      FV1    - REAL
!               VECTOR CONTAINING L1
!               SAVED  FUNCTIONAL VALUES OF POSITIVE ABSCISSAS

!      FV2    - REAL
!               VECTOR CONTAINING L2
!               SAVED  FUNCTIONAL VALUES OF NEGATIVE ABSCISSAS

!      L1     - INTEGER
!      L2       NUMBER OF ELEMENTS IN FV1,FV2  RESPECTIVELY

!    ON RETURN
!      Y      - REAL
!               APPROXIMATION TO THE INTEGRAL I
!               RESULT IS COMPUTED BY APPLYING THE REQUESTED RMS RULE

!      YA     - REAL
!               IF KEY = K1  APPROXIMATION TO THE INTEGRAL J
!               ELSE UNCHANGED

!      YM     - REAL
!               IF KEY = K1  APPROXIMATION TO THE INTEGRAL OF
!                              ABS(F-I/(XU-XL)   OVER (XL,XU)
!               ELSE UNCHANGED

!      FV1    - REAL
!               VECTOR CONTAINING L1
!               SAVED  FUNCTIONAL VALUES OF POSITIVE ABSCISSAS

!      FV2    - REAL
!               VECTOR CONTAINING L2
!               SAVED  FUNCTIONAL VALUES OF NEGATIVE ABSCISSAS

!      L1     - INTEGER
!      L2       NUMBER OF ELEMENTS IN FV1,FV2  RESPECTIVELY

!------------------------
USE constants_NSWC
IMPLICIT NONE

REAL (dp), INTENT(IN)                   :: xl, xu
INTEGER, INTENT(IN)                     :: ke, k1
REAL (dp), INTENT(OUT)                  :: y, ya, ym
REAL (dp), DIMENSION(:), INTENT(IN OUT) :: fv1, fv2
INTEGER, INTENT(IN OUT)                 :: l1, l2

REAL (dp) :: ldl, y2, aa, bb, c
INTEGER   :: istart(4) = (/ 0, 7, 17, 31 /), length(4) = (/ 7, 10, 14, 21 /), &
             j, i, is, k, ks

INTERFACE
  FUNCTION f(x) RESULT(fx)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN) :: x
    REAL (dp)             :: fx
  END FUNCTION f
END INTERFACE

!------------------------
REAL (dp) :: xx(21) = (/ 0.D0,                    .25000000000000000000D+00, &
                       .50000000000000000000D+00, .75000000000000000000D+00, &
                       .87500000000000000000D+00, .93750000000000000000D+00, &
                       .10000000000000000000D+01, .37500000000000000000D+00, &
                       .62500000000000000000D+00, .96875000000000000000D+00, &
                       .12500000000000000000D+00, .68750000000000000000D+00, &
                       .81250000000000000000D+00, .98437500000000000000D+00, &
                       .18750000000000000000D+00, .31250000000000000000D+00, &
                       .43750000000000000000D+00, .56250000000000000000D+00, &
                       .84375000000000000000D+00, .90625000000000000000D+00, &
                       .99218750000000000000D+00 /)
REAL (dp) :: ww(52) = (/     &
1.303262173284849021810473057638590518409112513421D-1, 2.390632866847646220320329836544615917290026806242D-1,  &
2.630626354774670227333506083741355715758124943143D-1, 2.186819313830574175167853094864355208948886875898D-1,  &
2.757897646642836865859601197607471574336674206700D-2, 1.055750100538458443365034879086669791305550493830D-1,  &
1.571194260595182254168429283636656908546309467968D-2, 1.298751627936015783241173611320651866834051160074D-1,  &
2.249996826462523640447834514709508786970828213187D-1, 1.680415725925575286319046726692683040162290325505D-1,  &
1.415567675701225879892811622832845252125600939627D-1, 1.006482260551160175038684459742336605269707889822D-1,  &
2.510604860724282479058338820428989444699235030871D-2, 9.402964360009747110031098328922608224934320397592D-3,  &
5.542699233295875168406783695143646338274805359780D-2, 9.986735247403367525720377847755415293097913496236D-2,  &
4.507523056810492466415880450799432587809828791196D-2, 6.300942249647773931746170540321811473310938661469D-2,  &
1.261383225537664703012999637242003647020326905948D-1, 1.273864433581028272878709981850307363453523117880D-1,  &
8.576500414311820514214087864326799153427368592787D-2, 7.102884842310253397447305465997026228407227220665D-2,  &
5.026383572857942403759829860675892897279675661654D-2, 4.683670010609093810432609684738393586390722052124D-3,  &
1.235837891364555000245004813294817451524633100256D-1, 1.148933497158144016800199601785309838604146040215D-1,  &
1.252575774226122633391477702593585307254527198070D-2, 1.239572396231834242194189674243818619042280816640D-1,  &
2.501306413750310579525950767549691151739047969345D-2, 4.915957918146130094258849161350510503556792927578D-2,  &
2.259167374956474713302030584548274729936249753832D-2, 6.362762978782724559269342300509058175967124446839D-2,  &
9.950065827346794643193261975720606296171462239514D-2, 7.048220002718565366098742295389607994441704889441D-2,  &
6.512297339398335645872697307762912795346716454337D-2, 3.998229150313659724790527138690215186863915308702D-2,  &
3.456512257080287509832054272964315588028252136044D-2, 2.212167975884114432760321569298651047876071264944D-3,  &
8.140326425945938045967829319725797511040878579808D-2, 6.583213447600552906273539578430361199084485578379D-2,  &
2.592913726450792546064232192976262988065252032902D-2, 1.187141856692283347609436153545356484256869129472D-1,  &
5.999947605385971985589674757013565610751028128731D-2, 5.500937980198041736910257988346101839062581489820D-2,  &
5.264422421764655969760271538981443718440340270116D-3, 1.533126874056586959338368742803997744815413565014D-2,  &
3.527159369750123100455704702965541866345781113903D-2, 5.000556431653955124212795201196389006184693561679D-2,  &
5.744164831179720106340717579281831675999717767532D-2, 1.598823797283813438301248206397233634639162043386D-2,  &
2.635660410220884993472478832884065450876913559421D-2, 1.196003937945541091670106760660561117114584656319D-2 /)
!------------------------
k = ke + 1
is = istart(k)
ks = length(k)
ldl = xu - xl
bb = ldl*0.5D0
aa = xl + bb

y = 0.0D0
DO i = 1, ks
  c = bb*xx(i)
  IF (i > l1) fv1(i) = f(aa + c)
  IF (i > l2) fv2(i) = f(aa - c)
  j = is + i
  y = y + (fv1(i) + fv2(i))*ww(j)
END DO

y2 = y
y = y*bb
IF (l1 < ks) l1 = ks
IF (l2 < ks) l2 = ks
IF (ke /= k1) RETURN

ya = 0.0D0
DO i = 1, ks
  j = is + i
  ya = ya + (ABS(fv1(i)) + ABS(fv2(i)))*ww(j)
END DO
ya = ya*ABS(bb)

y2 = y2*0.5D0
ym = 0.0D0
DO i = 1, ks
  j = is + i
  ym = ym + (ABS(fv1(i) - y2) + ABS(fv2(i) - y2))*ww(j)
END DO
ym = ym*ABS(bb)
RETURN
END SUBROUTINE qxrul


SUBROUTINE qxrrd (f, z, lz, xl, xu, r, s, lr, ls)

!    TO REORDER THE COMPUTED FUNCTIONAL VALUES BEFORE
!    THE BISECTION OF AN INTERVAL

!   PARAMETERS
!    ON ENTRY
!      F      - REAL
!               FUNCTION SUBPROGRAM DEFINING THE INTEGRAND
!               FUNCTION F(X). THE ACTUAL NAME FOR F NEEDS TO BE
!               DECLARED E X T E R N A L IN THE DRIVER PROGRAM.

!      XL     - REAL
!               LOWER LIMIT OF INTERVAL

!      XU     - REAL
!               UPPER LIMIT OF INTERVAL

!      Z      - REAL
!               VECTOR CONTAINING LZ SAVED FUNCTIONAL VALUES

!      LZ     - INTEGER
!               NUMBER OF ELEMENTS IN LZ

!    ON RETURN
!      R      - REAL
!      S        VECTORS CONTAINING LR, LS
!               SAVED  FUNCTIONAL VALUES FOR THE NEW INTERVALS

!      LR     - INTEGER
!      LS       NUMBER OF ELEMENTES IN R,S RESPECTIVELY

!***ROUTINES CALLED  F
USE constants_NSWC
IMPLICIT NONE

REAL (dp), DIMENSION(:), INTENT(IN)  :: z
REAL (dp), INTENT(IN)                :: xl, xu
INTEGER, INTENT(IN)                  :: lz
REAL (dp), DIMENSION(:), INTENT(OUT) :: r, s
INTEGER, INTENT(OUT)                 :: lr, ls

REAL (dp) :: dlen, centr

INTERFACE
  FUNCTION f(x) RESULT(fx)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN) :: x
    REAL (dp)             :: fx
  END FUNCTION f
END INTERFACE

dlen = (xu - xl)*0.5D0
centr = xl + dlen
r(1) =  z(3)
r(2) =  z(9)
r(3) =  z(4)
r(4) =  z(5)
r(5) =  z(6)
r(6) =  z(10)
r(7) =  z(7)
s(1) =  z(3)
s(2) =  z(8)
s(3) =  z(2)
s(7) =  z(1)
IF (lz > 11) GO TO 10

r(8) =  f(centr + dlen*0.375D0)
r(9) =  f(centr + dlen*0.625D0)
r(10) = f(centr + dlen*0.96875D0)
lr = 10
IF (lz /= 11) s(4) = f(centr - dlen*0.75D0)
IF (lz == 11) s(4) = z(11)
s(5) =  f(centr - dlen*0.875D0)
s(6) =  f(centr - dlen*0.9375D0)
s(8) =  f(centr - dlen*0.375D0)
s(9) =  f(centr - dlen*0.625D0)
s(10) = f(centr - dlen*0.96875D0)
ls = 10
RETURN

10 r(8) = z(12)
r(9) = z(13)
r(10) = z(14)
lr = 10
s(4) = z(11)
s(5) = f(centr - dlen*0.875D0)
s(6) = f(centr - dlen*0.9375D0)
IF (lz > 14) GO TO 20
s(8)  = f(centr - dlen*0.375D0)
s(9)  = f(centr - dlen*0.625D0)
s(10) = f(centr - dlen*0.96875D0)
ls = 10
RETURN

20 r(11) = z(18)
r(12) = z(19)
r(13) = z(20)
r(14) = z(21)
lr = 14
s(8) = z(16)
s(9) = z(15)
s(10) = f(centr - dlen*0.96875D0)
s(11) = z(17)
ls = 11
RETURN
END SUBROUTINE qxrrd

END MODULE adapt_quad

! An algorithm to compute the CDF of the product of two normal random
! variables (which may be correlated).
! Requires the use of the module constants_NSWC from file: constant.f90,
! and the module adapt_quad from file: qxgs.f90

! This version assembled by Alan Miller
! http://users.bigpond.net.au/amiller
! e-mail:  amiller@bigpond.net.au

! Latest revision - 10 December 1997
! N.B. Function FUNCGE has been modified for the case X = 0.


MODULE passer
! Replaces COMMON /passer/
	USE constants_NSWC
	IMPLICIT NONE
	REAL (dp), PUBLIC, SAVE :: xmuyp, xmuxp, zp, rhop, rootr
END MODULE passer

MODULE pass_q
	USE constants_NSWC
	IMPLICIT NONE
	REAL (dp), PUBLIC, SAVE :: xmux, xmuy, rho, target_p, abserr
	INTEGER, PUBLIC, SAVE ::ier, last
end module pass_q


!----------------------------------------------------------------
SUBROUTINE fnprod (xmux, xmuy, rho, z, answer, ier, abserr, last)
!----------------------------------------------------------------

!         SUBROUTINE TO COMPUTE THE PROBABILITY PR(XY < Z) WHERE
!         WHERE X AND Y ARE BIVARIATE NORMAL
!         WITH MEANS XMUX AND XMUY RESPECTIVELY,
!         UNIT VARIANCES, AND CORRELATION RHO

! INPUTS:

!       XMUX         MEAN OF X
!
!       XMUY         MEAN OF Y

!       RHO          CORRELATION COEFFICIENT

!       Z            POINT BELOW WHICH PROBABILITY IS TO BE COMPUTED

! OUTPUTS:

!       ANSWER       COMPUTED PROBABILITY PR(XY < Z)

!       IER          RETURN CONDITION INDICATOR
!                   -1 IF ABS(RHO) = 1 AND THE ALGORITHM FOR THE
!                         DEGENERATE DISTRIBUTION WAS USED.
!                    0 IF ABS(RHO) < 1 AND IF NO ERRORS WERE
!                         DETECTED IN QXGS (DQAGS).
!                         SEE QXGS (DQAGS) DOCUMENTATION FOR MEANING OF
!                         VALUES OF IER > O

!       ABSERR (Optional) Estimated absolute error in ANSWER.

!       LAST   (Optional) The number of sub-intervals of the range of
!                         integration used by QXGS.

! REFERENCE:
! Meeker, W.Q. & Escobar, L.A. (1994) `An algorithm to compute the CDF
!       of the product of two normal random variables', Commun. in
!       Statist.-Simula., vol.23(1), 271-280.

! AUXILIARY ROUTINES:
! DERFC & QXGS (N.B. QXGS is used instead of DQAGS)

!----------------------------------------------------------------
USE constants_NSWC
USE adapt_quad
USE passer
IMPLICIT NONE
REAL (dp), INTENT(IN)            :: xmux, xmuy, rho, z
REAL (dp), INTENT(OUT)           :: answer
INTEGER, INTENT(OUT)             :: ier
REAL (dp), INTENT(OUT), OPTIONAL :: abserr
INTEGER, INTENT(OUT), OPTIONAL   :: last

! Local variables
REAL (dp)            :: errabs, dmrho
REAL (dp), PARAMETER :: zero = 0.0D0, one = 1.0D0
INTEGER              :: final

INTERFACE
  FUNCTION funcge(x) RESULT(fx)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN) :: x
    REAL (dp)             :: fx
  END FUNCTION funcge

  SUBROUTINE fprode(xmux, xmuy, rho, z, answer)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN)  :: xmux, xmuy, rho, z
    REAL (dp), INTENT(OUT) :: answer
  END SUBROUTINE fprode
END INTERFACE

!     CONSTANTS THAT ONE MIGHT WANT TO CHANGE TO
!     ACHIEVE A HIGHER DEGREE OF ACCURACY FROM THE ALGORITHM

REAL (dp), PARAMETER :: xm = 8.0_dp, eps = 1.0E-10_dp
INTEGER, PARAMETER   :: limit = 100

ier = -1

!        CHECK TO SEE IF RHO IS CLOSE TO -1 OR 1

dmrho = SQRT(one - rho*rho)
IF (dmrho > zero) GO TO 40

!        CALL SPECIAL ROUTINE WHEN ABS(RHO) IS EQUAL TO ONE

CALL fprode(xmux, xmuy, rho, z, answer)
RETURN
40 rootr = one/dmrho

!       DEFINE OTHER CONSTANTS NEEDED TO COMPUTE THE INTEGRAND

xmuxp = xmux
xmuyp = xmuy
rhop = rho
zp = z

!        DO THE INTEGRATION

CALL qxgs(funcge, xmux-xm, xmux+xm, eps, eps, answer, errabs, ier, limit,  &
          final)
IF (PRESENT( abserr )) abserr = errabs
IF (PRESENT( last )) last = final

RETURN
END SUBROUTINE fnprod


!----------------------------------------------------------------
FUNCTION funcge(x) RESULT(fn_val)
!----------------------------------------------------------------

!        FUNCTION TO COMPUTE INTEGRAND FOR COMPUTING PROBABILITY
!        OVER THE NONRECTANGULAR REGION

!         THE  FOLLOWING VARIABLES ARE COMMUNICATED THROUGH MODULE passer

!              XMUXP MEAN OF VARIABLE ON THE Y AXIS
!              XMUYP MEAN OF VARIABLE ON THE X AXIS
!              ROOTR = ONE/DSQRT(ONE-RHOR*RHOR)
!              RHOP   CORRELATION BETWEEN VARIABLES ON X AND Y AXES
!              ZP     SPECIFIED VALUE OF Z FOR THE PRODUCT

USE constants_NSWC
USE passer
IMPLICIT NONE
REAL (dp), INTENT(IN) :: x
REAL (dp)             :: fn_val

INTERFACE
  FUNCTION fcdfn(z) RESULT(fn_val)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN) :: z
    REAL (dp)             :: fn_val
  END FUNCTION fcdfn

  FUNCTION phin(x) RESULT(fn_val)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN) :: x
    REAL (dp)             :: fn_val
  END FUNCTION phin
END INTERFACE

! Local variables
REAL (dp)            :: prob, xdiff, xmucy
REAL (dp), PARAMETER :: zero = 0.0_dp, one = 1.0_dp

IF (x == zero) GO TO 99
xdiff = x - xmuxp
xmucy = xmuyp + rhop*xdiff
prob = fcdfn(SIGN(one,x)*rootr*(zp/x-xmucy))
fn_val = prob*phin(xdiff)
RETURN

! 99 fn_val = phin(-xmuxp)
99 fn_val = zero
RETURN
END FUNCTION funcge


!----------------------------------------------------------------
SUBROUTINE fprode(xmux, xmuy, rho, z, answer)
!----------------------------------------------------------------

!          SUBROUTINE TO COMPUTE THE PROBABILITY P(Y<Z) WHERE
!          Y = X(1)*X(2) AND X(1) AND X(2) ARE BIVARIATE NORMAL
!          WITH MEANS XMUX AND XMUY RESPECTIVELY, SIGMA = 1, AND
!          CORRELATION RHO =  -1 OR 1

USE constants_NSWC
IMPLICIT NONE
REAL (dp), INTENT(IN)  :: xmux, xmuy, rho, z
REAL (dp), INTENT(OUT) :: answer

INTERFACE
  FUNCTION fcdfn(z) RESULT(fn_val)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN) :: z
    REAL (dp)             :: fn_val
  END FUNCTION fcdfn
END INTERFACE

! Local variables
REAL (dp)            :: discr, dsqdis, rho1, root1, root2, xmdiff
REAL (dp), PARAMETER :: zero = 0.0_dp, one = 1.0_dp, two = 2.0_dp, &
                        four = 4.0_dp

rho1 = SIGN(one,rho)
xmdiff = xmuy - rho1*xmux
discr = xmdiff**2 + four*rho1*z
IF(discr < zero) GO TO 95
dsqdis = SQRT(discr)
root1 = (-xmdiff-dsqdis) / (two*rho1)
root2 = (-xmdiff+dsqdis) / (two*rho1)
IF(rho1 > zero) THEN
  answer = fcdfn(root2-xmux) - fcdfn(root1-xmux)
ELSE
  answer = fcdfn(xmux-root1) + fcdfn(root2-xmux)
END IF
RETURN

95 IF(rho1 > zero) THEN
  answer = zero
ELSE
  answer = one
END IF

RETURN
END SUBROUTINE fprode


!----------------------------------------------------------------
FUNCTION phin(x) RESULT(fn_val)
!----------------------------------------------------------------

!        STANDARD NORMAL DENSITY

USE constants_NSWC
IMPLICIT NONE
REAL (dp), INTENT(IN) :: x
REAL (dp)             :: fn_val

! Local variables
REAL (dp), PARAMETER  :: cval = .39894228040143_dp, half = 0.5_dp

fn_val = cval*EXP(-half*x*x)
RETURN
END FUNCTION phin


!----------------------------------------------------------------
FUNCTION fcdfn(z) RESULT(fn_val)
!----------------------------------------------------------------

!        STANDARD NORMAL CDF

USE constants_NSWC
IMPLICIT NONE
REAL (dp), INTENT(IN) :: z
REAL (dp)             :: fn_val

INTERFACE
  FUNCTION derfc(x) RESULT(fn_val)
    USE constants_NSWC
    IMPLICIT NONE
    REAL (dp), INTENT(IN) :: x
    REAL (dp)             :: fn_val
  END FUNCTION derfc
END INTERFACE

! Local variables
REAL (dp)             :: zroot
REAL (dp), PARAMETER  :: half = 0.5_dp, root = .7071067811865475_dp

zroot = z*root
fn_val = half*derfc(-zroot)
RETURN
END FUNCTION fcdfn



FUNCTION derfc(x) RESULT(fn_val)
!-----------------------------------------------------------------------
!         EVALUATION OF THE COMPLEMENTARY ERROR FUNCTION
!-----------------------------------------------------------------------
USE constants_NSWC
IMPLICIT NONE
REAL (dp), INTENT(IN) :: x
REAL (dp)             :: fn_val

! Local variables
REAL (dp) :: ax, t, w
INTEGER   :: i, k
REAL (dp), PARAMETER :: a(21) = (/ .1283791670955125738961589031215D+00,  &
                                  -.3761263890318375246320529677070D+00,  &
                                   .1128379167095512573896158902931D+00,  &
                                  -.2686617064513125175943235372542D-01,  &
                                   .5223977625442187842111812447877D-02,  &
                                  -.8548327023450852832540164081187D-03,  &
                                   .1205533298178966425020717182498D-03,  &
                                  -.1492565035840625090430728526820D-04,  &
                                   .1646211436588924261080723578109D-05,  &
                                  -.1636584469123468757408968429674D-06,  &
                                   .1480719281587021715400818627811D-07,  &
                                  -.1229055530145120140800510155331D-08,  &
                                   .9422759058437197017313055084212D-10,  &
                                  -.6711366740969385085896257227159D-11,  &
                                   .4463222608295664017461758843550D-12,  &
                                  -.2783497395542995487275065856998D-13,  &
                                   .1634095572365337143933023780777D-14,  &
                                  -.9052845786901123985710019387938D-16,  &
                                   .4708274559689744439341671426731D-17,  &
                                  -.2187159356685015949749948252160D-18,  &
                                   .7043407712019701609635599701333D-20 /)
!-------------------------------

!                     ABS(X) <= 1

ax = ABS(x)
IF (ax <= 1.d0) THEN
  t = x * x
  w = a(21)
  DO i = 1, 20
    k = 21 - i
    w = t * w + a(k)
  END DO
  fn_val = 0.5D0 + (0.5D0-x*(1.d0+w))
  RETURN
END IF

!                       X < -1

IF (x <= 0.d0) THEN
  fn_val = 2.d0
  IF (x < -8.3D0) RETURN
  t = x * x
  fn_val = 2.d0 - EXP(-t) * derfc0(ax)
  RETURN
END IF

!                       X > 1

fn_val = 0.d0
IF (x > 100.d0) RETURN
t = x * x
IF (t > -dxparg(1)) RETURN
fn_val = EXP(-t) * derfc0(x)
RETURN

CONTAINS


FUNCTION derfc0(x) RESULT(fn_val)
!-----------------------------------------------------------------------
USE constants_NSWC
IMPLICIT NONE
REAL (dp), INTENT(IN) :: x
REAL (dp)             :: fn_val

!           EVALUATION OF EXP(X**2)*ERFC(X) FOR X >= 1

!-----------------------------------------------------------------------
!     WRITTEN BY ALFRED H. MORRIS, JR.
!        NAVAL SURFACE WARFARE CENTER
!        DAHLGREN, VIRGINIA
!        APRIL 1992
!-------------------------------
REAL (dp)            :: t, u, v, z
REAL (dp), PARAMETER :: rpinv = .56418958354775628694807945156077259D0
REAL (dp), PARAMETER :: p0 = .16506148041280876191828601D-03,  &
                        p1 =  .15471455377139313353998665D-03,  &
                        p2 =  .44852548090298868465196794D-04,  &
                        p3 = -.49177280017226285450486205D-05,  &
                        p4 = -.69353602078656412367801676D-05,  &
                        p5 = -.20508667787746282746857743D-05,  &
                        p6 = -.28982842617824971177267380D-06,  &
                        p7 = -.17272433544836633301127174D-07,  &
                        q1 =  .16272656776533322859856317D+01,  &
                        q2 =  .12040996037066026106794322D+01,  &
                        q3 =  .52400246352158386907601472D+00,  &
                        q4 =  .14497345252798672362384241D+00,  &
                        q5 =  .25592517111042546492590736D-01,  &
                        q6 =  .26869088293991371028123158D-02,  &
                        q7 =  .13133767840925681614496481D-03
REAL (dp), PARAMETER :: r0 =  .145589721275038539045668824025D+00,  &
                        r1 = -.273421931495426482902320421863D+00,  &
                        r2 =  .226008066916621506788789064272D+00,  &
                        r3 = -.163571895523923805648814425592D+00,  &
                        r4 =  .102604312032193978662297299832D+00,  &
                        r5 = -.548023266949835519254211506880D-01,  &
                        r6 =  .241432239725390106956523668160D-01,  &
                        r7 = -.822062115403915116036874169600D-02,  &
                        r8 =  .180296241564687154310619200000D-02
REAL (dp), PARAMETER :: a0 = -.45894433406309678202825375D-03,   &
                        a1 = -.12281298722544724287816236D-01,  &
                        a2 = -.91144359512342900801764781D-01,  &
                        a3 = -.28412489223839285652511367D-01,  &
                        a4 =  .14083827189977123530129812D+01,  &
                        a5 =  .11532175281537044570477189D+01,  &
                        a6 = -.72170903389442152112483632D+01,  &
                        a7 = -.19685597805218214001309225D+01,  &
                        a8 =  .93846891504541841150916038D+01,  &
                        b1 =  .25136329960926527692263725D+02,  &
                        b2 =  .15349442087145759184067981D+03,  &
                        b3 = -.29971215958498680905476402D+03,  &
                        b4 = -.33876477506888115226730368D+04,  &
                        b5 =  .28301829314924804988873701D+04,  &
                        b6 =  .22979620942196507068034887D+05,  &
                        b7 = -.24280681522998071562462041D+05,  &
                        b8 = -.36680620673264731899504580D+05,  &
                        b9 =  .42278731622295627627042436D+05,  &
                        b10=  .28834257644413614344549790D+03,  &
                        b11=  .70226293775648358646587341D+03
REAL (dp), PARAMETER :: c0 = -.7040906288250128001000086D-04,   &
                        c1 = -.3858822461760510359506941D-02,  &
                        c2 = -.7708202127512212359395078D-01,  &
                        c3 = -.6713655014557429480440263D+00,  &
                        c4 = -.2081992124162995545731882D+01,  &
                        c5 =  .2898831421475282558867888D+01,  &
                        c6 =  .2199509380600429331650192D+02,  &
                        c7 =  .2907064664404115316722996D+01,  &
                        c8 = -.4766208741588182425380950D+02,  &
                        d1 =  .5238852785508439144747174D+02,  &
                        d2 =  .9646843357714742409535148D+03,  &
                        d3 =  .7007152775135939601804416D+04,  &
                        d4 =  .8515386792259821780601162D+04,  &
                        d5 = -.1002360095177164564992134D+06,  &
                        d6 = -.2065250031331232815791912D+06,  &
                        d7 =  .5695324805290370358175984D+06,  &
                        d8 =  .6589752493461331195697873D+06,  &
                        d9 = -.1192930193156561957631462D+07
REAL (dp), PARAMETER :: e0 = .540464821348814822409610122136D+00,  &
                        e1 = -.261515522487415653487049835220D-01, &
                        e2 = -.288573438386338758794591212600D-02, &
                        e3 = -.529353396945788057720258856000D-03
REAL (dp), PARAMETER :: s1 = .75000000000000000000D+00,   &
        s2  = -.18750000000000000000D+01, s3  = .65625000000000000000D+01,  &
        s4  = -.29531250000000000000D+02, s5  = .16242187500000000000D+03,  &
        s6  = -.10557421875000000000D+04, s7  = .79180664062500000000D+04,  &
        s8  = -.67303564453125000000D+05, s9  = .63938386230468750000D+06,  &
        s10 = -.67135305541992187500D+07, s11 = .77205601373291015625D+08
!-------------------------------
!     RPINV = 1/SQRT(PI)
!-------------------------------

!                     1 <= X <= 2

IF (x <= 2.d0) THEN
  u = ((((((p7*x + p6)*x + p5)*x + p4)*x + p3)*x + p2)*x + p1) * x + p0
  v = ((((((q7*x + q6)*x + q5)*x + q4)*x + q3)*x + q2)*x + q1) * x + 1.d0

  t = (x-3.75D0) / (x+3.75D0)
  fn_val = (((((((((u/v)*t + r8)*t + r7)*t + r6)*t + r5)*t + r4)*t + r3)*t + &
           r2)*t + r1) * t + r0
  RETURN
END IF

!                     2 < X <= 4

IF (x <= 4.d0) THEN
  z = 1.d0 / (2.5D0 + x*x)
  u = (((((((a8*z + a7)*z + a6)*z + a5)*z + a4)*z + a3)*z + a2)*z + a1) * z + a0
  v = ((((((((((b11*z + b10)*z + b9)*z + b8)*z + b7)*z + b6)*z + b5)*z +  &
      b4)*z + b3)*z + b2)*z + b1) * z + 1.d0

  t = 13.d0 * z - 1.d0
  fn_val = ((((u/v)*t + e2)*t + e1)*t + e0) / x
  RETURN
END IF

!                     4 < X < 50

IF (x < 50.d0) THEN
  z = 1.d0 / (2.5D0 + x*x)
  u = (((((((c8*z + c7)*z + c6)*z + c5)*z + c4)*z + c3)*z + c2)*z + c1) * z + &
      c0
  v = ((((((((d9*z + d8)*z + d7)*z + d6)*z + d5)*z + d4)*z + d3)*z + d2)*z +  &
      d1)*z + 1.d0

  t = 13.d0 * z - 1.d0
  fn_val = (((((u/v)*t + e3)*t + e2)*t + e1)*t + e0) / x
  RETURN
END IF

!                        X >= 50

t = (1.d0/x) ** 2
z = (((((((((((s11*t + s10)*t + s9)*t + s8)*t + s7)*t + s6)*t + s5)*t +  &
    s4)*t + s3)*t + s2)*t + s1)*t - 0.5D0) * t + 1.d0
fn_val = rpinv * (z/x)
RETURN
END FUNCTION derfc0
END FUNCTION derfc

!--------------------------------------------------------------------------

!====================================================
! SUBROUTINE test_fnprod
!====================================================
SUBROUTINE test_fnprod (a, b, sea, seb, rho, alpha, lowz, highz, ier, abserr, last)
  USE constants_NSWC
  IMPLICIT NONE
  REAL (dp), INTENT(IN)            :: a, b , sea, seb, rho, alpha
  REAL (dp), INTENT(OUT)           :: lowz, highz
  INTEGER, INTENT(OUT)             :: ier
  REAL (dp), INTENT(OUT), OPTIONAL :: abserr
  INTEGER, INTENT(OUT), OPTIONAL   :: last

  !!Local Variables
  REAL (dp) :: answer, xmux, xmuy, z, za, zb, lper, uper, lstart, ustart,diff, iterate, u0,l0,p_l,p_u
  REAL (dp), PARAMETER :: root_eps=1d-12
  INTEGER, parameter :: max_iter=10000

  INTERFACE
     SUBROUTINE fnprod (xmux, xmuy, rho, z, answer, ier, abserr, last)
       USE constants_NSWC
       USE adapt_quad
       USE passer
       IMPLICIT NONE
       REAL (dp), INTENT(IN)            :: xmux, xmuy, rho, z
       REAL (dp), INTENT(OUT)           :: answer
       INTEGER, INTENT(OUT)             :: ier
       REAL (dp), INTENT(OUT), OPTIONAL :: abserr
       INTEGER, INTENT(OUT), OPTIONAL   :: last
     END SUBROUTINE fnprod
  END INTERFACE

  za = a/sea
  zb = b/seb
  xmux = za
  xmuy = zb
  lper = (alpha/2)
  uper = (1-(alpha/2))
  lstart=(xmux*xmuy+rho)-6*SQRT(xmux*xmux+xmuy*xmuy+2*xmux*xmuy*rho+1)
  ustart=(xmux*xmuy+rho)+6*SQRT(xmux*xmux+xmuy*xmuy+2*xmux*xmuy*rho+1)
  !Loop for the lower limit
  z=-99999
  iterate=0
  diff=1
  l0=lstart
  u0=xmux*xmuy
  CALL fnprod (xmux, xmuy, rho, l0, p_l, ier, abserr, last)
  CALL fnprod (xmux, xmuy, rho, u0, p_u, ier, abserr, last)
  p_l=p_l-lper
  p_u=p_u-lper
  DO WHILE (p_l>0) 
     iterate=iterate+1
     l0=l0-0.5d0
     CALL fnprod (xmux, xmuy, rho, l0, p_l, ier, abserr, last)
     p_l=p_l-lper
     IF (iterate==max_iter) THEN
        lowz=-700
        diff=root_eps/10
        p_l=-1
        p_u=1
     END IF
  END DO
  iterate=0
  DO WHILE (p_u<0) 
     iterate=iterate+1
     u0=u0+0.5d0
     CALL fnprod (xmux, xmuy, rho, u0, p_u, ier, abserr, last)
     p_u=p_u-lper
     IF (iterate==max_iter) THEN
        lowz=-701
        diff=root_eps/10
        p_u=1
     END IF
  END DO
  iterate=0
  DO WHILE(diff>root_eps)
     iterate=iterate+1
     z=(u0+l0)/2
     CALL fnprod (xmux, xmuy, rho, l0, p_l, ier, abserr, last)
     CALL fnprod (xmux, xmuy, rho, u0, p_u, ier, abserr, last)
     CALL fnprod (xmux, xmuy, rho, z, answer, ier, abserr, last)
     p_l=p_l-lper
     p_u=p_u-lper
     answer=answer-lper
     IF ((p_l*p_u)>0) THEN
        lowz=-702
        diff=root_eps/10
        !! GOTO 905
     ELSE IF (iterate>max_iter) THEN
        lowz=-703
        diff=root_eps/10
        !!GOTO 800
     ELSE IF (ABS(answer)<root_eps) THEN
        diff=root_eps/10
        lowz=z*sea*seb
     ELSE IF ((p_l*answer)<0) THEN
        u0=z
        diff=ABS(answer)
     ELSE
        l0=z
        diff=ABS(answer)
     END IF

  END DO
  ! for upper bound
  iterate=0
  diff=1
  l0=xmux*xmuy
  u0=ustart
  CALL fnprod (xmux, xmuy, rho, l0, p_l, ier, abserr, last)
  CALL fnprod (xmux, xmuy, rho, u0, p_u, ier, abserr, last)
  p_l=p_l-uper
  p_u=p_u-uper
  DO WHILE (p_l>0) 
     iterate=iterate+1
     l0=l0-0.5d0
     CALL fnprod (xmux, xmuy, rho, l0, p_l, ier, abserr, last)
     p_l=p_l-uper
     IF (iterate==max_iter) THEN
        highz=-800
        diff=root_eps/10
        p_l=-1
        p_u=1
     END IF
  END DO
  iterate=0
  DO WHILE (p_u<0) 
     iterate=iterate+1
     u0=u0+0.5d0
     CALL fnprod (xmux, xmuy, rho, u0, p_u, ier, abserr, last)
     p_u=p_u-uper
     IF (iterate==max_iter) THEN
        highz=-801
        diff=root_eps/10
        p_u=1
     END IF
  END DO
  iterate=0
  DO WHILE(diff>root_eps)
     iterate=iterate+1
     z=(u0+l0)/2
     CALL fnprod (xmux, xmuy, rho, l0, p_l, ier, abserr, last)
     CALL fnprod (xmux, xmuy, rho, u0, p_u, ier, abserr, last)
     CALL fnprod (xmux, xmuy, rho, z, answer, ier, abserr, last)
     p_l=p_l-uper
     p_u=p_u-uper
     answer=answer-uper
     IF ((p_l*p_u)>0) THEN
        highz=-802
        diff=root_eps/10
     ELSE IF (iterate>max_iter) THEN
        highz=-803
        diff=root_eps/10
     ELSE IF (ABS(answer)<root_eps) THEN
        diff=root_eps/10
        highz=z*sea*seb
     ELSE IF ((p_l*answer)<0) THEN
        u0=z
        diff=ABS(answer)
     ELSE
        l0=z
        diff=ABS(answer)
     END IF
  END DO

!!$800 STOP 'maximum iteration reached!'
!!$900 STOP 'No initial valid lower bound interval for lower tail!'
!!$901 STOP 'No initial valid upper bound interval for lower tail!'
!!$902 STOP 'No initial valid lower bound interval for upper tail!'
!!$903 STOP 'No initial valid upper bound interval for upper tail!'
!!$904 STOP 'Bisection failed!'
END SUBROUTINE test_fnprod

!----------------------------------------
! This function is to compute quantile and is passed to zeroin function
!----------------------------------------

FUNCTION fxprod1 (z) RESULT(answer)
! code by Davood Tofighi and Vanessa Thompson
!Date 7-26-2011
USE constants_NSWC
USE pass_q
IMPLICIT NONE
    REAL (dp), INTENT(IN)            :: z 
    REAL (dp)           :: answer

!local variable
    REAL(dp) :: Fz
! z is the quantile and fprod is a function of z
!target_p is a target percetile which will be passed on to zeroin
!answer=F(z)-target_p

INTERFACE
  SUBROUTINE fnprod (xmux, xmuy, rho, z, answer, ier, abserr, last)
    USE constants_NSWC
    USE adapt_quad
    USE passer
    IMPLICIT NONE
    REAL (dp), INTENT(IN)            :: xmux, xmuy, rho, z
    REAL (dp), INTENT(OUT)           :: answer
    INTEGER, INTENT(OUT)             :: ier
    REAL (dp), INTENT(OUT), OPTIONAL :: abserr
    INTEGER, INTENT(OUT), OPTIONAL   :: last
  END SUBROUTINE fnprod
END INTERFACE

call fnprod (xmux, xmuy, rho, z, Fz , ier, abserr, last)
answer=Fz-target_p;

END FUNCTION fxprod1
!!!!-------------------------------
! END OF function fxprod1
!!!--------------------------------

!!!-----------------
! function of zero in
!--------------------
FUNCTION zeroin(f, ax, bx, aerr, rerr) RESULT(fn_val)
 
! Code converted using TO_F90 by Alan Miller
! Date: 2003-07-14  Time: 12:32:54
 
!-----------------------------------------------------------------------

!         FINDING A ZERO OF THE FUNCTION F(X) IN THE INTERVAL (AX,BX)

!                       ------------------------

!  INPUT...

!  F      FUNCTION SUBPROGRAM WHICH EVALUATES F(X) FOR ANY X IN THE
!         CLOSED INTERVAL (AX,BX).  IT IS ASSUMED THAT F IS CONTINUOUS,
!         AND THAT F(AX) AND F(BX) HAVE DIFFERENT SIGNS.
!  AX     LEFT ENDPOINT OF THE INTERVAL
!  BX     RIGHT ENDPOINT OF THE INTERVAL
!  AERR   THE ABSOLUTE ERROR TOLERANCE TO BE SATISFIED
!  RERR   THE RELATIVE ERROR TOLERANCE TO BE SATISFIED

!  OUTPUT...

!         ABCISSA APPROXIMATING A ZERO OF F IN THE INTERVAL (AX,BX)

!-----------------------------------------------------------------------
!  ZEROIN IS A SLIGHTLY MODIFIED TRANSLATION OF THE ALGOL PROCEDURE
!  ZERO GIVEN BY RICHARD BRENT IN ALGORITHMS FOR MINIMIZATION WITHOUT
!  DERIVATIVES, PRENTICE-HALL, INC. (1973).
!-----------------------------------------------------------------------
USE pass_q
USE constants_NSWC
IMPLICIT NONE
!INTEGER, PARAMETER  :: dp = SELECTED_REAL_KIND(14, 60)

REAL (dp), INTENT(IN)  :: ax
REAL (dp), INTENT(IN)  :: bx
REAL (dp), INTENT(IN)  :: aerr
REAL (dp), INTENT(IN)  :: rerr
REAL (dp)              :: fn_val

! EXTERNAL f
INTERFACE
  FUNCTION f(z) RESULT(answer)
    USE constants_NSWC
    USE pass_q
    IMPLICIT NONE
!    INTEGER, PARAMETER  :: dp = SELECTED_REAL_KIND(14, 60)
    REAL (dp), INTENT(IN)  :: z
    REAL (dp)              :: answer
  END FUNCTION f
END INTERFACE

REAL (dp)  :: a, b, c, d, e, eps, fa, fb, fc, tol, xm, p, q, r, s, atol, rtol

!  COMPUTE EPS, THE RELATIVE MACHINE PRECISION

eps = EPSILON(0.0_dp)

! INITIALIZATION

a = ax
b = bx
fa = f(a)
fb = f(b)
atol = 0.5 * aerr
rtol = MAX(0.5_dp*rerr, 2.0_dp*eps)

! BEGIN STEP

10 c = a
fc = fa
d = b - a
e = d
20 IF (ABS(fc) < ABS(fb)) THEN
  a = b
  b = c
  c = a
  fa = fb
  fb = fc
  fc = fa
END IF

! CONVERGENCE TEST

tol = rtol * MAX(ABS(b),ABS(c)) + atol
xm = 0.5 * (c-b)
IF (ABS(xm) > tol) THEN
  IF (fb /= 0.0) THEN
    
! IS BISECTION NECESSARY
    
    IF (ABS(e) >= tol) THEN
      IF (ABS(fa) > ABS(fb)) THEN
        
! IS QUADRATIC INTERPOLATION POSSIBLE
        
        IF (a == c) THEN
          
! LINEAR INTERPOLATION
          
          s = fb / fc
          p = (c-b) * s
          q = 1.0 - s
        ELSE
          
! INVERSE QUADRATIC INTERPOLATION
          
          q = fa / fc
          r = fb / fc
          s = fb / fa
          p = s * ((c-b)*q*(q-r)-(b-a)*(r-1.0))
          q = (q-1.0) * (r-1.0) * (s-1.0)
        END IF
        
! ADJUST SIGNS
        
        IF (p > 0.0) q = -q
        p = ABS(p)
        
! IS INTERPOLATION ACCEPTABLE
        
        IF (2.0*p < (3.0*xm*q-ABS(tol*q))) THEN
          IF (p < ABS(0.5*e*q)) THEN
            e = d
            d = p / q
            GO TO 30
          END IF
        END IF
      END IF
    END IF
    
! BISECTION
    
    d = xm
    e = d
    
! COMPLETE STEP
    
    30 a = b
    fa = fb
    IF (ABS(d) > tol) b = b + d
    IF (ABS(d) <= tol) b = b + SIGN(tol,xm)
    fb = f(b)
    IF (fb*(fc/ABS(fc)) > 0.0) GO TO 10
    GO TO 20
  END IF
END IF

! DONE

fn_val = b
RETURN
END FUNCTION zeroin
!-------------------------
! End of function zeroin
!---------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Quantile for the distribution of the product
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine quantile_prodclin (perc,a, b, sea, seb, rho1, answer , ier1, abserr1, last1)
USE constants_NSWC
USE pass_q
IMPLICIT NONE
REAL (dp), intent(in)           :: a, b , sea, seb,  perc, rho1
REAL (dp), intent(out)           :: answer
INTEGER, INTENT(OUT)             :: ier1
real (dp), INTENT(OUT), OPTIONAL :: abserr1
INTEGER, INTENT(OUT), OPTIONAL   :: last1

!!Local Variables
REAL (dp) ::  za, zb, lstart, ustart,rerr, eps , se_ab
REAL (dp), PARAMETER :: aerr=1.0E-10_dp
INTEGER, parameter :: max_iter=10000
INTEGER ::iter
interface
FUNCTION fxprod1 (z) RESULT(answer)
! code by Davood Tofighi and Vanessa Thompson
!Date 7-26-2011
USE constants_NSWC
USE pass_q
    REAL (dp), INTENT(IN)            :: z 
    REAL (dp)           :: answer
end function fxprod1
end interface  

interface
FUNCTION zeroin(f, ax, bx, aerr, rerr) RESULT(fn_val)
USE pass_q
USE constants_NSWC
REAL (dp), INTENT(IN)  :: ax
REAL (dp), INTENT(IN)  :: bx
REAL (dp), INTENT(IN)  :: aerr
REAL (dp), INTENT(IN)  :: rerr
REAL (dp)              :: fn_val
INTERFACE
  FUNCTION f(z) RESULT(answer)
    USE constants_NSWC
    USE pass_q
    IMPLICIT NONE
    REAL (dp), INTENT(IN)  :: z
    REAL (dp)              :: answer
  END FUNCTION f
END INTERFACE
END function
end interface

!print *, "enter p:"
!read (*,*) perc
!print *, "enter a:"
!read (*,*) a
!print *, "enter b:"
!read (*,*) b
!print *, "se a:"
!read (*,*) sea
!print *, "se b:"
!read (*,*) seb
!print *, "rho:"
!read (*,*) r

eps = EPSILON(0.0_dp)
rerr=2.0_dp*eps

za = a/sea
zb = b/seb
!from pass_q
xmux = za 
xmuy = zb
target_p=perc
rho=rho1
ier1=ier
last1=last
abserr1=abserr

se_ab=SQRT(xmux*xmux+xmuy*xmuy+2*xmux*xmuy*rho+1)
lstart=(xmux*xmuy+rho)-6*se_ab
ustart=(xmux*xmuy+rho)+6*se_ab
!initial upper bound
iter=0
do while (fxprod1(ustart)<0)
iter=iter+1
  IF (iter==max_iter) THEN  !modified 3/4/2013 Davood
       answer=-703
       EXIT
   !stop 'Initial upper bound cannot be found'
  END IF
ustart=ustart+.5*se_ab
end do

! initial lower bound
IF (answer/=-703) THEN !modified 3/4/2013 Davood
iter=0
do while (fxprod1(lstart)>0)
iter=iter+1
   IF (iter==max_iter) THEN !modified 3/4/2013 Davood
      answer=-703
      EXIT
   !stop 'Initial lower bound cannot be found'
  END IF
lstart=lstart-.5*se_ab
end do
end IF

IF (answer/=-703) THEN  !modified 3/4/2013 Davood
answer=zeroin(fxprod1,lstart,ustart,aerr, rerr)
answer=answer*sea*seb
END IF
END subroutine quantile_prodclin
