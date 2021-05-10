      SUBROUTINE CLEAR (A,N)
C     Çå¿ÕÊý×éA
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      DIMENSION A(1)
      DO 10 I = 1,N
   10 A(I) = 0.
      RETURN
      END