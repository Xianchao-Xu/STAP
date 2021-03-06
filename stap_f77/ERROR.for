      SUBROUTINE ERROR (N, I)
C     当分配的存储空间不够时，输出错误信息
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT

      GO TO (1,2,3,4),I
    1 WRITE (IOUT,2000)
      GO TO 6
    2 WRITE (IOUT,2010)
      GO TO 6
    3 WRITE (IOUT,2020)
      GO TO 6
    4 WRITE (IOUT,2030)

    6 WRITE (IOUT,2050) N
      STOP

 2000 FORMAT (//, ' NOT ENOUGH STORAGE FOR ID ARRAY AND NODAL POINT ',
     1        'COORDINATES')
 2010 FORMAT (//, ' NOT ENOUGH STORAGE FOR DEFINITION OF LOAD VECTORS')
 2020 FORMAT (//, ' NOT ENOUGH STORAGE FOR ELEMENT DATA INPUT')
 2030 FORMAT (//, ' NOT ENOUGH STORAGE FOR ASSEMBAGE OF GLOBAL ',
     1        'STRUCTURE STIFFINESS, AND DISPLACEMENT AND STRESS ',
     2        'SOLUTION PHASE')
 2050 FORMAT (//, ' *** ERROR *** STORAGE EXCEEDED BY ', I9)
      END