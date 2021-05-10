C     NWK：一维数组中存储的刚度矩阵元素个数
C     MK：半带宽
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK
      COMMON /DIM/ N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15
      COMMON /EL/ IND,NPAR(10),NUMEG,MTOT,NFIRST,NLAST,ITWO
      COMMON /VAR/ NG,MODEX
      COMMON /TAPES/ IELMNT,ILOAD,IIN,IOUT

      DIMENSION TIM(5), HED(20)
      DIMENSION IA(1)
      EQUIVALENCE (A(1),IA(1))

C     最大存储空间
      COMMON A(10000)
      MTOT = 10000

C     双精度
      ITWO = 2

C     输入输出设备号
      IELMNT = 10
      ILOAD = 20
      IIN = 50
      IOUT = 60
      
      OPEN(IELMNT, FILE='..\..\ELEMENT.BIN',FORM='UNFORMATTED')
      OPEN(ILOAD, FILE='..\..\LOAD.BIN',FORM='UNFORMATTED')
      OPEN(IIN, FILE='..\..\TEST.INP')
      OPEN(IOUT, FILE='..\..\TEST.OUT')

  200 NUMEST = 0
      MAXEST = 0
      
C     *******************************************************************
C     *******************************************************************
C     输入阶段
C     *******************************************************************
C     *******************************************************************

      CALL SECOND (TIM(1))
C     *******************************************************************
C     读取控制信息
C     HED:：顶部标签
C     NUMNP：节点数
C     NUMEG：单元组数
C     NLCASE：工况组数
C     MODEX：求解模式，0：检查数据；1：计算
C     *******************************************************************
      READ (IIN, 1000) HED,NUMNP,NUMEG,NLCASE,MODEX
      IF (NUMNP .EQ. 0) GO TO 800
      WRITE (IOUT, 2000) HED,NUMNP,NUMEG,NLCASE,MODEX

C     *******************************************************************
C     读取节点信息
C     *******************************************************************
      N1 = 1
      N2 = N1 + 3*NUMNP
      N2 = (N2/2)*2 + 1
      N3 = N2 + NUMNP * ITWO
      N4 = N3 + NUMNP * ITWO
      N5 = N4 + NUMNP * ITWO
      IF (N5 .GT. MTOT) CALL ERROR (N5-MTOT, 1)
      
C                 ID      X     Y     Z
      CALL INPUT (A(N1),A(N2),A(N3),A(N4),NUMNP,NEQ)
      
      NEQ1 = NEQ + 1
      
C     *******************************************************************
C     计算并存储载荷向量
C     *******************************************************************
      N6 = N5 + NEQ * ITWO
      WRITE(IOUT, 2005)
      
      REWIND ILOAD
      
      DO 300 L = 1, NLCASE
          
          READ(IIN, 1010)LL, NLOAD
          WRITE(IOUT, 2010) LL, NLOAD
          
          IF (LL .EQ. L) GO TO 310
          WRITE(IOUT, 2020)
          GO TO 800
          
C         The CONTINUE statement is a "do-nothing" statement.
  310     CONTINUE
          
          N7 = N6 + NLOAD
          N8 = N7 + NLOAD
          N9 = N8 + NLOAD * ITWO
          
          IF (N9 .GT. MTOT) CALL ERROR(N9-MTOT,2)
          
          CALL LOADS(A(N5),A(N6),A(N7),A(N8),A(N1),NLOAD,NEQ)
          
  300 CONTINUE
      
C     *******************************************************************
C     读取、生成、存储单元信息
C     *******************************************************************
      N6 = N5 + NEQ
      N6 = (N6 / 2) * 2 + 1
      DO 10 I = N5, N6
   10 IA(I) = 0
C     ELCAL、ASSEM和STRESS三个子程序都会调用ELEMNT子程序
C     IND是一个标识符，用于确定单元子程序执行的是什么操作
C         为1：读取单元信息
C         为2：装配刚度矩阵
C         为3：计算应力
      IND = 1
      CALL ELCAL
      
C     *******************************************************************
C     *******************************************************************
C     求解阶段
C     *******************************************************************
C     *******************************************************************
      CALL SECOND (TIM(2))
      
C     *******************************************************************
C     组装刚度矩阵
C     *******************************************************************
      CALL ADDRES (A(N2),A(N5))
      
      MM = NWK / NEQ
      N3 = N2 + NEQ + 1
      N3 = (N3 / 2) * 2 + 1
      N4 = N3 + NWK * ITWO
      N5 = N4 + NEQ * ITWO
      N6 = N5 + MAXEST
      IF (N6 .GT. MTOT) CALL ERROR (N6-MTOT, 4)
      
C     *******************************************************************
C     输出全局系统数据
C     *******************************************************************
      WRITE(IOUT,2025)NEQ, NWK, MK, MM
      
      IF (MODEX .GT. 0) GO TO 100
      CALL SECOND (TIM(3))
      CALL SECOND (TIM(4))
      CALL SECOND (TIM(5))
      GO TO 120
      
C     *******************************************************************
C     清空存储
C     *******************************************************************
  100 NNL = NWK + NEQ
      CALL CLEAR (A(N3),NNL)
      
      IND = 2
      CALL ASSEM (A(N5))
      
      CALL SECOND (TIM(3))
      
C     *******************************************************************
C     刚度矩阵的三角分解
C     *******************************************************************
      KTR = 1
      CALL COLSOL(A(N3),A(N4),A(N2),NEQ,NWK,NEQ1,KTR)
      
   35 CALL SECOND (TIME(4))
      
      KTR = 2
      IND = 3
      
      REWIND ILOAD
      DO 400 L = 1, NLCASE
          CALL LOADV (A(N4), NEQ)
C         ***************************************************************
C         计算位移
C         ***************************************************************
          CALL COLSOL(A(N3),A(N4),A(N2),NEQ,NWK,NEQ1,KTR)
          WRITE(IOUT, 2015) L
          CALL WRITED(A(N4), A(N1), NEQ, NUMNP)
C         ***************************************************************
C         计算应力
C         ***************************************************************
          CALL STRESS(A(N5))

  400 CONTINUE
      
  120 TT = 0.
      DO 500 I = 1, 4
          TIM(I) = TIM(I+1) - TIM(I)
  500 TT = TT + TIM(I)
      WRITE(IOUT,2030) HED,(TIM(I),I=1,4),TT
      
C     *******************************************************************
C     读取另一个分析
C     *******************************************************************
      GO TO 200

C     *******************************************************************
C     *******************************************************************
  800 STOP
 1000 FORMAT(20A4,/,4I5)
 1010 FORMAT(2I5)
 2000 FORMAT(///,' ',20A4,///,
     1    ' C O N T R O L   I N F O R M A T I O N',//,
     2    '      NUMBER OF NODAL POINTS',10(' .'),' (NUMNP)  = ',I5,//,
     3    '      NUMBER OF ELEMENT GROUPS',9(' .'),' (NUMEG)  = ',I5,//,
     4    '      NUMBER OF LOAD CASES',11(' .'),' (NLCASE) = ',I5,//,
     5    '      SOLUTION MODE ',14(' .'),' (MODEX)  = ',I5,/,
     6    '         EQ.0, DATA CHECK',/,
     7    '         EQ.1, EXECUTION')
 2005 FORMAT(//,' L O A D   C A S E   D A T A')
 2010 FORMAT(////,'     LOAD CASE NUMBER',7(' .'),' = ',I5,//,
     1       '     NUMBER OF CONCENTRATED LOADS . = ',I5)
 2015 FORMAT(//,' LOAD CASE ', I3)
 2020 FORMAT(' *** ERROR *** LOAD CASES ARE NOT IN ORDER')
 2025 FORMAT(//,' TOTAL SYSTEM DATA',///,
     1      '     NUMBER OF EQUATIONS',14(' .'),'(NEQ) = ',I5,//,
     2      '     NUMBER OF MATRIX ELEMENTS',11(' .'),'(NWK) = ',I5,//,
     3      '     MAXIMUM HALF BANDWIDTH ',12(' .'),'(MK ) = ',I5,//,
     4      '     MEAN HALF BANDWIDTH',14(' .'),'(MM ) = ',I5)
 2030 FORMAT(//,' S O L U T I O N   T I M E   L O G   I N   S E C',//,
     1 '            FOR PROBLEM',//,' ',20A4,///,
     2 '     TIME FOR INPUT PHASE ',14(' .'),' =',F12.2,//,
     3 '     TIME FOR CALCULATION OF STIFFNESS MATRIX  . . . . =',F12.2,
     4 //,
     5 '     TIME FOR FACTORIZATION OF STIFFNESS MATRIX  . . . =',F12.2,
     6 //,
     7 '     TIME FOR LOAD CASE SOLUTIONS ',10(' .'),' =',F12.2,///,
     8 '      T O T A L   S O L U T I O N   T I M E  . . . . . =',F12.2)
      END