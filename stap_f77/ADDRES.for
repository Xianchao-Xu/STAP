      SUBROUTINE ADDRES (MAXA,MHT)
C     ����һά�����洢�ĸնȾ���Խ���Ԫ�ص�λ��
C         MHT: �и�
C         MAXA: ��λ�������Խ�Ԫ��λ��
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK
      DIMENSION MAXA(*),MHT(*)
      
C     ��ʼ����λ����
      NN = NEQ + 1
      DO 20 I = 1, NN
   20 MAXA(I) = 0.0
      
      MAXA(1) = 1
      MAXA(2) = 2
      MK = 0
      IF (NEQ .EQ. 1) GO TO 100
      DO 10 I = 2, NEQ
          IF (MHT(I) .GT. MK) MK = MHT(I)
   10 MAXA(I+1) = MAXA(I) + MHT(I) + 1
  100 MK = MK + 1
C     NWK: һά�����д洢�ĸն�Ԫ�ظ���
      NWK = MAXA(NEQ+1) - MAXA(1)
      RETURN
      END