      SUBROUTINE COLHT (MHT, ND, LM)
C     �����иߣ��������У��и߲�����Խ���Ԫ�أ�
C     �иߵ���i-mi�����У�
C         i���кţ�Ҳ�������ɶȱ��
C         mi�ǵ�i���׸�����Ԫ�ص��кţ�Ҳ���ǵ�Ԫ�е���С���ɶȱ��
      COMMON /SOL/ NUMNP,NEQ,NWK,NUMEST,MIDEST,MAXEST,MK
      DIMENSION LM(1),MHT(1)
      
C     ѭ�����LSΪ��Ԫ�з��̱����С�����ɶ�
      LS = 100000
      DO 100 I = 1, ND
          IF (LM(I)) 110, 100, 110
  110     IF (LM(I) - LS) 120, 100, 100
  120     LS = LM(I)
  100 CONTINUE
      
C     ������ȡ�и�
C     ��������У�IIΪ���ɶȱ�ţ��ǵ�i���Խ���Ԫ��
C     ���ڸնȾ���Ϊ�Գƾ����ڱ�������ֻ�洢��������
C     ���ԣ����ɶȱ�Ŵ���II�Ļ���������������У�����Ҫ����
      DO 200 I = 1, ND
          II = LM(I)
          IF (II .EQ. 0) GO TO 200
          ME = II - LS
          IF (ME .GT. MHT(II)) MHT(II) = ME
  200 CONTINUE
      RETURN
      END
