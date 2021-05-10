subroutine colsol(a, v, maxa, neq, kkk)
    ! �������Ԫ��ƽ�ⷽ�̣��նȾ���Ϊһά�����洢��
    use common_mod
    implicit none
    integer, parameter       :: iwp = selected_real_kind(15)
    real(iwp), intent(inout) :: a(:)
    real(iwp), intent(inout) :: v(:)
    integer,   intent(in   ) :: maxa(:)
    integer,   intent(in   ) :: neq
    integer,   intent(in   ) :: kkk
    
    integer             :: kn  ! ��n�жԽ�Ԫ��������a���նȾ����е�λ��
    integer             :: kl  ! �Խ�Ԫ����һ��������a�е�λ��
    integer             :: ku  ! ��n���׸�����Ԫ����a�е�λ��
    integer             :: kh  ! ��n��ȥ���Խ�Ԫ�غ͵�һ������Ԫ�غ��Ԫ������
    integer             :: klt ! ��n�е�k��Ԫ��������a�е�λ��
    integer             :: k   ! �к�
    integer             :: ki  ! ��k�жԽ���Ԫ��������a�е�λ��
    integer             :: nd  ! ��n�е�k��Ԫ�����n���׸�����Ԫ����a�����д洢λ�õļ��
    
    integer                  :: ic, j, kk, l, n
    
    real(iwp)                :: b, c
    
    if (kkk - 2 < 0) then
        ! *****************************************************************
        ! LDLT�ֽ⣬��ȡL��D
        ! *****************************************************************
        do n = 1, neq
            kn = maxa(n)
            kl = kn + 1
            ku = maxa(n+1) - 1
            kh = ku - kl
            if (kh < 0) then
                if (a(kn) <= 0) then
                    write(iout, 2000) n, a(kn)
                    stop
                end if
            else
                if (kh > 0) then
                    k = n - kh
                    ic = 0
                    klt = ku
                    do j = 1, kh
                        ic = ic + 1
                        klt = klt - 1
                        ki = maxa(k)
                        nd = maxa(k+1) - ki - 1
                        if (nd > 0) then
                            kk = min0(ic, nd)
                            c = 0.0
                            do l = 1, kk
                                c = c + a(ki+l) * a(klt+l)
                            end do
                            a(klt) = a(klt) - c
                        end if
                        k = k + 1
                    end do
                end if
                k = n
                b = 0.
                do kk = kl, ku
                    k = k - 1
                    ki = maxa(k)
                    c = a(kk) / a(ki)
                    b = b + c * a(kk)
                    a(kk) = c
                end do
                a(kn) = a(kn) - b
            end if
        end do
    else
        ! *****************************************************************
        ! �����غ�����
        ! *****************************************************************
        do n = 1, neq
            kl = maxa(n) + 1
            ku = maxa(n+1) - 1
            if (ku - kl >= 0) then
                k = n
                c = 0.
                do kk = kl, ku
                    k = k - 1
                    c = c + a(kk) * v(k)
                end do
                v(n) = v(n) - c
            end if
        end do
        
        ! *****************************************************************
        ! �ش�
        ! *****************************************************************
        do n = 1, neq
            k = maxa(n)
            v(n) = v(n) / a(k)
        end do
        if (neq /= 1) then
            n = neq
            do l = 2, neq
                kl = maxa(n) + 1
                ku = maxa(n+1) - 1
                if (ku - kl >= 0) then
                    k = n
                    do kk = kl, ku
                        k = k - 1
                        v(k) = v(k) - a(kk) * v(n)
                    end do
                end if
                n = n - 1
            end do
        end if
    end if
    
2000 format(//' STOP - STIFFINESS MATRIX NOT POSITIVE DEFINITE', //,      &
     ' NONPOSITIVE PIVOT FOR EQUATION ', I8, //, ' PIVOT = ', E20.12)

end subroutine colsol