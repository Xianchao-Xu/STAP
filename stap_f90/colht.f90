subroutine colht(mht, nd, lm_n)
    ! ����նȾ�����и�
    implicit none
    integer,   intent(  out) :: mht(:)
    integer,   intent(in   ) :: nd
    integer,   intent(in   ) :: lm_n(:)
    
    integer                  :: i, ii, ls, me
    
    ! �ҵ���ǰ��Ԫ�е���С���ɶȱ��
    ls = 1E9
    do i = 1, nd
        if (lm_n(i) /= 0 .and. lm_n(i) - ls < 0) ls = lm_n(i)
    end do
    
    ! ��ȡ�и�
    do i = 1, nd
        ! ii: ���ɶȱ�ţ�Ҳ�ǵ�i���Խ���Ԫ��
        ii = lm_n(i)  
        if (ii /= 0) me = ii - ls
        if (me > mht(ii)) mht(ii) = me
    end do
    
end subroutine colht