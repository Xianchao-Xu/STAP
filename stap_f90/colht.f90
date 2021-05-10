subroutine colht(mht, nd, lm_n)
    ! 计算刚度矩阵的列高
    implicit none
    integer,   intent(  out) :: mht(:)
    integer,   intent(in   ) :: nd
    integer,   intent(in   ) :: lm_n(:)
    
    integer                  :: i, ii, ls, me
    
    ! 找到当前单元中的最小自由度编号
    ls = 1E9
    do i = 1, nd
        if (lm_n(i) /= 0 .and. lm_n(i) - ls < 0) ls = lm_n(i)
    end do
    
    ! 获取列高
    do i = 1, nd
        ! ii: 自由度编号，也是第i个对角线元素
        ii = lm_n(i)  
        if (ii /= 0) me = ii - ls
        if (me > mht(ii)) mht(ii) = me
    end do
    
end subroutine colht