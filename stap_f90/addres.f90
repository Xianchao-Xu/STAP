subroutine addres(mht, maxa, mk, nwk)
    ! 计算对角元素在刚度矩阵中的位置
    implicit none
    integer,   intent(in   ) :: mht(:)
    integer,   intent(  out) :: maxa(:)
    integer,   intent(  out) :: mk
    integer,   intent(  out) :: nwk
    
    integer                  :: neq
    integer                  :: i
    
    neq = ubound(mht, 1)
    maxa(1) = 1
    maxa(2) = 2
    mk = 0
    if (neq /= 1) then
        do i = 2, neq
            if (mht(i) > mk) mk = mht(i)
            maxa(i+1) = maxa(i) + mht(i) + 1
        end do
    end if
    mk = mk + 1
    nwk = maxa(neq+1) - maxa(1)
end subroutine addres