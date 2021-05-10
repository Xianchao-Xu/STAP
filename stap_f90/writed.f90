subroutine writed(v, numnp, id)
    ! 输出位移信息
    use common_mod
    implicit none
    integer, parameter       :: iwp = selected_real_kind(15)
    real(iwp), intent(in   ) :: v(:)
    integer,   intent(in   ) :: numnp
    integer,   intent(in   ) :: id(:, :)
    
    real(iwp)                :: d(3)
    integer                  :: i, ic, ii, il, kk
    
    write(iout, 2000)
    ic = 4
    do ii = 1, numnp
        ic = ic + 1
        if (ic >= 56) then
            write(iout, 2000)
            ic = 4
        end if
        do i = 1, 3
            d(i) = 0.0
        end do
        do i = 1, 3
            kk = id(i, ii)
            il = i
            if (kk /= 0) d(il) = v(kk)
        end do
        write(iout, 2010) ii, d
    end do
    
2000 format(///, ' D I S P L A C E M E N T S', //, '  NODE ', 10X,        &
     'X-DISPLACEMENT    Y-DISPLACEMENT    Z-DISPLACEMENT')
2010 format(1X, I3, 8X, 3E18.6)
end subroutine writed