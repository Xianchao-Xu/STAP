subroutine loads(r, caseid, nload, id, modex)
    ! 读取节点载荷数据
    ! 为每一个工况生成载荷向量
    use common_mod
    implicit none
    integer, parameter       :: iwp = selected_real_kind(15)
    real(iwp), intent(inout) :: r(:, :)
    integer,   intent(in   ) :: caseid
    integer,   intent(in   ) :: nload
    integer,   intent(in   ) :: id(:, :)
    integer,   intent(in   ) :: modex

    integer                  :: nod(nload)
    integer                  :: idirn(nload)
    real(iwp)                :: fload(nload)
    
    integer                  :: i, ii, l, li, ln
    
    write(iout, 2000)
    read(iin, 1000) (nod(i), idirn(i), fload(i), i = 1, nload)
    write(iout, 2010) (nod(i), idirn(i), fload(i), i = 1, nload)
    if (modex == 0) return
    
    do l = 1, nload
        ln = nod(l)
        li = idirn(l)
        ii = id(li, ln)
        if (ii > 0) r(ii, caseid) = r(ii, caseid) + fload(l)
    end do
    
1000 format(2I5, F10.0)
2000 format(//, '    NODE        DIRECTION      LOAD', /, '   NUMBER',    &
     20X, 'MAGNITUDE')
2010 format(' ', I6, 9X, I4, 7X, E12.5)

end subroutine loads