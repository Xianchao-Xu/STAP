subroutine addban(a, s, lm_n, maxa, nd)
    ! 装配单元刚度矩阵到整体刚度矩阵中
    use common_mod
    implicit none
    integer, parameter       :: iwp = selected_real_kind(15)
    real(iwp), intent(inout) :: a(:)
    real(iwp), intent(in   ) :: s(:)
    integer,   intent(in   ) :: lm_n(:)
    integer,   intent(in   ) :: maxa(:)
    integer,   intent(in   ) :: nd
    
    integer                  :: i, ii, ij, j, jj, kk, ks, kss, mi, ndi
    
    ndi = 0
    do i = 1, nd
        ii = lm_n(i)
        if (ii > 0) then
            mi = maxa(ii)
            ks = i
            do j = 1, nd
                jj = lm_n(j)
                if (jj > 0) then
                    ij = ii - jj
                    if (ij >= 0) then
                        kk = mi + ij
                        kss = ks
                        if (j > i) kss = j + ndi
                        a(kk) = a(kk) + s(kss)
                    end if
                end if
                ks = ks + nd - j
            end do
        end if
        ndi = ndi + nd - i
    end do
end subroutine