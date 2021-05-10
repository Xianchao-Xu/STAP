subroutine truss(npar, ind, id, maxa, xyz, xyz_el, lm, matp, e, area,     &
        mht, a, v, ng)
    ! 桁架单元
    use common_mod
    implicit none
    
    integer, parameter       :: iwp = selected_real_kind(15)
    integer,   intent(inout) :: npar(:)
    integer,   intent(in   ) :: ind
    integer,   intent(in   ) :: id(:, :)
    integer,   intent(in   ) :: maxa(:)
    real(iwp), intent(in   ) :: xyz(:, :)
    
    real(iwp), intent(inout), allocatable :: xyz_el(:, :)
    integer,   intent(inout), allocatable :: lm(:, :)
    real(iwp), intent(inout), allocatable :: matp(:)
    real(iwp), intent(inout), allocatable :: e(:)
    real(iwp), intent(inout), allocatable :: area(:)
    integer,   intent(  out) :: mht(:)
    real(iwp), intent(inout) :: a(:)
    real(iwp), intent(in   ), optional :: v(:)
    integer,   intent(in   ), optional :: ng
    
    integer                  :: nd
    integer                  :: npar1, nume, nummat
    integer                  :: mtyp, mtype
    integer                  :: i, ii, j, jj, k, kg, kkk, kl, l, m, n
    integer                  :: iprint
    real(iwp)                :: xl, xl2, xx, yy, str, p
    real(iwp)                :: d(3), s(21), st(6)
    
    nd = 6
    
    npar1 = npar(1)
    nume = npar(2)
    select case (ind)
    ! *********************************************************************
    ! 读取和生成单元信息
    ! *********************************************************************
    case(1)
        ! *****************************************************************
        ! 读取材料信息
        ! *****************************************************************
        write(iout, 2000) npar1, nume
        if (npar(3) == 0) npar(3) = 1
        write(iout, 2010) npar(3)
        
        nummat = npar(3)
        write(iout, 2020)
        if (allocated(xyz_el)) deallocate(xyz_el)
        if (allocated(lm)) deallocate(lm)
        if (allocated(matp)) deallocate(matp)
        if (allocated(e)) deallocate(e)
        if (allocated(area)) deallocate(area)
        allocate(xyz_el(6, nume), lm(6, nume), matp(nume), e(nummat),     &
            area(nummat))
        do i = 1, nummat
            read(iin, 1000) n, e(n), area(n)
            write(iout, 2030) n, e(n), area(n)
        end do
        
        ! *****************************************************************
        ! 读取单元信息
        ! *****************************************************************
        write(iout, 2040)
        n = 1
        read(iin, 1020) m, ii, jj, mtyp, kg
        if (kg == 0) kg = 1
        do 
            if (m == n) then
                i = ii
                j = jj
                mtype = mtyp
                kkk = kg
            end if
            
            xyz_el(1:3, n) = xyz(:, i)
            xyz_el(4:6, n) = xyz(:, j)
            
            matp(n) = mtype
            
            lm(:, n) = 0
            lm(1:3, n) = id(:, i)
            lm(4:6, n) = id(:, j)
            
            call colht(mht, nd, lm(:, n))
            
            write(iout, 2050) n, i, j, mtype
            if (n == nume) return
            n = n + 1
            i = i + kkk
            j = j + kkk
            
            if (n > m) then
                read(iin, 1020) m, ii, jj, mtyp, kg
                if (kg == 0) kg = 1
            end if
        end do
    ! *********************************************************************
    ! 组装刚度矩阵
    ! *********************************************************************
    case(2)
        do n = 1, nume
            mtype = matp(n)
            xl2 = 0
            do l = 1, 3
                d(l) = xyz_el(l, n) - xyz_el(l+3, n)
                xl2 = xl2 + d(l) * d(l)
            end do
            xl = sqrt(xl2)
            xx = e(mtype) * area(mtype) * xl
            do l = 1, 3
                st(l) = d(l) / xl2
                st(l+3) = -st(l)
            end do
            
            kl = 0
            do l = 1, 6
                yy = st(l) * xx
                do k = l, 6
                    kl = kl + 1
                    s(kl) = st(k) * yy
                end do
            end do
            call addban(a, s, lm(:, n), maxa, nd)
        end do
    ! *********************************************************************
    ! 计算应力
    ! *********************************************************************
    case(3)
        iprint = 0
        do n = 1, nume
            iprint = iprint + 1
            if (iprint > 50) iprint = 1
            if (iprint == 1) write(iout, 2060) ng
            
            mtype = matp(n)
            
            xl2 = 0.
            do l = 1, 3
                d(l) = xyz_el(l, n) - xyz_el(l+3, n)
                xl2 = xl2 + d(l) * d(l)
            end do
            do l = 1, 3
                st(l) = (d(l) / xl2) * e(mtype)
                st(l+3) = -st(l)
            end do
            str = 0.0
            do l = 1, 3
                i = lm(l, n)
                if (i > 0) str = str + st(l) * v(i)
                j = lm(l+3, n)
                if (j > 0) str = str + st(l+3) * v(j)
                
            end do
            p = str * area(mtype)
            write(iout, 2070) n, p, str
        end do
    end select

1000 format(I5, 2F10.0)
1020 format(5I5)
2000 format(' E L E M E N T   D E F I N I T I O N', ///,                  &
     ' ELEMENT TYPE ', 13(' .'), '( NPAR(1) ) .. =', I5, /,               &
     '     EQ.1, TRUSS ELEMENTS', /, '     EQ.2, ELEMENTS CURRENTLY', /,  &
     '     EQ.3, NOT AVAILABLE', //, ' NUMBER OF ELEMENTS.', 10(' .'),    &
     '( NPAR(2) ) .. =', I5, //)
2010 format(' M A T E R I A L   D E F I N I T I O N', ///,                &
     ' NUMBER OF DIFFERENT SETS OF MATERIAL', /,                          &
     ' AND CROSS-SECTIONAL  CONSTANTS ', 4(' .'), '( NPAR(3) ) . . =',    &
     I5, //)
2020 format('  SET       YOUNG''S     CROSS-SECTIONAL', /,                &
     ' NUMBER     MODULUS', 10X, 'AREA', /, 15x, 'E', 14X, 'A')
2030 format(/, I5, 4X, ES12.5, 2X, ES14.6)
2040 format(//, ' E L E M E N T   I N F O R M A T I O N', ///             &
     ' ELEMENT     NODE     NODE       MATERIAL', /,                      &
     ' NUMBER-N      I        J       SET NUMBER',/)
2050 format(I5, 6X, I5, 4X, I5, 7X, I5)
2060 format(///, ' S T R E S S  C A L C U L A T I O N S  F O R  ',        &
     'E L E M E N T  G R O U P', I4, //, '  ELEMENT', 13X, 'FORCE', 12X,  &
     'STRESS', /, '  NUMBER', /)
2070 format(1X, I5, 11X, E13.6, 4X, E13.6)

end subroutine truss