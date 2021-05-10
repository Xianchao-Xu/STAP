subroutine node_input(id, xyz, numnp, neq)
    ! 读取、生成并输出节点信息
    ! 计算方程数，并将节点信息存入数组
    use common_mod
    implicit none
    integer, parameter       :: iwp = selected_real_kind(15)
    integer,   intent(  out) :: id(:, :)
    real(iwp), intent(  out) :: xyz(:, :)
    integer,   intent(in   ) :: numnp
    integer,   intent(  out) :: neq
    
    integer                  :: kn
    integer                  :: n       ! 节点号
    integer                  :: kn_old  ! 上一行的kn值
    integer                  :: n_old   ! 上一行的n值
    integer                  :: ndof    ! 节点自由度数
    integer                  :: ndim    ! 维数
    integer                  :: num     ! 两节点间的单元数
    integer                  :: num_n   ! 两节点间的节点数（不包含自身）
    
    real(iwp), allocatable   :: dxyz(:) ! 坐标间隔
    
    integer                  :: i, j, k, kk
    
    ! *********************************************************************
    ! 读取和生成节点数据
    ! *********************************************************************
    write(iout, 2000)
    write(iout, 2010)
    write(iout, 2020)
    
    ! kn不为0时，需要2行数据，以便获取差量，自动生成节点坐标和约束
    ! kn为0时，每一行的即为需要的节点信息
    kn_old = 0
    n_old = 0
    ndof = ubound(id, 1)
    ndim = ubound(xyz, 1)
    allocate(dxyz(ndim))
    do
        read(iin, 1000) n,(id(i,n),i=1,ndof),(xyz(i,n),i=1,ndim),kn
        write(iout, 2030) n,(id(i,n),i=1,ndof),(xyz(i,n),i=1,ndim),kn
        if (n == numnp) exit
        ! kn_old等于0，说明所有节点数据直接给出，不用自动生成
        if (kn_old == 0) then
            n_old = n
            kn_old = kn
        else
            num = (n - n_old) / kn_old
            num_n = num - 1
            ! num_n小于1时，说明没有需要自动生成的节点
            if (num_n < 1) then
                n_old = n
                kn_old = n
            end if
            dxyz = (xyz(:, n) - xyz(:, n_old)) / num
            k = n_old
            do j = 1, num_n
                kk = k  ! 上一个节点编号
                k = k + kn_old  ! 当前节点编号
                xyz(:, k) = xyz(:, kk) + dxyz(:)
                do i = 1, ndof
                    id(i, k) = id(i, kk)
                end do
            end do
        end if
    end do
    
    ! *********************************************************************
    ! 输出全部节点信息
    ! *********************************************************************
    write(iout, 2015)
    write(iout, 2020)
    do n = 1, numnp
        write(iout, 2030) n,(id(i,n),i=1,ndof),(xyz(i,n),i=1,ndim),kn
    end do
    
    ! *********************************************************************
    ! 更新节点自由度矩阵
    ! *********************************************************************
    neq = 0
    do n = 1, numnp
        do i = 1, ndof
            if (id(i, n) == 0) then
                neq = neq + 1
                id(i, n) = neq
            else
                id(i, n) = 0
            end if
        end do
    end do
    
    ! *********************************************************************
    ! 输出方程总数
    ! *********************************************************************
    write(iout, 2040) (n, (id(i, n), i= 1, ndof), n = 1, numnp)
    
1000 format(4I5, 3F10.0, I5)
2000 format(//, ' NODAL POINT DATA', /)
2010 format(' INPUT NODAL DATA', //)
2015 format(//,' GENERATED NODAL DATA',//)
2020 format('  NODE', 10X, 'BOUNDARY', 25X, 'NODAL POINT', 17X, 'MESH',   &
     /, ' NUMBER     CONDITION  CODES', 21X, 'COORDINATES', 14X,          &
     'GENERATING', /, 77X, 'CODE', /, 15X, 'X    Y    Z', 15X, 'X', 12X,  &
     'Y', 12X, 'Z', 10X, 'KN')
2030 format(I5, 6X, 3I5, 6X, 3F13.3, 3X, I6)
2040 format(//, ' EQUATION NUMBERS', //, '   NODE', 9X,                   &
     'DEGREES OF FREEDOM', /, '  NUMBER', //, '     N', 13X,              &
     'X    Y    Z', /, (1X, I5, 9X, 3I5))
    
end subroutine node_input