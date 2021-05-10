subroutine node_input(id, xyz, numnp, neq)
    ! ��ȡ�����ɲ�����ڵ���Ϣ
    ! ���㷽�����������ڵ���Ϣ��������
    use common_mod
    implicit none
    integer, parameter       :: iwp = selected_real_kind(15)
    integer,   intent(  out) :: id(:, :)
    real(iwp), intent(  out) :: xyz(:, :)
    integer,   intent(in   ) :: numnp
    integer,   intent(  out) :: neq
    
    integer                  :: kn
    integer                  :: n       ! �ڵ��
    integer                  :: kn_old  ! ��һ�е�knֵ
    integer                  :: n_old   ! ��һ�е�nֵ
    integer                  :: ndof    ! �ڵ����ɶ���
    integer                  :: ndim    ! ά��
    integer                  :: num     ! ���ڵ��ĵ�Ԫ��
    integer                  :: num_n   ! ���ڵ��Ľڵ���������������
    
    real(iwp), allocatable   :: dxyz(:) ! ������
    
    integer                  :: i, j, k, kk
    
    ! *********************************************************************
    ! ��ȡ�����ɽڵ�����
    ! *********************************************************************
    write(iout, 2000)
    write(iout, 2010)
    write(iout, 2020)
    
    ! kn��Ϊ0ʱ����Ҫ2�����ݣ��Ա��ȡ�������Զ����ɽڵ������Լ��
    ! knΪ0ʱ��ÿһ�еļ�Ϊ��Ҫ�Ľڵ���Ϣ
    kn_old = 0
    n_old = 0
    ndof = ubound(id, 1)
    ndim = ubound(xyz, 1)
    allocate(dxyz(ndim))
    do
        read(iin, 1000) n,(id(i,n),i=1,ndof),(xyz(i,n),i=1,ndim),kn
        write(iout, 2030) n,(id(i,n),i=1,ndof),(xyz(i,n),i=1,ndim),kn
        if (n == numnp) exit
        ! kn_old����0��˵�����нڵ�����ֱ�Ӹ����������Զ�����
        if (kn_old == 0) then
            n_old = n
            kn_old = kn
        else
            num = (n - n_old) / kn_old
            num_n = num - 1
            ! num_nС��1ʱ��˵��û����Ҫ�Զ����ɵĽڵ�
            if (num_n < 1) then
                n_old = n
                kn_old = n
            end if
            dxyz = (xyz(:, n) - xyz(:, n_old)) / num
            k = n_old
            do j = 1, num_n
                kk = k  ! ��һ���ڵ���
                k = k + kn_old  ! ��ǰ�ڵ���
                xyz(:, k) = xyz(:, kk) + dxyz(:)
                do i = 1, ndof
                    id(i, k) = id(i, kk)
                end do
            end do
        end if
    end do
    
    ! *********************************************************************
    ! ���ȫ���ڵ���Ϣ
    ! *********************************************************************
    write(iout, 2015)
    write(iout, 2020)
    do n = 1, numnp
        write(iout, 2030) n,(id(i,n),i=1,ndof),(xyz(i,n),i=1,ndim),kn
    end do
    
    ! *********************************************************************
    ! ���½ڵ����ɶȾ���
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
    ! �����������
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