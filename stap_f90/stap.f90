program stap
    use common_mod
    implicit none
    
    integer, parameter     :: iwp = selected_real_kind(15)  ! ˫����
    
    ! *********************************************************************
    ! ������
    ! *********************************************************************
    integer                :: numnp        ! �ڵ�����
    integer                :: numeg        ! ��Ԫ����
    integer                :: ng           ! ��Ԫ���
    integer                :: nlcase       ! ��������
    integer                :: caseid       ! ������
    integer                :: nload        ! ��ǰ�������غ���
    integer                :: neq          ! ��������
    integer                :: neq1         ! ��������+1
    integer                :: mk           ! �����
    integer                :: nwk          ! һά�����д洢�ĸնȾ���Ԫ������
    real(iwp), allocatable :: a(:)         ! ����նȾ���
    integer,   allocatable :: mht(:)       ! �иߣ������Խ�Ԫ�أ�
    integer,   allocatable :: maxa(:)      ! �Խ�Ԫ�ض�λ����
    real(iwp), allocatable :: r(:, :)      ! �غ�
    
    ! *********************************************************************
    ! �ڵ�͵�Ԫ
    ! *********************************************************************
    integer                :: npar(10)     ! npar(1)����Ԫ���ͣ�1Ϊ���
                                           ! npar(2)����ǰ��Ԫ��ĵ�Ԫ��
                                           ! npar(3)����ǰ��Ԫ��Ĳ�����
    integer,   allocatable :: id(:, :)     ! �ڵ����ɶ�����
    real(iwp), allocatable :: xyz(:, :)    ! �ڵ�����
    real(iwp), allocatable :: xyz_el(:, :) ! ��Ԫ�ڵ�����
    integer,   allocatable :: lm(:, :)     ! ��Ԫ���ɶȱ��
    real(iwp), allocatable :: matp(:)      ! ��Ԫ�Ĳ������Ժ�
    real(iwp), allocatable :: e(:)         ! ����ģ��
    real(iwp), allocatable :: area(:)      ! �����
    
    ! *********************************************************************
    ! ��ʶ��
    ! *********************************************************************
    ! modex�����ģʽ   0����飻1������
    integer                :: modex        
    ! ind������ȷ��ִ�еĵ�Ԫ����  1����ȡ��Ԫ��2װ��նȾ���3������Ӧ��
    integer                :: ind
    
    ! *********************************************************************
    ! ������Ϣ
    ! *********************************************************************
    character(len=80)      :: hed  ! ��ǩ��Ϣ��������������ļ�����
    
    real(iwp)              :: tim(5)
    
    ! *********************************************************************
    ! ��ʱ����
    ! *********************************************************************
    integer                :: i, j, ll, mm, ktr
    real(iwp)              :: tt
    
    ! ��������豸��
    iin = 50
    iout = 60
    
    ! ��������ļ���
    input_file = '..\..\TEST.INP'
    output_file = '..\..\TEST.OUT'
    
    open(unit=iin, file=input_file, status='old', action='read',          &
        iostat=istat, iomsg=imsg)
    if (istat /= 0) then
        write(*, *) trim(imsg)
        stop
    end if
    open(unit=iout, file=output_file, action='write')
    
    do
        ! *****************************************************************
        ! *****************************************************************
        ! ����׶�
        ! *****************************************************************
        ! *****************************************************************
        call second(tim(1))
        
        ! *****************************************************************
        ! ��ȡ������Ϣ
        ! *****************************************************************
        read(iin, 1000) hed, numnp, numeg, nlcase, modex
        if (numnp == 0) stop
        write(iout, 2000) hed, numnp, numeg, nlcase, modex
        
        ! *****************************************************************
        ! ��ȡ�ڵ���Ϣ
        ! *****************************************************************
        ! һ�������ļ��п����ж����������ʼ��ȡ����ǰ��Ҫ�ͷŲ����·����ڴ�
        ! ���ɶȺ����������ÿһ��Ԫ�ض��ḳֵ�����ó�ʼ����ֻ��Ҫ�����ڴ�
        if (allocated(id)) deallocate(id)
        if (allocated(xyz)) deallocate(xyz)
        allocate(id(3, numnp), xyz(3, numnp))
        call node_input(id, xyz, numnp, neq)
        
        neq1 = neq + 1
        
        ! *****************************************************************
        ! �����غ�����
        ! *****************************************************************
        ! Ϊ�غ���������ڴ�ռ䣬����ʼ��
        if (allocated(r)) deallocate(r)
        allocate(r(neq, nlcase))
        do i = 1, neq
            do j = 1, nlcase
                r(i, j) = 0.0
            end do
        end do
        ! ��ȡ�������غ�����
        write(iout, 2005)
        do caseid = 1, nlcase
            read(iin, 1010) ll, nload
            write(iout, 2010) ll, nload
            if (ll /= caseid) then
                write(iout, 2020)
                stop
            else
                call loads(r, caseid, nload, id, modex)
            end if
        end do
        
        ! *****************************************************************
        ! ��ȡ�����ɵ�Ԫ����
        ! *****************************************************************
        ! �и߳�ʼ��
        if (allocated(mht)) deallocate(mht)
        allocate(mht(neq))
        do i = 1, neq
            mht(i) = 0
        end do
        ! indΪ1ʱtruss���ɵ�Ԫ����
        ind = 1
        write(iout, 4000)
        do ng = 1, numeg
            if (ng /= 1) write(iout, 4010)
            read(iin, 3000) npar
            select case (npar(1))
            case(1)
                call truss(npar, ind, id, maxa, xyz, xyz_el, lm, matp, e, &
                    area, mht, a)
            case(2)
                stop 'No such element type'
            end select
        end do
        
        call second(tim(2))
        ! *****************************************************************
        ! *****************************************************************
        ! ���׶�
        ! *****************************************************************
        ! *****************************************************************
        
        ! *****************************************************************
        ! ��װ�նȾ���
        ! *****************************************************************
        ! ��λ������ʼ��
        if (allocated(maxa)) deallocate(maxa)
        allocate(maxa(neq+1))
        do i = 1, neq+1
            maxa(i) = 0
        end do
        ! ��ȡ�иߡ���������Ϣ�����
        call addres(mht, maxa, mk, nwk)
        mm = nwk / neq
        write(iout, 2025) neq, nwk, mk, mm

        if (modex == 0) then
            call second(tim(3))
            call second(tim(4))
            call second(tim(5))
        else
            ! *************************************************************
            ! ��װ�նȾ���
            ! *************************************************************
            ind = 2
            ! Ϊ�նȾ�������ڴ�ռ䣬����ʼ��Ϊ0
            if (allocated(a)) deallocate(a)
            allocate(a(nwk))
            do i = 1, nwk
                a(i) = 0.0
            end do
            ! ����նȾ���
            do i = 1, numeg
                select case (npar(1))
                case(1)
                    call truss(npar, ind, id, maxa, xyz, xyz_el, lm, matp,&
                        e, area, mht, a)
                case(2)
                    stop 'No such element type'
                end select
            end do
            
            call second(tim(3))
            ! *************************************************************
            ! LDLT�ֽ�
            ! *************************************************************
            ktr = 1
            call colsol(a, r(:, 1), maxa, neq, ktr)

            call second(tim(4))
            
            ! *************************************************************
            ! �ش��õ�λ�ƺ�Ӧ��
            ! *************************************************************
            ktr = 2
            ind = 3
            do caseid = 1, nlcase
                ! *********************************************************
                ! ����λ��
                ! *********************************************************
                call colsol(a, r(:, caseid), maxa, neq, ktr)
                
                write(iout, 2015) caseid
                call writed(r(:, caseid), numnp, id)
                
                ! *********************************************************
                ! ����Ӧ��
                ! *********************************************************
                do ng = 1, numeg
                    select case (npar(1))
                    case(1)
                        call truss(npar, ind, id, maxa, xyz, xyz_el, lm,  &
                            matp, e, area, mht, a, r(:, caseid), ng)
                    case(2)
                        stop 'No such element type'
                    end select
                end do
            end do
        end if
        ! *****************************************************************
        ! ��ӡ�����Ϣ
        ! *****************************************************************
        tt = 0.
        do i = 1, 4
            tim(i) = tim(i+1) - tim(i)
            tt = tt + tim(i)
        end do
        write(iout, 2030) hed, (tim(i), i = 1, 4), tt
    end do

1000 format(A80, /, 4I5)
1010 format(2I5)
2000 format(///,' ',A80,///,' C O N T R O L   I N F O R M A T I O N',//,  &
     '      NUMBER OF NODAL POINTS', 10(' .'), ' (NUMNP)  = ', I5, //,    &
     '      NUMBER OF ELEMENT GROUPS', 9(' .'), ' (NUMEG)  = ', I5, //,   &
     '      NUMBER OF LOAD CASES',  11(' .'), ' (NLCASE) = ', I5, //,     &
     '      SOLUTION MODE ', 14(' .'), ' (MODEX)  = ', I5, /,             &
     '         EQ.0, DATA CHECK', /, '         EQ.1, EXECUTION')
2005 format(//,' L O A D   C A S E   D A T A')
2010 format(////,'     LOAD CASE NUMBER', 7(' .'), ' = ',I5, //,          &
     '     NUMBER OF CONCENTRATED LOADS . = ', I5)
2015 format(//, ' LOAD CASE ', I3)
2020 format(' *** ERROR *** LOAD CASES ARE NOT IN ORDER')
2025 format(//, ' TOTAL SYSTEM DATA', ///, '     NUMBER OF EQUATIONS',    &
     14(' .'), '(NEQ) = ', I5, //, '     NUMBER OF MATRIX ELEMENTS',      &
     11(' .'), '(NWK) = ', I5, //, '     MAXIMUM HALF BANDWIDTH ',        &
     12(' .'), '(MK ) = ', I5, //, '     MEAN HALF BANDWIDTH',            &
     14(' .'), '(MM ) = ', I5)
2030 format(//, ' S O L U T I O N   T I M E   L O G   I N   S E C', //,   &
     '            FOR PROBLEM', //, ' ', A80, ///,                        &
     '     TIME FOR INPUT PHASE ', 14(' .'), ' =', F12.2, //,             &
     '     TIME FOR CALCULATION OF STIFFNESS MATRIX  . . . . =', F12.2,//,&
     '     TIME FOR FACTORIZATION OF STIFFNESS MATRIX  . . . =', F12.2,//,&
     '     TIME FOR LOAD CASE SOLUTIONS ', 10(' .'), ' =', F12.2, ///,    &
     '      T O T A L   S O L U T I O N   T I M E  . . . . . =', F12.2)
3000 format(10I5)
4000 format(//, ' E L E M E N T   G R O U P   D A T A', //)
4010 format(' ')

end program stap