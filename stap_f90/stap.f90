program stap
    use common_mod
    implicit none
    
    integer, parameter     :: iwp = selected_real_kind(15)  ! 双精度
    
    ! *********************************************************************
    ! 求解相关
    ! *********************************************************************
    integer                :: numnp        ! 节点总数
    integer                :: numeg        ! 单元组数
    integer                :: ng           ! 单元组号
    integer                :: nlcase       ! 工况组数
    integer                :: caseid       ! 工况号
    integer                :: nload        ! 当前工况的载荷数
    integer                :: neq          ! 方程总数
    integer                :: neq1         ! 方程总数+1
    integer                :: mk           ! 半带宽
    integer                :: nwk          ! 一维数组中存储的刚度矩阵元素数量
    real(iwp), allocatable :: a(:)         ! 整体刚度矩阵
    integer,   allocatable :: mht(:)       ! 列高（不含对角元素）
    integer,   allocatable :: maxa(:)      ! 对角元素定位数组
    real(iwp), allocatable :: r(:, :)      ! 载荷
    
    ! *********************************************************************
    ! 节点和单元
    ! *********************************************************************
    integer                :: npar(10)     ! npar(1)：单元类型，1为桁架
                                           ! npar(2)：当前单元组的单元数
                                           ! npar(3)：当前单元组的材料数
    integer,   allocatable :: id(:, :)     ! 节点自由度数组
    real(iwp), allocatable :: xyz(:, :)    ! 节点坐标
    real(iwp), allocatable :: xyz_el(:, :) ! 单元节点坐标
    integer,   allocatable :: lm(:, :)     ! 单元自由度编号
    real(iwp), allocatable :: matp(:)      ! 单元的材料属性号
    real(iwp), allocatable :: e(:)         ! 弹性模量
    real(iwp), allocatable :: area(:)      ! 截面积
    
    ! *********************************************************************
    ! 标识符
    ! *********************************************************************
    ! modex：求解模式   0：检查；1：计算
    integer                :: modex        
    ! ind：用于确定执行的单元操作  1：读取单元；2装配刚度矩阵；3：计算应力
    integer                :: ind
    
    ! *********************************************************************
    ! 其它信息
    ! *********************************************************************
    character(len=80)      :: hed  ! 标签信息，对所分析问题的简单描述
    
    real(iwp)              :: tim(5)
    
    ! *********************************************************************
    ! 临时变量
    ! *********************************************************************
    integer                :: i, j, ll, mm, ktr
    real(iwp)              :: tt
    
    ! 输入输出设备号
    iin = 50
    iout = 60
    
    ! 输入输出文件名
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
        ! 输入阶段
        ! *****************************************************************
        ! *****************************************************************
        call second(tim(1))
        
        ! *****************************************************************
        ! 读取控制信息
        ! *****************************************************************
        read(iin, 1000) hed, numnp, numeg, nlcase, modex
        if (numnp == 0) stop
        write(iout, 2000) hed, numnp, numeg, nlcase, modex
        
        ! *****************************************************************
        ! 读取节点信息
        ! *****************************************************************
        ! 一个输入文件中可以有多个算例，开始读取数据前需要释放并重新分配内存
        ! 自由度和坐标数组的每一个元素都会赋值，不用初始化，只需要分配内存
        if (allocated(id)) deallocate(id)
        if (allocated(xyz)) deallocate(xyz)
        allocate(id(3, numnp), xyz(3, numnp))
        call node_input(id, xyz, numnp, neq)
        
        neq1 = neq + 1
        
        ! *****************************************************************
        ! 计算载荷向量
        ! *****************************************************************
        ! 为载荷数组分配内存空间，并初始化
        if (allocated(r)) deallocate(r)
        allocate(r(neq, nlcase))
        do i = 1, neq
            do j = 1, nlcase
                r(i, j) = 0.0
            end do
        end do
        ! 读取并计算载荷向量
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
        ! 读取和生成单元数据
        ! *****************************************************************
        ! 列高初始化
        if (allocated(mht)) deallocate(mht)
        allocate(mht(neq))
        do i = 1, neq
            mht(i) = 0
        end do
        ! ind为1时truss生成单元数据
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
        ! 求解阶段
        ! *****************************************************************
        ! *****************************************************************
        
        ! *****************************************************************
        ! 组装刚度矩阵
        ! *****************************************************************
        ! 定位向量初始化
        if (allocated(maxa)) deallocate(maxa)
        allocate(maxa(neq+1))
        do i = 1, neq+1
            maxa(i) = 0
        end do
        ! 获取列高、半带宽等信息并输出
        call addres(mht, maxa, mk, nwk)
        mm = nwk / neq
        write(iout, 2025) neq, nwk, mk, mm

        if (modex == 0) then
            call second(tim(3))
            call second(tim(4))
            call second(tim(5))
        else
            ! *************************************************************
            ! 组装刚度矩阵
            ! *************************************************************
            ind = 2
            ! 为刚度矩阵分配内存空间，并初始化为0
            if (allocated(a)) deallocate(a)
            allocate(a(nwk))
            do i = 1, nwk
                a(i) = 0.0
            end do
            ! 计算刚度矩阵
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
            ! LDLT分解
            ! *************************************************************
            ktr = 1
            call colsol(a, r(:, 1), maxa, neq, ktr)

            call second(tim(4))
            
            ! *************************************************************
            ! 回代得到位移和应力
            ! *************************************************************
            ktr = 2
            ind = 3
            do caseid = 1, nlcase
                ! *********************************************************
                ! 计算位移
                ! *********************************************************
                call colsol(a, r(:, caseid), maxa, neq, ktr)
                
                write(iout, 2015) caseid
                call writed(r(:, caseid), numnp, id)
                
                ! *********************************************************
                ! 计算应力
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
        ! 打印求解信息
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