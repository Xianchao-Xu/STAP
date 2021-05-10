module common_mod
    implicit none
    
    integer                :: iin          ! 输入信息设备号
    integer                :: iout         ! 输出信息设备号
    integer                :: istat        ! 文件状态

    character(len=80)      :: input_file   ! 输入文件名
    character(len=80)      :: output_file  ! 输出文件名
    character(len=80)      :: imsg         ! 文件打开信息
    
interface

    subroutine addban(a, s, lm_n, maxa, nd)
        implicit none
        integer, parameter       :: iwp = selected_real_kind(15)
        real(iwp), intent(inout) :: a(:)
        real(iwp), intent(in   ) :: s(:)
        integer,   intent(in   ) :: lm_n(:)
        integer,   intent(in   ) :: maxa(:)
        integer,   intent(in   ) :: nd
    end subroutine

    subroutine addres(mht, maxa, mk, nwk)
        implicit none
        integer,   intent(in   ) :: mht(:)
        integer,   intent(  out) :: maxa(:)
        integer,   intent(  out) :: mk
        integer,   intent(  out) :: nwk
    end subroutine addres

    subroutine colht(mht, nd, lm_n)
        implicit none
        integer,   intent(  out) :: mht(:)
        integer,   intent(in   ) :: nd
        integer,   intent(in   ) :: lm_n(:)
    end subroutine colht
    
    subroutine colsol(a, v, maxa, neq, kkk)
        implicit none
        integer, parameter       :: iwp = selected_real_kind(15)
        real(iwp), intent(inout) :: a(:)
        real(iwp), intent(inout) :: v(:)
        integer,   intent(in   ) :: maxa(:)
        integer,   intent(in   ) :: neq
        integer,   intent(in   ) :: kkk
    end subroutine colsol

    subroutine loads(r, caseid, nload, id, modex)
        implicit none
        integer, parameter       :: iwp = selected_real_kind(15)
        real(iwp), intent(inout) :: r(:, :)
        integer,   intent(in   ) :: caseid
        integer,   intent(in   ) :: nload
        integer,   intent(in   ) :: id(:, :)
        integer,   intent(in   ) :: modex
    end subroutine loads

    subroutine node_input(id, xyz, numnp, neq)
        implicit none
        integer, parameter       :: iwp = selected_real_kind(15)
        integer,   intent(  out) :: id(:, :)
        real(iwp), intent(  out) :: xyz(:, :)
        integer,   intent(in   ) :: numnp
        integer,   intent(  out) :: neq
    end subroutine node_input
    
    subroutine second(time)
        implicit none
        integer, parameter       :: iwp = selected_real_kind(15)
        real(iwp), intent(  out) :: time
    end subroutine second
    
    subroutine truss(npar, ind, id, maxa, xyz, xyz_el, lm, matp, e, area, &
        mht, a, v, ng)
        implicit none
        integer, parameter       :: iwp = selected_real_kind(15)
        integer,   intent(inout) :: npar(:)
        integer,   intent(in   ) :: ind
        integer,   intent(in   ) :: id(:, :)
        integer,   intent(in   ) :: maxa(:)
        real(iwp), intent(in   ) :: xyz(:, :)
        real(iwp), intent(inout),  allocatable :: xyz_el(:, :)
        integer,   intent(inout),  allocatable :: lm(:, :)
        real(iwp), intent(inout),  allocatable :: matp(:)
        real(iwp), intent(inout),  allocatable :: e(:)
        real(iwp), intent(inout),  allocatable :: area(:)
        integer,   intent(  out) :: mht(:)
        real(iwp), intent(inout) :: a(:)
        real(iwp), intent(in   ), optional :: v(:)
        integer,   intent(in   ), optional :: ng
    end subroutine truss
        
    subroutine writed(v, numnp, id)
        implicit none
        integer, parameter       :: iwp = selected_real_kind(15)
        real(iwp), intent(in   ) :: v(:)
        integer,   intent(in   ) :: numnp
        integer,   intent(in   ) :: id(:, :)
    end subroutine writed
    
end interface
end module