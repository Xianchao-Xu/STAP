subroutine second(time)
    ! ��ȡʱ����Ϣ
    implicit none
    
    integer, parameter     :: iwp = selected_real_kind(15)
    real(iwp), intent(out) :: time
    
    time = 0.01 * mclock()

end subroutine second