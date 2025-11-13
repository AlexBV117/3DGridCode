module writer_mod
    !! I/O related routines.

implicit none

    contains
        subroutine writer(grid, bank, radius, max_gen)
        !! subroutine to write out normalised fluence

            use constants,   only : fileplace, wp
            use gridset_mod, only : cart_grid
            use neutron_class,only: neutron
            use linked_list, only : list_type, list_get_gen, list_get_neu, list_next

            !> grid
            type(cart_grid), intent(in) :: grid
            !> number of neutrons to normalise by
            type(list_type), intent(in), pointer :: bank
            type(list_type), pointer             :: bank_cur 
            
            integer, pointer       :: gen_cur
            type(neutron), pointer :: neu_cur    

            integer :: u, i, max_gen
            integer, dimension(max_gen) :: bank_binned
            ! for identifying the run data    
            real(kind=wp) :: radius
            character(len=10) :: str
            write(str, "(F8.5)") radius
            bank_binned = 0
            allocate(bank_cur)
            allocate(gen_cur)
            allocate(neu_cur)
                
            

            bank_cur => bank

            do while (associated(bank_cur))
                gen_cur => list_get_gen(bank_cur)
                bank_binned(gen_cur) = bank_binned(gen_cur) + 1
                bank_cur => list_next(bank_cur)
            end do
            ! normalise fluence assuming power is 1W

            ! write out fluence
            print*, "Writing Generation Data: (gen.dat) to", fileplace 
            open(newunit=u, file=trim(fileplace)//"gen_data_"//trim(str)//".csv")
            do i = 1, max_gen
                write(u, "(I3, A1, I6)") i, ",", bank_binned(i)
            end do
            close(u)

        
        end subroutine writer
end module writer_mod
