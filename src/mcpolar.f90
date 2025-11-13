program mcpolar

    !imports
    use constants,                only : resdir, wp, PI
    use gridset_mod,              only : gridset, cart_grid
    use inttau2,                  only : tauint1
    use optical_properties_class, only : optical_properties, init_opt_sphere
    use neutron_class,            only : neutron
    use random_mod,               only : ran2, init_seed
    use sourceph_mod,             only : isotropic_point_src
    use utils,                    only : set_directories, str
    use writer_mod,               only : writer
    use linked_list,              only : list_type, list_init, list_insert, list_next, list_get_gen, list_get_neu, list_append 

    implicit none

    !> variable that holds all information about the neutron to be simulated
    type(neutron)     :: packet, packet_gen1
    !> variable that holds the 3D grid information
    type(cart_grid)  :: grid
    !> optical properties variable
    type(optical_properties) :: opt_prop
    !> number of neutrons to run in the simulation
    integer :: nneutrons
    !> counter for number of scatterings for all neutrons
    real(kind=wp) :: nscatt
    !> user defined seed
    integer :: seed
    !> temp variables related to I/O from param file
    integer :: nxg, nyg, nzg
    !> loop variable
    integer :: i, j, cur_gen, prv_gen, max_gen
    !> file handle
    integer :: u
    !> temp variables related to I/O from param file
    real(kind=wp) :: xmax, ymax, zmax, mus, mua, muf, hgg, radius, mass
    !> timing vars
    real(kind=wp) :: start, finish

    !> Create a pointer to the bank head and the current neutron in the bank
    type(list_type), pointer :: bank_head => null()
    type(list_type), pointer :: bank_current => null()

    call cpu_time(start)

    !set directory paths
    call set_directories()
    
    !set random seed
    seed = 42
    call init_seed(seed)
    
    !**** Read in parameters from the file input.params
    open(newunit=u,file=trim(resdir)//'input.params',status='old')
    read(u,*) nneutrons
    read(u,*) xmax
    read(u,*) ymax
    read(u,*) zmax
    read(u,*) nxg
    read(u,*) nyg
    read(u,*) nzg
    read(u,*) mus
    read(u,*) mua
    read(u,*) muf
    read(u,*) hgg
    read(u,*) mass
    read(u,*) max_gen
    close(u)
    
    print*, ''      
    
    radius = ((3._wp * mass)/(4._wp * PI * 19._wp))**(1.0/3.0)
    print*, "Running sim for sphere of radius: ", radius
    !set optical properties
    call init_opt_sphere(mus, mua, muf, hgg, opt_prop)
    ! Set up grid
    call gridset(grid, opt_prop, nxg, nyg, nzg, xmax, ymax, zmax, radius)

    ! inialise the number of scatterings counter
    nscatt = 0_wp

    print*,'Generating First Generation'
    prv_gen = 0
    cur_gen = 1
    ! Release neutron from point source
    call isotropic_point_src(packet_gen1, grid)
    call list_init(bank_head, cur_gen, packet_gen1)
    print*,'List Init'
    do i = 2, 10
        call isotropic_point_src(packet_gen1, grid)
        call list_append(bank_head, cur_gen, packet_gen1)
    end do

    bank_current => bank_head
    print*,'neutrons now running'
    !loop over neutrons 
    do while (associated(bank_current))
        cur_gen = list_get_gen(bank_current) 
        !display progress
        if(cur_gen >= max_gen) then
            exit
        end if 

        if(cur_gen /= prv_gen)then
            print *, 'Simulating Generation: ', cur_gen
            prv_gen = cur_gen
        end if
        packet = list_get_neu(bank_current)
        ! Find scattering location
        call tauint1(packet, grid)
        ! neutron scatters in grid until it exits (tflag=TRUE) 
        do while(.not. packet%tflag)
            !interact with medium
            if(ran2() < opt_prop%albedo)then
                ! neutron is scattered
                call packet%scatter(opt_prop)
                nscatt = nscatt + 1._wp    
            else
                if (ran2() < (muf/(mua+muf))) then
                    ! neutron has caused a fission
                    if (ran2() < 0.56) then
                        do j = 1, 2
                            call isotropic_point_src(packet_gen1, grid)
                            call list_append(bank_head, cur_gen+1, packet_gen1)
                        end do
                    else   
                        do j = 1, 3
                            call isotropic_point_src(packet_gen1, grid)
                            call list_append(bank_head, cur_gen+1, packet_gen1)
                        end do
                    end if
                end if
                ! neutron is absorbed
                packet%tflag=.true.
                exit
            end if

            ! Find next scattering location
            call tauint1(packet, grid)

        end do
        bank_current => list_next(bank_current)
    end do      ! end loop over nph neutrons
    ! print*,'Average # of scatters per neutron: '//str(nscatt/(nneutrons))
    !write out files
    call writer(grid, bank_head, radius, max_gen)
    print*,'write done'

    call cpu_time(finish)

    if(finish-start >= 60._wp)then
        print*,floor((finish-start)/60._wp)+mod(finish-start,60._wp)/100._wp
    else
        print*, 'time taken ~'//str(floor(finish-start/60._wp))//'s'
    end if
    !
end program mcpolar
