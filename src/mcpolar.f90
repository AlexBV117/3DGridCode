program mcpolar

    !imports
    use constants,                only : resdir, wp
    use gridset_mod,              only : gridset, cart_grid
    use inttau2,                  only : tauint1
    use optical_properties_class, only : optical_properties, init_opt_sphere
    use neutron_class,            only : neutron
    use random_mod,               only : ran2, init_seed
    use sourceph_mod,             only : isotropic_point_src
    use utils,                    only : set_directories, str
    use writer_mod,               only : writer
    use linked_list,              only : list_type, list_init, list_insert, list_next, list_get_gen, list_append 

    implicit none

    !> variable that holds all information about the neutron to be simulated
    type(neutron)     :: packet
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
    integer :: i, j
    !> file handle
    integer :: u
    !> temp variables related to I/O from param file
    real(kind=wp) :: xmax, ymax, zmax, mus, mua, muf, hgg, radius
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
    read(u,*) radius
    close(u)
    
    print*, ''      


    !set optical properties
    call init_opt_sphere(mus, mua, muf, hgg, opt_prop)
    ! Set up grid
    call gridset(grid, opt_prop, nxg, nyg, nzg, xmax, ymax, zmax, radius)

    ! inialise the number of scatterings counter
    nscatt = 0_wp

    print*,'Generating First Generation'
    ! Release neutron from point source
    call isotropic_point_src(packet, grid)
    call list_init(bank_head, 1, packet)
    print*,'List Init'
    do i = 2, 10
        call isotropic_point_src(packet, grid)
        call list_append(bank_head, 1, packet)
    end do

    bank_current => bank_head
    print*,'neutrons now running'
    !loop over neutrons 
    do while (associated(list_next(bank_current)))
        print*, list_get_gen(bank_current) 
        bank_current => list_next(bank_current)
        !display progress
        ! if(mod(j,10000) == 0)then
        !     print *, str(j)//' scattered neutrons completed'
        ! end if

        
        ! Find scattering location
        ! call tauint1(packet, grid)
        ! ! neutron scatters in grid until it exits (tflag=TRUE) 
        ! do while(.not. packet%tflag)
        !
        !     !interact with medium
        !     if(ran2() < opt_prop%albedo)then
        !         ! neutron is scattered
        !         call packet%scatter(opt_prop)
        !         nscatt = nscatt + 1._wp    
        !     else
        !         ! neutron is absorbed
        !         packet%tflag=.true.
        !         exit
        !     end if
        !
        !     ! Find next scattering location
        !     call tauint1(packet, grid)
        !
        ! end do
    end do      ! end loop over nph neutrons
    print*, list_get_gen(bank_current)
    ! print*,'Average # of scatters per neutron: '//str(nscatt/(nneutrons))
    ! !write out files
    ! call writer(grid, nneutrons)
    ! print*,'write done'
    !
    ! call cpu_time(finish)
    !
    ! if(finish-start >= 60._wp)then
    !     print*,floor((finish-start)/60._wp)+mod(finish-start,60._wp)/100._wp
    ! else
    !     print*, 'time taken ~'//str(floor(finish-start/60._wp))//'s'
    ! end if
    !
end program mcpolar
