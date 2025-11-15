module sourceph_mod
!! Module contains the routines to inialise a neutron, i.e different light sources.
    implicit none

    contains
        subroutine isotropic_point_src(packet, grid, radius, pos)
        !! set intial neutron position at (0.0, 0.0, 0.0) and sample neutron direction in an isotropic manner.

            use constants,    only  : TWOPI, wp
            use gridset_mod,  only  : cart_grid
            use neutron_class, only : neutron
            use random_mod,   only  : ran2, ranu
            use vector_class, only  : vector
            use vector_class

            !> neutron object
            type(neutron),    intent(out) :: packet
            type(vector),     optional    :: pos
            !> grid object
            type(cart_grid), intent(in)  :: grid
            real(kind=wp)                :: radius, x, y, z


            if (present(pos)) then
                packet%pos%z = pos%z
                packet%pos%x = pos%x
                packet%pos%y = pos%y
            else
                x = ranu(-1._wp * radius, radius)
                y = ranu(-1._wp * radius, radius)
                z = ranu(-1._wp * radius, radius)
                do while (((x**2)+(y**2)+(z**2)) > (radius**2))
                    x = ranu(-1._wp * radius, radius)
                    y = ranu(-1._wp * radius, radius)
                    z = ranu(-1._wp * radius, radius)
                end do 
            end if

            ! set packet cosines
            packet%phi  = ran2()*twoPI
            packet%cosp = cos(packet%phi)
            packet%sinp = sin(packet%phi)
            packet%cost = 2._wp*ran2()-1._wp
            packet%sint = sqrt(1._wp - packet%cost**2)

            ! set direction vector
            packet%dir%x = packet%sint * packet%cosp  
            packet%dir%y = packet%sint * packet%sinp
            packet%dir%z = packet%cost
            packet%inv_dir = inverse(packet%dir)

            ! set packet voxel
            packet%xcell=int(grid%nxg*(packet%pos%x+grid%dim%x)/(2._wp*grid%dim%x))+1
            packet%ycell=int(grid%nyg*(packet%pos%y+grid%dim%y)/(2._wp*grid%dim%y))+1
            packet%zcell=int(grid%nzg*(packet%pos%z+grid%dim%z)/(2._wp*grid%dim%z))+1

            packet%tflag = .false.

        end subroutine isotropic_point_src
end module sourceph_mod
