module optical_properties_class
    !! Change optical properties
    !! Set the \(\mu_s\) (scattering coefficient), \(\mu_a\)(absorption coefficient) both in cm\(^{-1}\), and hgg (g factor).

    use constants, only : wp

    implicit none

    !> Stores the optical properties of the medium. Currently only 1 media type. Can expand by making these arrays.
    type :: optical_properties
        !> \(\mu_s\) is the scattering coefficent. in cm\(^{-1}\)   
        real(kind=wp) :: mus
        !> \(\mu_a\) is the absorption coefficent. in cm\(^{-1}\)
        real(kind=wp) :: mua
        !> \(\mu_a\) is the fission coefficent. in cm\(^{-1}\)
        real(kind=wp) :: muf
        !> hgg is the g factor. Describes the bias of the scattering direction. 1 means forward, 0 isotropic and -1 backscattering. unitless   
        real(kind=wp) :: hgg
        !> Is the g factor squared
        real(kind=wp) :: g2
        !> \(\kappa\) is \(\mu_s\) + \(\mu_a\) 
        real(kind=wp) :: kappa
        !> The albedo is \(\frac{\mu_s}{\mu_a+\mu_s}\)
        real(kind=wp) :: albedo
    end type optical_properties

    private
    public :: optical_properties, init_opt_sphere

    contains
    
        subroutine init_opt_sphere(mus, mua, muf, hgg, opt_prop)
        !!  Set tissue optical properties to that of the input parameter file.
            !> optical property container
            type(optical_properties), intent(out) :: opt_prop
            real(kind=wp),            intent(in)  :: mus, mua, muf, hgg

                opt_prop%hgg = hgg
                opt_prop%g2  = opt_prop%hgg**2._wp
                opt_prop%mus = mus
                opt_prop%mua = mua
                opt_prop%muf = muf 

                opt_prop%kappa  = opt_prop%mus + opt_prop%mua + opt_prop%muf
                opt_prop%albedo = opt_prop%mus / opt_prop%kappa
    
        end subroutine init_opt_sphere
end module optical_properties_class
