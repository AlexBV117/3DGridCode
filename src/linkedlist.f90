! Definition of the linked list object used for neutron banking
! Adapted from a generic linked list object provided by:
! Blevins, J. R. (2009). A Generic Linked List Implementation in Fortran 95. ACM SIGPLAN Fortran Forum 28(3), 2-7.

module linked_list
    use neutron_class, only : neutron
    
    implicit none
    
    private
    
    public :: list_type                !Var
    public :: list_init                !Sub
    public :: list_free                !Sub
    public :: list_insert              !Sub
    public :: list_append              !Sub
    public :: list_get_gen             !Func
    public :: list_get_neu             !Func
    public :: list_next                !Func
   
    ! Define the list node data type
    type :: list_type
        private
        integer, pointer :: gen => null()
        type(neutron), pointer :: neu => null() ! Generic pointer to a type
        type(list_type), pointer :: next => null()
    end type list_type

contains

  ! Initialize a head node SELF and optionally store the provided DATA.
  subroutine list_init(self, gen, neu)
    type(list_type), pointer :: self
    integer,       intent(in), optional, target :: gen
    type(neutron), intent(in), optional, target :: neu
    integer, pointer       :: gen_cpy  
    type(neutron), pointer :: neu_cpy

    allocate(self)
    nullify(self%next)

    ! loop over the provided data array and create the corresponding linked list
    if (present(gen) .and. present(neu)) then
        allocate(gen_cpy)
        allocate(neu_cpy)

        gen_cpy = gen
        neu_cpy = neu
        self%gen => gen_cpy
        self%neu => neu_cpy
    else
       nullify(self%gen)
       nullify(self%neu)
    end if
  end subroutine list_init

  ! Free the entire list and all data, beginning at SELF
  subroutine list_free(self)
    type(list_type), pointer :: self
    type(list_type), pointer :: current
    type(list_type), pointer :: next

    current => self
    do while (associated(current))
       next => current%next
       if (associated(current%gen)) then
          deallocate(current%gen)
          nullify(current%gen)
       end if
       if (associated(current%neu)) then
          deallocate(current%neu)
          nullify(current%neu)
       end if
       deallocate(current)
       nullify(current)
       current => next
    end do
  end subroutine list_free

  ! Return the next node after SELF
  function list_next(self) result(next)
    type(list_type), pointer :: self
    type(list_type), pointer :: next
    next => self%next
  end function list_next

  ! Insert a list node after SELF containing DATA (optional)
  subroutine list_insert(self, gen, neu)
    type(list_type), pointer :: self
    integer,       intent(in), optional, target :: gen    
    type(neutron), intent(in), optional, target :: neu
    type(list_type), pointer :: next
    ! Pointers to copy data into list
    integer, pointer :: gen_cpy
    type(neutron), pointer:: neu_cpy

    allocate(next)

    if (present(gen) .and. present(neu)) then
        allocate(gen_cpy)
        allocate(neu_cpy)

        gen_cpy = gen
        neu_cpy = neu
       next%gen => gen_cpy
       next%neu => neu_cpy
    else
       nullify(next%gen)
       nullify(next%neu)
    end if

    next%next => self%next
    self%next => next
  end subroutine list_insert

  subroutine list_append(self, gen, neu)
    type(list_type), pointer :: self, current
    integer, intent(in) :: gen
    type(neutron), intent(in) :: neu

    current => self
    do while (associated(current%next))
      current => current%next
    end do
    
    call list_insert(current, gen, neu)
  end subroutine list_append

  ! Return the DATA stored in the node SELF
  function list_get_gen(self) result(gen)
    type(list_type), pointer :: self
    integer, pointer :: gen
    gen => self%gen
  end function list_get_gen

  function list_get_neu(self) result(neu)
    type(list_type), pointer :: self
    type(neutron), pointer :: neu
    neu => self%neu
  end function list_get_neu
end module linked_list
