program matplotlib_example
  use forpy_mod
  implicit none

  integer :: ierror, ii
  real, parameter :: PI = 3.1415927
  integer, parameter :: NPOINTS = 200
  real :: x(NPOINTS)
  real :: y(NPOINTS)

  do ii = 1, NPOINTS
    x(ii) = ((ii-1) * 2. * PI)/(NPOINTS-1)
    y(ii) = sin(x(ii))
  enddo

  ierror = forpy_initialize()
  ! forpy_initialize returns NO_NUMPY_ERROR if numpy could not be imported
  ! You could still use forpy without the array features, but here we need them.
  if (ierror == NO_NUMPY_ERROR) then
    write(*,*) "This example needs numpy..."
    stop
  endif


  call simple_plot(x, y)

  call forpy_finalize

  CONTAINS

  subroutine simple_plot(x, y)
    real, asynchronous, intent(in) :: x(:)
    real, asynchronous, intent(in) :: y(:)

    integer :: ierror
    type(module_py) :: plt
    type(tuple) :: args
    type(ndarray) :: x_arr, y_arr

    ierror = import_py(plt, "matplotlib.pyplot")

    ! You can also test for certain exceptions
    if (ierror /= 0) then
      if (exception_matches(ImportError)) then
        write(*,*) "This example needs matplotlib..."
        stop
      else
        call err_print
        stop
      endif
    endif

    ierror = ndarray_create_nocopy(x_arr, x)

    ierror = ndarray_create_nocopy(y_arr, y)

    ierror = tuple_create(args, 2)

    ierror = args%setitem(0, x_arr)
    ierror = args%setitem(1, y_arr)

    ierror = call_py_noret(plt, "plot", args)
    ierror = call_py_noret(plt, "show")

    call x_arr%destroy
    call y_arr%destroy
    call args%destroy
    call plt%destroy
  end subroutine

end program
