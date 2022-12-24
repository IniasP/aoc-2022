subroutine inc_cycle(c, x, sum, screen)
    integer, intent (inout) :: c, x
    integer, intent (inout) :: sum
    logical, intent (inout) :: screen(240)

    if (any([20, 60, 100, 140, 180, 220] == c)) then
        sum = sum + c * x
    end if

    screen(c) = any([x, x + 1, x + 2] == mod(c, 40))

    c = c + 1
end subroutine

subroutine print_screen(screen)
    logical, intent (in) :: screen(240)
    integer :: width, height, i, x, y

    width = 40
    height = size(screen) / width

    do y = 0, height - 1
        do x = 1, width
            i = y * width + x
            if (screen(i)) then
                write(*, fmt="(a)", advance="no") "█"
            else
                write(*, fmt="(a)", advance="no") " "
            end if
        end do
        print *
    end do

end subroutine

program aoc10_2
    implicit none
    integer :: ios
    integer :: x
    integer :: c
    integer :: addx_val
    integer :: sum
    logical :: screen(240)
    character(len=10) :: line

    x = 1
    c = 1
    sum = 0

    open (1, file="input", status="old", iostat=ios)
    if (ios /= 0) stop "error opening input file"

    do
        read(1, "(a)", iostat=ios) line
        if (ios /= 0) exit
        if (line(1:4) == "noop") then
            call inc_cycle(c, x, sum, screen)
        else if (line(1:4) == "addx") then
            read(line(6:), *) addx_val
            call inc_cycle(c, x, sum, screen)
            call inc_cycle(c, x, sum, screen)
            x = x + addx_val
        end if
    end do

    print *, "sum:", sum
    call print_screen(screen)

end program aoc10_2
