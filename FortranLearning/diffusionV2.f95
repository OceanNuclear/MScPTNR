program diffusionV2
implicit none
												interface

													subroutine Laplacify(Lap,l)
													real, dimension (:,:) :: Lap
													integer :: l
													end subroutine Laplacify

													subroutine LaplaceOperationZIP(Lap,phi,dphi)
													real, dimension(:,:) :: Lap
													real, dimension(:) :: phi
													real, dimension(:) :: dphi
													end subroutine LaplaceOperationZIP

												end interface

	!	n is grid dimension

	integer, parameter :: n = 100
	integer,parameter :: m = 5
	integer,parameter :: T = 100
	integer :: i,j
	real :: lambda = -0.02

	real,dimension(n*n,m) :: lap

	real,dimension(n*n) :: phi
	real,dimension(n*n) :: dphi
	real,dimension(n*n, T) :: phiHistory

	!	instantiate Laplacian

	!real,dimension (:,:), allocatable :: lap

	!allocate (lap(n*n,5))

	call Laplacify(lap,n)

	! instantiate phi

	do i = 1, n*n

		phi(i) = 0

	end do

	phi(5050) = 10

	do i = 1,n*n

        phiHistory(i,1) = phi(i)

    end do

	! start time loop

	do i = 2, T

		call LaplaceOperationZIP(lap,phi,dphi)

		phi = phi + lambda * dphi

		do j = 1,n*n

        	phiHistory(j,i) = phi(j)

    	end do

	end do

!	do i = 1,T

!		do j = 1,n*n

!			print*, phiHistory(j,i)

!		end do

!	print *, '-------'

!	end do 

	!export and save Histories

	print *, 'finishied computation printing data to file diffusionData.dat'

       	open(10, file='diffusionData.dat', access = 'stream')

        write(10) phiHistory

        close(10)

end program diffusionV2 

subroutine LaplaceOperationZIP(Lap, phi, dphi)

	real, dimension(:,:) :: Lap
	real, dimension(:) :: phi
	real, dimension(:) :: dphi

	integer :: i, j

	integer :: n , m

	n = SIZE(Lap,1)
	m = SIZE(Lap,2)

	do i = 1,n

		dphi(i) = phi(i) * Lap(i,1)

	end do

	do i = 2,m

		do j = 1,n

			if (Lap(j,i) > 0) then

			dphi(j) = dphi(j) - phi(INT(Lap(j,i)))

			end if

		end do

	end do

end subroutine LaplaceOperationZIP

Subroutine Laplacify (Lap,l)
	
	!	n is now total number of grid points, m contians connectivity info, l is the grid dimension 

	real, dimension (:,:) :: Lap
	integer :: i,j
	integer :: n,m
	integer :: l
	real :: degree = 4

	n = SIZE(Lap,1)
	m = SIZE(Lap,2)

	do i = 1,m

		do j = 1,n
			
			lap(j,i) = -1

		end do

	end do

	! add the degrees column first:

	do j = 1,n

		lap(j,1) = 4

		if (j < l + 1) then
       		lap(j,1) = lap(j,1) - 1
       	end if

        if (modulo(j,l) == 0) then
       		lap(j,1) = lap(j,1) - 1
       	end if

        if (modulo(j,l)-1 == 0) then
       		lap(j,1) = lap(j,1) - 1
       	end if

        if (j > n - l) then 
       		lap(j,1) = lap(j,1) - 1
       	end if

    end do

    ! now lets add the connectivity indexes

    do j = 1,n 

    	if (j + l < n + 1) then 
        		lap(j,2) = j+l
        	end if

        	if (j - l > -1) then
        		lap(j,3) = j-l
        	end if

	       	if (modulo(j,l) /= 0) then 
        		lap(j,4) = j+1
        	end if

        	if (modulo((j - 1),l) /= 0) then 
        		lap(j,5) = j-1
        	end if

        end do

!        do i = 1,m

!        	do j = 1,n

!        		print*, lap(j,i)

!        	end do

!        	print*, '------'

!        end do

end subroutine Laplacify