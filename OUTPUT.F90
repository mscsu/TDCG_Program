!======================================================================
    module ModOutput
        use ModTypDef
        implicit none
        type(typPoint),ALLOCATABLE :: Nodes(:)
        integer :: nNodes

    endmodule ModOutput
!======================================================================
    subroutine OutputFlowField(TimeStepStr)
    use ModInpGlobal
    use ModOutput
    use ModInpMesh
    use ModMesh,    only : nCells
    implicit none
    character(*),INTENT(IN):: TimeStepStr
    character(80):: FileName
    integer :: ios, tp  ! Temp Precision
    integer :: i
    real(R8):: minSize

    print*, 'Data outputting......'
    FileName=trim(NameStr)//'-'//TimeStepStr//'.dat'
    call NodeInfo(nCells)
    print"(A7,I10,5X,A5,I10)", "Cells:", nCells, "Nodes:", nNodes

    open(21, file=FileName, iostat=ios, status="replace", action="write")
        if ( ios /= 0 ) stop ("Error====> Error opening file "//FileName)
        write(21,*) 'TITLE="3D Results"'
        write(21,*)'VARIABLES="X","Y","Z","U","V","W","Rou","T","P",',  &
                    '"Ma","NRR"'
        write(21,*)'ZONE N=',nNodes,'E=',nCells,'ZONETYPE=FEbrick'
        write(21,*)'DATAPACKING=BLOCK'
        write(21,*)'VARLOCATION=([1-3]=NODAL,[4-11]=CELLCENTERED)'

        ! Single precision -- croase mesh
        ! Double precision -- fine mesh
        if (InitRefineLVL < 8) then
            write(21,*) real(Node(1:nNodes)%x,R4),                          &
                        real(Node(1:nNodes)%y,R4),                          &
                        real(Node(1:nNodes)%x,R4)
        else
            write(21,*) real(Node(1:nNodes)%x,R8),                          &
                        real(Node(1:nNodes)%y,R8),                          &
                        real(Node(1:nNodes)%x,R8)
        endif
        call TSData  ! TS -- Temporary Storage
    print*, 'Done'
    end subroutine OutputFlowField
!======================================================================
    subroutine NodeInfo(nCellst)    ! nCellst=nCells, as a parameter form.
    use ModMesh
    implicit none
    integer,PARAMETER :: nCellst
    ALLOCATE(Nodes(nCellst*8))  ! Reserve enough space for Nodes-array.
    nNodes=8; nCells=1  ! Initial count.
    Cell(1)%Node(1)=1; Cell(1)%Node(2)=2;
    Cell(1)%Node(3)=3; Cell(1)%Node(4)=4;
    Cell(1)%Node(5)=5; Cell(1)%Node(6)=6;
    Cell(1)%Node(7)=7; Cell(1)%Node(8)=8;

    call InitNodeInfo(Cell(1:nBGCells))
    endsubroutine NodeInfo
!======================================================================
    recursive subroutine InitNodeInfo(c)
    use ModTypDef
    use ModMesh
    implicit none
    type(typCell),pointer :: c

    if(ASSOCIATED(c%son8))then
        call InitNodeInfo(c%son1)
        call InitNodeInfo(c%son2)
        call InitNodeInfo(c%son3)
        call InitNodeInfo(c%son4)
        call InitNodeInfo(c%son5)
        call InitNodeInfo(c%son1)
        call InitNodeInfo(c%son1)
        call InitNodeInfo(c%son1)
    elseif(ASSOCIATED(c%son4))then
        call InitNodeInfo(c%son1)
        call InitNodeInfo(c%son1)
        call InitNodeInfo(c%son1)
        call InitNodeInfo(c%son1)
    elseif(ASSOCIATED(c%son2))then
        call InitNodeInfo(c%son1)
        call InitNodeInfo(c%son1)
    else

    endif
    endsubroutine InitNodeInfo
!======================================================================
!======================================================================
    subroutine TSData
    implicit none
    do i = 1, nBGCells
    enddo
    endsubroutine TSData
!======================================================================
!======================================================================
!======================================================================