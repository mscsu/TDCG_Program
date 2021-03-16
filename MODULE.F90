!======================================================================
!
!======================================================================
    module ModPrecision
        implicit none
        integer,parameter::R4=4
        integer,parameter::R8=8
        integer,parameter::I1=1
        integer,parameter::I2=2
        integer,parameter::I4=4
    end module ModPrecision
!======================================================================
    module ModTypDef
        use ModPrecision
        implicit none
        type typPoint
            real(R8):: x,y,z
        end type typPoint

        type typCell
            
        end type typCell
    end module ModTypDef
!======================================================================
    module ModInpGlobal
        use ModPrecision
        implicit none
        logical :: Aniso
        integer :: nStep
        integer :: nSave
        integer :: nAdaptStep
        logical :: Debug
        Real(R8):: CFL
        logical :: NRR
        logical :: Chimera
        integer :: Limiter
        logical :: Restart
        character(50):: NameStr
    end module ModInpGlobal
!======================================================================
    module ModInpMesh
        use ModPrecision
        implicit none
        integer :: nCellx, nCelly, nCellz
        real(R8):: DomainX, DomainY, DomainZ
        integer :: InitRefineLVL
        integer :: AdaptRefineLVL
    end module ModInpMesh
!======================================================================
    module ModInpInflow
        use ModPrecision
        implicit none
        real(R8):: Alpha
        real(R8):: Beta
        real(R8):: ReyNum
        real(R8):: T00
        real(R8):: Mach00
        real(R8):: Gama00
    end module ModInpInflow
!======================================================================
    module ModInpNRRset
        use ModPrecision
        implicit none
        integer :: NRRSeed
        real(R8):: NRRLength
        real(R8):: NRRTheta
    end module ModInpNRRset
!======================================================================