************************************************************************
*      FORTRAN code for a simplified one-line model                    *
*                   --- Coded by 高木　利光，　Yan　Ding               *
*                                   Date: Oct. 20, 1999                *                
*                   --- Modified by 黒岩　正光                         *
*                                   Date: Nov.  1, 2022                *
************************************************************************
      PROGRAM MAIN
      COMMON/VTIME/TIME, DT, TMAX, NTMAX, ITER
      
      CALL INPUT
      CALL INIT
      DO WHILE(TIME.LE.TMAX)
C         write(*,*) 'TIME = ', TIME
         CALL WVDFRM
         CALL CALQ
         CALL QBOUND
         CALL CALY
         CALL YBOUND
         CALL CHANGE
         TIME = TIME + DT
         ITER = ITER + 1
      ENDDO
      CALL OUTPUT
      STOP
      END
************************************************************************
*      Modules for one-line model                                      *
************************************************************************
      SUBROUTINE MODULES
      PARAMETER(NXMAX=501, NGRI=1)
C-----------------------------------------------------------------------
C     NX:  The total node number                                        |
C     NGRI: The total number of groins over shoreline                   |
C     Y(NX): Positions of shoreline (m)                                 |
C     Q(NX): Littoral drift flux (m^3/s)                                |
C     HB(NX): Breaking wave height (m)                                  |
C     ALB(NX): Breaking wave angle (rad)                                |
C     CGB(NX): Breaking wave group velocity(m/s  )                      |
C     H0:  Deep water wave height(m)                                    |
C     ALPHA0: Deep water wave angle (DEG)                               |
C     T: Wave peroid (s)                                                |
C     DRF: Water depth at reference line (m)                            |
C     RK: Longshore transpot calibration parameter                      |
C     DC: Depth of closure (m)                                          |
C     SLOPE: Average bottom slope                                       |
C     PORO: Porosity of sediment                                        |
C     ROWS: Density of sediment                                         |
C     ROW:  Density of sea water                                        |
C     GAMMA: Breaking criterion                                         |
C     IGR(NGRI): Nodal points of groins                                 |
C     DT: Time increment (s)                                            |
C     TMAX: Maximum time of calculation (s)                             |
C     IBCL: Boundary condition index on the left hand                   |
C     IBCR: Boundary condition index on the right hand                  |
C      IF(IBCL or IBCR = 1) Flux is specified                           |
C      IF(IBCL or IBRL = 0) Flux gradient is specified                  |
C-----------------------------------------------------------------------
      COMMON/VAR/X(NXMAX), Y(NXMAX), YOLD(NXMAX), YOUT(NXMAX, 6), 
     $    Q(NXMAX), WDB(NXMAX),HB(NXMAX), HBC(NXMAX),
     &    ALB(NXMAX),ALPHA0(NXMAX),,ALPHA0C(NXMAX),CGB(NXMAX)
      COMMON/IVR/IGR(NGRI), IYG(NXMAX)
      
      COMMON/VTIME/TIME, DT, TMAX, NTMAX, ITER
      COMMON/CNST1/DRF, RKRF, CGRF, GAMMA
      COMMON/CNST2/SIGMA, SIGG
      COMMON/ICONST/NX1, NGR, IBCL, IBCR
      COMMON/CONST/ H0, T, G, RK, DC, FDT, RTD,
     &              QBCL, QBCR, DX, RKAP
      
C-----------------------------------------------------------------------
      ENTRY INPUT
      write(*,100) 
100   format(1x,'******************************************************'
     &    ,//1x,'　 　　  小笹・Brampton漂砂算定式による           　　'
     &     ,/1x,'           海岸線変化の計算　　　　　　　　　　　 　　'
     &     ,/1x,'           (One-line theory)                          '
     &    ,//1x,' プログラム作成者：黒岩　正光   '
     &    ,//1x,'         以下指示に従って入力してください             '
     &   ,//1x,'******************************************************')

C     EBED
      OPEN(1,FILE='data\bpdata.dat')
	read(1,*) NX,DS
	read(1,*) T
	DO I=1,NX
	read(1,*) ii,WDB(i),HBC(i),ALPHA0C(i)
	END DO
	CLOSE(1)
      DO I=2,NX
	HB(I)=(HBC(I)+HBC(I-1))/2.0
	ALPHA0(i)=(ALPHA0C(I)+ALPHA0C(I-1))/2.0
	END DO
	HB(1)=HB(2)
	HB(NX+1)=HB(NX)
	ALPHA0(1)=ALPHA0(2)
	ALPHA0(NX+1)=ALPHA0(NX)

c      OPEN(9, FILE='input.dat', STATUS='OLD')
c      READ(9,*) T, SLOPE, RK1, RK2,DC
c      READ(9,*) DT, TMAX, XL
c      READ(9,*) IBCL, IBCR
c      IF(IBCL.EQ.1) READ(9,*) QBCL
c      IF(IBCR.EQ.1) READ(9,*) QBCR
c      READ(9,*) NGR
c      READ(9,*) (IGR(I), I = 1, NGR)
c      CLOSE(9)

     
      write(*,*) '海底勾配を入力してください（単位：無次元）'
      read(*,*) SLOPE
      
      write(*,*) '漂砂量係数K1を入力してください（単位：無次元）'
      read(*,*) RK1

      write(*,*) '漂砂量係数K2を入力してください（単位：無次元）'
      read(*,*) RK2

      write(*,*) '移動高さを入力してください（単位：m）'
      read(*,*) DC

      write(*,*) '左側の境界を選択してください'
      write(*,*) '[1:固定境界；2：自由境界]'
      read(*,*) IBCL
c      if(IBCL.EQ.1) then
c        write(*,*) '左側の漂砂量を入力してください　（単位：m＾3／s）'
c        read(*,*) QBCL
c      endif
      
      write(*,*) '右側の境界を選択してください'
      write(*,*) '[1:固定境界；2：自由境界]'
      read(*,*) IBCR
c      if(IBCR.EQ.1) then
c        write(*,*) '右側の漂砂量を入力してください　（単位：m＾3／s）'
c        read(*,*) QBCR
c      endif
      
          
      write(*,*) '計算時間を入力してください （単位：月）'
      read(*,*) TMAX
      TMAX = TMAX*30.0*24.0*3600.0
      
      write(*,*) '計算時間間隔を入力してください（単位：s）'
      read(*,*) DT
      
      NTMAX = TMAX / DT
      
      write(*,'(a,i10)') '繰り返し回数＝', NTMAX
      
      write(*,*) '計算中……'
      RETURN
C-----------------------------------------------------------------------
      ENTRY INIT
      NX1 = NX+1
      DX = DS
      PI   = 3.1415926
      G    = 9.806
      DTR  = PI/180.0
      RTD  = 180.0/PI
      ROWS = 2.65
      ROW  = 1.03
      PORO = 0.4
      RKAP1  = RK1/(16.0*(ROWS/ROW - 1.0)*(1.0-PORO)*2.386)
      RKAP2  = RK2/(16.0*(ROWS/ROW - 1.0)*(1.0-PORO)*2.386)
C      ALPHA0 = ALPHA0 * DTR
      FDT = DT / (DC*DX)
      TIME = 0.0
      ITER = 0
      GAMMA = 0.78
      SIGMA = 2.0*PI / T
      SIGG = SIGMA * SIGMA /G
      DO I=1,NX1
      ALPHA0(i) = ALPHA0(i) * DTR
      END DO 
      DO I = 1, NX
        Y(I) = 0.0
        YOUT(I,1) = 0.0
        YOLD(I) = 0.0
        Q(I) = 0.0
C        HB(I) = 0.0
C        ALB(I) = 0.0
        CGB(I) = 0.0
        IYG(I) = 0
      ENDDO
      
      X(1) = 0.0
      DO I = 2, NX1
         X(I) = X(I-1) + DX
      ENDDO
      
      RETURN
      
C-----------------------------------------------------------------------
      ENTRY WVDFRM
      DO I= 2, NX
          DXTEMP = DX
C          IF(I.EQ.2.OR.I.EQ.NX) DXTEMP = 0.5*DX
          ATSNL  = ATAN( (Y(I)-Y(I-1))/DXTEMP )
          ZGI = ALPHA0(i) - ATSNL 
          ALB(I) = ZGI
          CGB(I) = SQRT(G*WDB(I))
C        ENDIF
      ENDDO
      RETURN
C-----------------------------------------------------------------------
      ENTRY CALQ
      DO I = 2, NX
C        IF(IYG(I).EQ.0) THEN
          PWR=HB(I)*HB(I)*CGB(I)
       Q(I)=PWR*RKAP1*SIN(2*ALB(I))
     $     -PWR*RKAP2/SLOPE*(HBC(I)-HBC(I-1))/DX*COS(ALB(i))
C        ENDIF
      ENDDO
      RETURN
C-----------------------------------------------------------------------
C      ENTRY QGROIN
C      IF(NGR.GE.1) THEN
C        DO IG = 1, NGR
C           I = IGR(IG)
C           Q(I) = 0.0
C        ENDDO
C      ENDIF
C      RETURN
C-----------------------------------------------------------------------
      ENTRY CALY
      DO I = 2, NX1
        Y(I) = YOLD(I) - FDT * (Q(I+1) - Q(I))
      ENDDO
      RETURN
C-----------------------------------------------------------------------
      ENTRY QBOUND
      IF(IBCL.EQ.2) Q(2) = 2.0*Q(3) - Q(4)
      IF(IBCR.EQ.2) Q(NX) = 2.0*Q(NX-1) - Q(NX-2)
      RETURN
C-----------------------------------------------------------------------
      ENTRY YBOUND
      IF(IBCL.EQ.1) THEN
        Y(1) = YOLD(1)
      ELSE
        Y(1) = 1.5 * Y(2) - 0.5 * Y(3)
      ENDIF
      
      IF(IBCR.EQ.1) THEN
        Y(NX) = YOLD(NX)
      ELSE
        Y(NX) = 1.5 * Y(NX1) - 0.5 * Y(NX1-1)
      ENDIF
      RETURN
C-----------------------------------------------------------------------
      ENTRY CHANGE
      DO I = 1, NX
        YOLD(I) = Y(I)
      ENDDO
      
      IOUTT = FLOAT(ITER)/FLOAT(NTMAX)*5.0
      
      DO J = 2, 5
        JCHEK = J - 1
        IOUTT = FLOAT(NTMAX*JCHEK)*0.2
        IF(IOUTT.EQ.ITER) THEN
        write(*,*) IOUTT, ITER
          DO I = 1, NX
            YOUT(I, J) = Y(I)
          ENDDO
        ENDIF
      ENDDO
      
      
      RETURN
C-----------------------------------------------------------------------
      ENTRY OUTPUT
      IF(TIME.GE.TMAX) THEN
         DO I = 1, NX
           YOUT(I, 6) = Y(I)
         ENDDO
      ENDIF
      OPEN(10, FILE='outputdata\cstline.dat')
Cding      WRITE(10,'(F15.3)') TIME
      WRITE(10,'(7E13.5)') (X(I), (YOUT(I,J), J=1,6), I= 1, NX)
      CLOSE(10)
      
      DO IG = 1, NGR
        I = IGR(IG)
        HB(I) = 0.5 *(HB(I-1)+HB(I+1))
      ENDDO

      OPEN(10,FILE='outputdata\flux.dat')
         xtemp = -dx
         do i = 2, nx
           xtemp = xtemp + dx
           write(10,'(4E16.7)') xtemp, q(i)*365.0*24.0*3600.0,
     &                          HB(I), alb(i)*RTD
         enddo
      CLOSE(10)
      RETURN
      END
