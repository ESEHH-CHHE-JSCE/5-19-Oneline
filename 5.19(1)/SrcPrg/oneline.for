************************************************************************
*      FORTRAN code for a simplified one-line model                    *
*                               --- Coded by 高木　利光，　Yan　Ding　 *
*                                   Date: Oct. 20, 1999                *
************************************************************************
      PROGRAM MAIN
      COMMON/VTIME/TIME, DT, TMAX, NTMAX, ITER
      
      CALL INPUT
      CALL INIT
      DO WHILE(TIME.LE.TMAX)
C         write(*,*) 'TIME = ', TIME
         CALL WVDFRM
         CALL CALQ
         CALL QGROIN
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
      PARAMETER(NX=101, NGRI=1)
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
C------------------------------------------------------------------------
      COMMON/VAR/X(NX), Y(NX), YOLD(NX), Q(NX), HB(NX), ALB(NX),CGB(NX),
     &                 YOUT(NX, 6)
      COMMON/IVR/IGR(NGRI), IYG(NX)
      
      COMMON/VTIME/TIME, DT, TMAX, NTMAX, ITER
      COMMON/CNST1/DRF, RKRF, CGRF, GAMMA
      COMMON/CNST2/SIGMA, SIGG
      COMMON/ICONST/NX1, NGR, IBCL, IBCR
      COMMON/CONST/ H0, T, ALPHA0, G, RK, DC, FDT, RTD,
     &              QBCL, QBCR, DX, RKAP
      
C-----------------------------------------------------------------------
      ENTRY INPUT
      write(*,100) 
100   format(1x,'******************************************************'
     &    ,//1x,'　 　　  CERC漂砂算定式による　                   　　'
     &     ,/1x,'           海岸線変化の計算　　　　　　　　　　　 　　'
     &     ,/1x,'           (One-line theory)                          '
     &    ,//1x,' プログラム作成者：高木　利光　Yan Ding               '
     &    ,//1x,'         以下指示に従って入力してください             '
     &   ,//1x,'******************************************************')
      write(*,*) '入射波波高を入力してください（単位：m）'
      read(*,*) H0
      
      write(*,*) '入射波周期を入力してください（単位：s）'
      read(*,*) T
      
      write(*,*) '入射波波向を入力してください（単位：度）'
      read(*,*) ALPHA0
      
      write(*,*) '入射波の水深を入力してください（単位：m）'
      read(*,*) DRF
      
      write(*,*) '海底勾配を入力してください（単位：無次元）'
      read(*,*) SLOPE
      
      write(*,*) '漂砂量係数を入力してください（単位：無次元）'
      read(*,*) RK

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
      
      write(*,*) '海岸線の長さを入力してください（単位：m）'
      read(*,*) XL
      DX = XL / (NX-2)
      
      write(*,*) '突堤の位置を入力してください（単位：m）'
      write(*,*) '注）：突堤の位置は左側からの距離である'
      read(*,*) XNGR
      
      NGR=1
      IGR(1) = XNGR / DX + 1
      
      write(*,*) '計算時間を入力してください （単位：月）'
      read(*,*) TMAX
      TMAX = TMAX*30.0*24.0*3600.0
      
      write(*,*) '計算時間間隔を入力してください（単位：s）'
      read(*,*) DT
      
      NTMAX = TMAX / DT
      
      write(*,'(a,i10)') '繰り返し回数＝', NTMAX
      
      write(*,*) '計算中……'
c      OPEN(9, FILE='input.dat', STATUS='OLD')
c      READ(9,*) H0, T, ALPHA0, DRF, SLOPE, RK, DC
c      READ(9,*) DT, TMAX, XL
c      READ(9,*) IBCL, IBCR
c      IF(IBCL.EQ.1) READ(9,*) QBCL
c      IF(IBCR.EQ.1) READ(9,*) QBCR
c      READ(9,*) NGR
c      READ(9,*) (IGR(I), I = 1, NGR)
c      CLOSE(9)
      RETURN
C-----------------------------------------------------------------------
      ENTRY INIT
      NX1 = NX-1
      DX = XL / (NX - 2)
      PI   = 3.1415926
      G    = 9.806
      DTR  = PI/180.0
      RTD  = 180.0/PI
      ROWS = 2.65
      ROW  = 1.03
      PORO = 0.4
      RKAP  = RK/(16.0*(ROWS/ROW - 1.0)*(1.0-PORO))
      ALPHA0 = ALPHA0 * DTR
      FDT = DT / (DC*DX)
      TIME = 0.0
      ITER = 0
      GAMMA = 0.78
      SIGMA = 2.0*PI / T
      SIGG = SIGMA * SIGMA /G
       
      DO I = 1, NX
        Y(I) = 0.0
        YOUT(I,1) = 0.0
        YOLD(I) = 0.0
        Q(I) = 0.0
        HB(I) = 0.0
        ALB(I) = 0.0
        CGB(I) = 0.0
        IYG(I) = 0
      ENDDO
      
      X(1) = 0.0
      X(2) = X(1) + 0.5 * DX
      DO I = 3, NX1
         X(I) = X(I-1) + DX
      ENDDO
      X(NX) = X(NX1) + 0.5*DX
      
      DO IG = 1, NGR
        I = IGR(IG)
        IYG(I) = 1
      ENDDO
      RETURN
      
C-----------------------------------------------------------------------
      ENTRY WVDFRM
      CALL SNELL ( 0.0,0.0,0.0,0.0,DRF,RKRF,DUM,DUM,CGRF)
      DO I= 2, NX
        IF( IYG(I).EQ.0 ) THEN
          DXTEMP = DX
          IF(I.EQ.2.OR.I.EQ.NX) DXTEMP = 0.5*DX
          ATSNL  = ATAN( (Y(I)-Y(I-1))/DXTEMP )
          ZGI = ALPHA0 - ATSNL 
          CALL FINDBR ( H0,ZGI,HBOUT,ALBOUT)
          HB(I) = HBOUT
          ALB(I) = ALBOUT
          CGB(I) = SQRT(G*HB(I)/GAMMA)
        ENDIF
      ENDDO
      RETURN
C-----------------------------------------------------------------------
      ENTRY CALQ
      DO I = 2, NX
        IF(IYG(I).EQ.0) THEN
          ALBS = 2.0*ALB(I)
          PWR=HB(I)*HB(I)*CGB(I)
          Q(I)=PWR*RKAP*SIN(ALBS)
        ENDIF
      ENDDO
      RETURN
C-----------------------------------------------------------------------
      ENTRY QGROIN
      IF(NGR.GE.1) THEN
        DO IG = 1, NGR
           I = IGR(IG)
           Q(I) = 0.0
        ENDDO
      ENDIF
      RETURN
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
      OPEN(10, FILE='temp\cstline.dat', STATUS='unknown')
Cding      WRITE(10,'(F15.3)') TIME
      WRITE(10,'(7E13.5)') (X(I), (YOUT(I,J), J=1,6), I= 1, NX)
      CLOSE(10)
      
      DO IG = 1, NGR
        I = IGR(IG)
        HB(I) = 0.5 *(HB(I-1)+HB(I+1))
      ENDDO

      OPEN(10,FILE='temp\flux.dat',status='unknown')
         xtemp = -dx
         do i = 2, nx
           xtemp = xtemp + dx
           write(10,'(4E16.7)') xtemp, q(i)*365.0*24.0*3600.0,
     &                          HB(I), alb(i)*RTD
         enddo
      CLOSE(10)
      RETURN
      END

C=======================================================================
      SUBROUTINE  SNELL (RKIN, HIN, ZIN, CGIN, D, RK, H, Z, CG)
C=======================================================================
C     Wave refraction by Snell's law                                    |
C-----------------------------------------------------------------------
      COMMON/CNST2/SIGMA, SIGG
      
      YF   = SIGG*D
      FAY = YF+1./(1.+YF*(.6522+YF*(.4622+YF*(.0864+YF*.0675))))
      RK  = SQRT(SIGG*FAY/D)
      Z   = ASIN(RKIN*SIN(ZIN)/RK)
      RKR = SQRT(COS(ZIN)/COS(Z))
      ARG2 = 2.0*RK*D
      CG  = 0.5*(1.+ARG2/SINH(ARG2))*SIGMA/RK
      RKS = SQRT(CGIN/CG)
      H   = HIN*RKR*RKS
      RETURN
      END

C=======================================================================
      SUBROUTINE FINDBR(HRF,ZRF,HBR,ZBR)
C=======================================================================
C     This routine calculates breaking wave heights and angles along    |
C     the shoreline by using a binary search technique                  |
C-----------------------------------------------------------------------
      COMMON/CNST1/DRF,RKRF,CGRF,GAMMA
     
      DDEEP = DRF
      DSHAL = 0.01
      D = DSHAL
      H = DRF
      ICOUNT = 0
   10 ICOUNT = ICOUNT+1
      IF( ICOUNT.EQ.30 )  GO TO 30
      HBR = GAMMA*D
      IF( ABS(HBR-H) .LE. 0.005*HBR )  GO TO 30
      IF( H.LT.HBR )  GO TO 20
      DSHAL=D
      D=0.5*(DDEEP+D)
      CALL SNELL ( RKRF,HRF,ZRF,CGRF,D,RK,H,Z,CG )
      GO TO 10
   20 DDEEP=D
      D=0.5*(DSHAL+D)
      CALL SNELL ( RKRF,HRF,ZRF,CGRF,D,RK,H,Z,CG )
      GO TO 10
   30 HBR=H
      ZBR=Z
      IF(ICOUNT.LT.20)  GO TO 999
      WRITE( 6, 40 )

   40 FORMAT(1H ,'WARNING:Calculation for HB did not converge')
  999 RETURN
      END
