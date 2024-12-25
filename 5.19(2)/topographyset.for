C 計算領域の設定
C
C
       PARAMETER(IMAX=500,JMAX=500)
       DIMENSION DEP(IMAX,JMAX),IST(IMAX,JMAX)
      write(*,*) '沖側境界の水深を入力してください（単位：m）'
      read(*,*) DEP0

      write(*,*) '海底勾配を入力してください（例　1/30であれば30）'
      read(*,*) GRAD

      write(*,*) '計算領域の沿岸方向の長さ（m）を入力してください。'
      read(*,*) YMAX

      write(*,*) '計算格子の大きさ（m）を入力してください。'
      read(*,*) DS
   

      write(*,*) '離岸堤の離岸距離を入力してください（単位：m）'
      read(*,*) XB
      write(*,*) '離岸堤の長さを入力してください（単位：m）'
      read(*,*) YB
	  DX=DS
	  DY=DS
       XMAX=INT(DEP0*GRAD)

	  IM=XMAX/DX+1
	  JM=YMAX/Dy+1

	  DO J=1,JM
	  DO I=1,IM
	  IST(i,j)=0
	  DEP(I,J)=DEP0-(I-1)*DX/GRAD
	  IF(DEP(i,j).le.0.0) THEN
	  DEP(I,j)=0.1
	  IST(I,j)=10
	ENDIF
	  END DO
	  END DO
	  IBE=(IM-1)-XB/DX
	  IBS=IBE-1
	  JBS=(JM-1)/2-INT((YB/2.0)/DY)
	  JBE=(JM-1)/2+INT((YB/2.0)/DY)
	write(*,*) IM,IBS,IBE,JM,JBS,JBE
	  DO I=IBS,IBE
	  DO J=JBS,JBE
	  IST(i,j)=10
	  END DO
	  END DO
	open(1,file='inputdata\depth.dat')
	  open(2,file='inputdata\str.dat')
	WRITE(1,'(2i7,2F7.3)') im,jm,ds,grad
	  DO I=1,IM
	  write(1,'(300F7.3)') (DEP(i,j),J=1,JM)
	  write(2,'(300I4)') (IST(i,j),J=1,JM)
	  END DO
	  CLOSE(2)
	  CLOSE(1)
	  STOP
	  END

