C �v�Z�̈�̐ݒ�
C
C
       PARAMETER(IMAX=500,JMAX=500)
       DIMENSION DEP(IMAX,JMAX),IST(IMAX,JMAX)
      write(*,*) '�������E�̐��[����͂��Ă��������i�P�ʁFm�j'
      read(*,*) DEP0

      write(*,*) '�C����z����͂��Ă��������i��@1/30�ł����30�j'
      read(*,*) GRAD

      write(*,*) '�v�Z�̈�̉��ݕ����̒����im�j����͂��Ă��������B'
      read(*,*) YMAX

      write(*,*) '�v�Z�i�q�̑傫���im�j����͂��Ă��������B'
      read(*,*) DS
   

      write(*,*) '���ݒ�̗��݋�������͂��Ă��������i�P�ʁFm�j'
      read(*,*) XB
      write(*,*) '���ݒ�̒�������͂��Ă��������i�P�ʁFm�j'
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

