
      SUBROUTINE SOLVE(N, NRHS, A, B, INFO)
      INTEGER N
      INTEGER NRHS
      REAL A(N,N)
      REAL B(N,NRHS)
      INTEGER INFO

      INTEGER I
      INTEGER LDA
      INTEGER IPIV(N)
      INTEGER LDB
      LDA=N
      LDB=N
      DO I=1,N
         IPIV(I)=I
      ENDDO
      call SGETRF(N, N, A, N, IPIV, INFO)
      call SGETRS('N', N, NRHS, A, LDA, IPIV, B, LDB, INFO)
      RETURN
      END



      SUBROUTINE EIG(N, A, D, V)
      REAL D(N)
      REAL E(N)
      REAL TAU(N-1)
      REAL WORK(28*N)
      INTEGER INFO
      REAL FDUMMY
      INTEGER IDUMMY
      INTEGER IDUMMY2
      REAL ISUPPZ(2*N)
      INTEGER IWORK(10*N)
c      CALL SSYEV('V', 'U', N, A, N, D, WORK, 18*N, INFO)
      CALL SSYEVR('V', 'A', 'U', N, A, N, FDUMMY, FDUMMY, IDUMMY, 
     *IDUMMY, DLAMCH, IDUMMY2, D, V, N, ISUPPZ, WORK, 28*N, IWORK, 10*N,
     *INFO)

      RETURN
      END



      SUBROUTINE SVD(M, N, A, U, SIGMA, VT)
      INTEGER LWORK
      REAL WORK(15*N*N)
      INTEGER IWORK(8*N)
      INTEGER INFO

      CALL SGESDD('A', M, N, A, M, SIGMA, U, M, VT, N, WORK, 15*N*N, 
     *IWORK, INFO)

c      CALL SGESVD('A', 'A', M, N, A, M, SIGMA, U, M, VT, N, WORK, 
c     *15*N*N, INFO)

      RETURN
      END
