$PROBLEM NM TRIANING BASE MODEL
;DV: mcg/mL
;TIME: day
;CL  : L/d
;VC   : L

$DATA ../../../data/derived/PK_Data_v01.csv IGNORE = @

$INPUT C LINE ID TIME AMT RATE DV EVID MDV CMT LLOQ BLQ DOSEN COHORT BWT BAGE BALB SEXF RACEN COHORTC=DROP SEXC=DROP RACEC=DROP

$SUBROUTINE ADVAN6 TRANS1 TOL=6 ;general DiffEq solver

$MODEL
  COMP = (CENTRAL) ;central: CMT = 1

$PK
TVCL = THETA(1) ;Typical value clearance
CL = TVCL * EXP(ETA(1)) ;Individual clearance value

TVVC = THETA(2) ;Typical value central compartment volume
VC = TVVC * EXP(ETA(2)) ;Individual Vc

S1 = VC * 1; mg/L == ug/mL

K10 = CL / VC ; clearance rate, (L/d / L = 1/d)

$DES
DADT(1) = -K10*A(1)

$ERROR (OBSERVATION ONLY)
IPRED = F
W = SQRT(SIGMA(1)*F*F + SIGMA(2)) ;variance on (y = F + eps(1) + Feps(2))
IRES = DV-IPRED
IWRES = IRES/W
Y = IPRED * (1 + EPS(1)) + EPS(2)

$THETA
(0, 0.5)  ;1 CL (L/d)
(0, 20)   ;2 VC (L)

$OMEGA
0.09     ;11 IIV on CL: 30% CV on CL (%CV/100)^2 = sigma_CL^2
0.2025   ;12 IIV on VC: 45% CV on Vc (%CV/100)^2 = sigma_Vc^2

$SIGMA
0.05      ; PROPORTIONAL ERROR
0.05      ; ADDITIVE ERROR

$ESTIMATION METHOD=1 MAXEVAL=9999 PRINT=5 NOABORT

$COV PRINT=E MATRIX=S ;prints eigenvalues of variance-covariance matrix for condition number lambda_n/1

$TABLE LINE ID TIME EVID DV PRED IPRED WRES CWRES IWRES NOPRINT NOAPPEND ONEHEADER FILE=run1001.tab
$TABLE ID ETAS(1:LAST) NOPRINT NOAPPEND ONEHEADER NOTITLE FIRSTONLY FILE=run1001-param.tab