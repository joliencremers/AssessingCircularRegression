* Encoding: windows-1252.
*restructure the original datafile

SORT CASES BY klas .
CASESTOVARS
  /ID=klas
  /GROUPBY=VARIABLE.

*teachers with missings on a variable are deleted from the file
*re-center ex and leserv variables

DATASET ACTIVATE DataSet1.
COMPUTE ex_c=ex - 0.0399.
COMPUTE leserv_c=leserv - (-0.1242).
EXECUTE.

*delete variables that are not necessary for analysis
*new file: empirical_data.sav
