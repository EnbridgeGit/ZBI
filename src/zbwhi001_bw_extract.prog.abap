REPORT  ZBWHI001_BW_EXTRACT.
*&---------------------------------------------------------------------*
*& Report  ZBWHI001_BW_EXTRACT
*&
*&---------------------------------------------------------------------*
*& Author: Glenn Ymana
*& Date:   April 7, 2016
*& Issue:  ACR-881 Cost Management Project
*&
*& This program will execute function module ZBW_EXTRACTPROC remotely
*& in BW which in turn will execute a BW process chain based on the
*& system ID that will be passed
*&---------------------------- CHANGE LOG -----------------------------*
*&
*&---------------------------------------------------------------------*

TABLES:  zvarsys.

DATA: v_sysid       like sy-sysid,
      v_returncode  TYPE i,
      v_rfcdest     TYPE zvarsys-value1,
      v_error_flag  TYPE C,
      v_msg(255)    TYPE C.

DATA: begin of itab_bapi_return occurs 0.
        include structure bapiret2.
DATA: end of itab_bapi_return.

*-----------------  SELECTION SCREEN -----------------------------------
PARAMETERS:
    p_pgmnam LIKE zvarsys-programm DEFAULT 'LOGSYS' OBLIGATORY,
    p_varnam LIKE zvarsys-varname  DEFAULT 'BW_RFC' OBLIGATORY,
    p_varnum LIKE zvarsys-varnum   DEFAULT '1' OBLIGATORY.

MOVE sy-sysid TO v_sysid.

* Select RFC destination for BW system
SELECT SINGLE  value1
         FROM  zvarsys
         INTO  v_rfcdest
         WHERE programm = p_pgmnam
         AND   varname  = p_varnam
         AND   varnum   = p_varnum.
IF sy-subrc <> 0.
  MESSAGE E036(zs).
  EXIT.
ENDIF.

* Call BW function to exectue BW process chain based on sysid.

CALL FUNCTION 'Z_BW_EXTRACTPROC' DESTINATION v_rfcdest
  EXPORTING
    ECC_SYSID     = v_sysid
  TABLES
    RETURNMSG     = itab_bapi_return.

IF sy-subrc <> 0.
  v_msg = 'Could not Call FM Z_BW_EXTRACTPROC'.
  MESSAGE A019(ZS) WITH v_msg.
ELSE.
  v_error_flag = 'N'.
  LOOP AT itab_bapi_return.
    CONCATENATE: itab_bapi_return-type
                 itab_bapi_return-id
                 itab_bapi_return-number
                 itab_bapi_return-message
           INTO v_msg SEPARATED BY space.
    WRITE v_msg.
    IF itab_bapi_return-type = 'E'.
      v_error_flag = 'Y'.
      MESSAGE A019(ZS) WITH v_msg.
    ENDIF.
  ENDLOOP.
  IF v_error_flag = 'N'.
    MESSAGE I019(ZS) with v_msg.
  ENDIF.
ENDIF.
