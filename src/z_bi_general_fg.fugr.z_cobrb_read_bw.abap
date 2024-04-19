FUNCTION Z_COBRB_READ_BW.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_REQUNR) TYPE  SRSC_S_IF_SIMPLE-REQUNR
*"     VALUE(I_DSOURCE) TYPE  SRSC_S_IF_SIMPLE-DSOURCE OPTIONAL
*"     VALUE(I_MAXSIZE) TYPE  SRSC_S_IF_SIMPLE-MAXSIZE OPTIONAL
*"     VALUE(I_INITFLAG) TYPE  SRSC_S_IF_SIMPLE-INITFLAG OPTIONAL
*"     VALUE(I_READ_ONLY) TYPE  SRSC_S_IF_SIMPLE-READONLY OPTIONAL
*"     VALUE(I_REMOTE_CALL) TYPE  SBIWA_FLAG DEFAULT SBIWA_C_FLAG_OFF
*"  TABLES
*"      I_T_SELECT TYPE  SRSC_S_IF_SIMPLE-T_SELECT OPTIONAL
*"      I_T_FIELDS TYPE  SRSC_S_IF_SIMPLE-T_FIELDS OPTIONAL
*"      E_T_DATA STRUCTURE  ZBW_COBRB OPTIONAL
*"  EXCEPTIONS
*"      NO_MORE_DATA
*"      ERROR_PASSED_TO_MESS_HANDLER
*"----------------------------------------------------------------------

* Example: DataSource for table SFLIGHT
  TABLES: cobrb.

* Auxiliary Selection criteria structure
  DATA: l_s_select TYPE srsc_s_select.

* Maximum number of lines for DB table
  STATICS: s_s_if TYPE srsc_s_if_simple,

* counter
          s_counter_datapakid LIKE sy-tabix,

* cursor
          s_cursor TYPE cursor.
* Select ranges
 DATA: it_objnr TYPE RANGE OF cobrb-objnr,
       wa_objnr LIKE LINE OF it_objnr.

  FIELD-SYMBOLS: <fs_cobrb> TYPE zbw_cobrb.

* Initialization mode (first call by SAPI) or data transfer mode
* (following calls) ?
  IF i_initflag = sbiwa_c_flag_on.

************************************************************************
* Initialization: check input parameters
*                 buffer input parameters
*                 prepare data selection
************************************************************************

* Check DataSource validity
    CASE i_dsource.
      WHEN 'ZBW_COBRB'.
      WHEN OTHERS.
        IF 1 = 2. MESSAGE e009(r3). ENDIF.
* this is a typical log call. Please write every error message like this
*          log_write 'E'                  "message type
*                  'R3'                 "message class
*                  '009'                "message number
*                  i_dsource   "message variable 1
*                  ' '.                 "message variable 2
*        RAISE error_passed_to_mess_handler.
    ENDCASE.

    APPEND LINES OF i_t_select TO s_s_if-t_select.

* Fill parameter buffer for data extraction calls
    s_s_if-requnr    = i_requnr.
    s_s_if-dsource = i_dsource.
    s_s_if-maxsize   = i_maxsize.


* Fill field list table for an optimized select statement
* (in case that there is no 1:1 relation between InfoSource fields
* and database table fields this may be far from beeing trivial)
    APPEND LINES OF i_t_fields TO s_s_if-t_fields.

  ELSE.                 "Initialization mode or data extraction ?

************************************************************************
* Data transfer: First Call      OPEN CURSOR + FETCH
*                Following Calls FETCH only
************************************************************************

* First data package -> OPEN CURSOR
    IF s_counter_datapakid = 0.

* Fill range tables BW will only pass down simple selection criteria
* of the type SIGN = 'I' and OPTION = 'EQ' or OPTION = 'BT'.
      LOOP AT S_S_IF-T_SELECT INTO L_S_SELECT WHERE FIELDNM = 'OBJNR'.
        CLEAR: wa_objnr.
        MOVE-CORRESPONDING L_S_SELECT TO wa_objnr.
        APPEND wa_objnr TO it_objnr.
      ENDLOOP.


* Determine number of database records to be read per FETCH statement
* from input parameter I_MAXSIZE. If there is a one to one relation
* between DataSource table lines and database entries, this is trivial.
* In other cases, it may be impossible and some estimated value has to
* be determined.
      OPEN CURSOR WITH HOLD s_cursor FOR
      SELECT mandt
            objnr
            bureg
            lfdnr
            perbz
            urzuo
            ersja
            erspe
            letja
            letpe
            konty
            kostl
            aufnr
            ps_psp_pnr
            rec_objnr1
            kokrs
      FROM cobrb
      WHERE objnr IN it_objnr
        AND urzuo NE '010'.
*        AND ps_psp_pnr NE space.
    ENDIF.


    "First data package ?

* Fetch records into interface table.
*   named E_T_'Name of extract structure'.
    FETCH NEXT CURSOR s_cursor
               APPENDING CORRESPONDING FIELDS
               OF TABLE e_t_data
               PACKAGE SIZE s_s_if-maxsize.

    IF sy-subrc <> 0.
      CLOSE CURSOR s_cursor.
      RAISE no_more_data.
    ENDIF.

    LOOP AT e_t_data ASSIGNING <fs_cobrb>.
      IF <fs_cobrb>-ps_psp_pnr IS NOT INITIAL.
        SELECT SINGLE posid FROM prps INTO <fs_cobrb>-ps_psp_pnr1
          WHERE pspnr = <fs_cobrb>-ps_psp_pnr.
      ENDIF.
    ENDLOOP.

    s_counter_datapakid = s_counter_datapakid + 1.

  ENDIF.              "Initialization mode or data extraction ?




ENDFUNCTION.
