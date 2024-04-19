FUNCTION ZCCM_COSS_READ_BW.
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
*"      E_T_DATA STRUCTURE  ZCCM_ECOSS OPTIONAL
*"----------------------------------------------------------------------
*  TABLES: zbset.

* Auxiliary Selection criteria structure
  DATA: l_s_select TYPE srsc_s_select.

* Maximum number of lines for DB table
  STATICS: s_s_if TYPE srsc_s_if_simple,

* counter
          s_counter_datapakid LIKE sy-tabix,

* cursor
          s_cursor TYPE cursor.


  DATA: lt_gjahr  TYPE RANGE OF cosp-gjahr,
        lt_kstar  TYPE RANGE OF cosp-kstar,
        lt_lednr  TYPE RANGE OF cosp-lednr,
        lt_versn  TYPE RANGE OF cosp-versn,
        lt_wrttp  TYPE RANGE OF cosp-wrttp.

  FIELD-SYMBOLS: <lfs_gjahr>  LIKE LINE OF lt_gjahr,
                 <lfs_kstar>  LIKE LINE OF lt_kstar,
                 <lfs_lednr>  LIKE LINE OF lt_lednr,
                 <lfs_versn>  LIKE LINE OF lt_versn,
                 <lfs_wrttp>  LIKE LINE OF lt_wrttp,
                 <lfs_data>   LIKE LINE OF e_t_data,
                 <lfs_val>    TYPE any.
  DATA: lv_waers TYPE tka01-waers.


* Initialization mode (first call by SAPI) or data transfer mode
* (following calls) ?
  IF i_initflag = sbiwa_c_flag_on.
************************************************************************
* Initialization: check input parameters
*    sap community network sdn - sdn.sap.com | BPX - bpx.sap.com | boc - boc.sap.com
*    © 2009 sap ag 6 using timestamps in generic delta extraction by function module
* buffer input parameters
* prepare data selection
************************************************************************
* Check DataSource validity
    CASE i_dsource.
      WHEN 'ZCCM_ECOSS'.
      WHEN OTHERS.
        IF 1 = 2. MESSAGE e009(r3). ENDIF.
* this is a typical log call. Please write every error message like this
*        log_write 'E' "message type
*        'R3' "message class
*        '009' "message number
*        i_dsource "message variable 1
*        ' '. "message variable 2
        RAISE error_passed_to_mess_handler.
    ENDCASE.
    APPEND LINES OF i_t_select TO s_s_if-t_select.
* Fill parameter buffer for data extraction calls
    s_s_if-requnr = i_requnr.
    s_s_if-dsource = i_dsource.
    s_s_if-maxsize = i_maxsize.
* Fill field list table for an optimized select statement
* (in case that there is no 1:1 relation between InfoSource fields
* and database table fields this may be far from beeing trivial)
    APPEND LINES OF i_t_fields TO s_s_if-t_fields.
  ELSE. "Initialization mode or data extraction ?
************************************************************************
* Data transfer: First Call OPEN CURSOR + FETCH
* Following Calls FETCH only
************************************************************************
* First data package -> OPEN CURSOR
    IF s_counter_datapakid = 0.
* Fill range tables BW will only pass down simple selection criteria
* of the type SIGN = 'I' and OPTION = 'EQ' or OPTION = 'BT'.

      LOOP AT s_s_if-t_select INTO l_s_select .
        CASE l_s_select-fieldnm.
          WHEN 'GJAHR'.
            APPEND INITIAL LINE TO lt_gjahr ASSIGNING <lfs_gjahr>.
            MOVE-CORRESPONDING l_s_select TO <lfs_gjahr>.
          WHEN 'KSTAR'.
            APPEND INITIAL LINE TO lt_kstar ASSIGNING <lfs_kstar>.
            MOVE-CORRESPONDING l_s_select TO <lfs_kstar>.
          WHEN 'LEDNR'.
            APPEND INITIAL LINE TO lt_lednr ASSIGNING <lfs_lednr>.
            MOVE-CORRESPONDING l_s_select TO <lfs_lednr>.
          WHEN 'VERSN'.
            APPEND INITIAL LINE TO lt_versn ASSIGNING <lfs_versn>.
            MOVE-CORRESPONDING l_s_select TO <lfs_versn>.
          WHEN 'WRTTP'.
            APPEND INITIAL LINE TO lt_wrttp ASSIGNING <lfs_wrttp>.
            MOVE-CORRESPONDING l_s_select TO <lfs_wrttp>.
          WHEN OTHERS.
        ENDCASE.
        CLEAR: l_s_select.
      ENDLOOP.


* Determine number of database records to be read per FETCH statement
* from input parameter I_MAXSIZE. If there is a one to one relation
* between DataSource table lines and database entries, this is trivial.
* In other cases, it may be impossible and some estimated value has to
* be determined.
      OPEN CURSOR WITH HOLD s_cursor FOR
      SELECT lednr
             objnr
             gjahr
             wrttp
            versn
            kstar
            beknz
            wkg001
            wkg002
            wkg003
            wkg004
            wkg005
            wkg006
            wkg007
            wkg008
            wkg009
            wkg010
            wkg011
            wkg012
            wkg013
            wkg014
            wkg015
            wkg016
            bukrs
            hrkft
            vrgng
            parob
            perbl
       FROM coss
      WHERE gjahr  IN lt_gjahr
        AND lednr  IN lt_lednr
        AND kstar  IN lt_kstar
        AND versn  IN lt_versn
        AND wrttp  IN lt_wrttp.
    ENDIF. "First data package ?
* Fetch records into interface table.
* named E_T_'Name of extract structure'.
    FETCH NEXT CURSOR s_cursor
                APPENDING TABLE e_t_data
                PACKAGE SIZE s_s_if-maxsize.
    IF sy-subrc <> 0.
      CLOSE CURSOR s_cursor.
      RAISE no_more_data.
    ENDIF.
    SELECT SINGLE waers FROM tka01 INTO lv_waers
      WHERE kokrs = '10'.
    IF sy-subrc NE 0.
      lv_waers = 'CAD'.
    ENDIF.
    LOOP AT e_t_data ASSIGNING <lfs_data>.
      ASSIGN COMPONENT 'TWAER' OF STRUCTURE <lfs_data> TO <lfs_val>.
      IF sy-subrc EQ 0.
        <lfs_val> = lv_waers.
      ENDIF.
    ENDLOOP.

    s_counter_datapakid = s_counter_datapakid + 1.
  ENDIF. "Initialization mode or data extraction ?

ENDFUNCTION.
