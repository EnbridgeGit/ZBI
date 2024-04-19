class ZCL_IM_ZRSU5_SAPI_BADI_PM definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_IM_ZRSU5_SAPI_BADI_PM
*"* do not include other source files here!!!

  interfaces IF_EX_RSU5_SAPI_BADI .
protected section.
*"* protected components of class ZCL_IM_ZRSU5_SAPI_BADI_PM
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_ZRSU5_SAPI_BADI_PM
*"* do not include other source files here!!!

  constants C_SOURCE_2LIS_17_I3HDR type ROOSOURCER value '2LIS_17_I3HDR'. "#EC NOTEXT
  constants C_SOURCE_2LIS_17_I3OPER type ROOSOURCER value '2LIS_17_I3OPER'. "#EC NOTEXT
  constants C_SOURCE_0PM_ORD_STATUS type ROOSOURCER value '0PM_ORDER_STATUS'. "#EC NOTEXT
  constants C_SOURCE_ZPM_MEASUREMENT type ROOSOURCER value 'ZPM_MEASUREMENT'. "#EC NOTEXT
  constants C_SOURCE_0EQUIPMENT_ATTR type ROOSOURCER value '0EQUIPMENT_ATTR'. "#EC NOTEXT
  constants C_SOURCE_0PM_MEASUREMENT type ROOSOURCER value '0PM_MEASUREMENT'. "#EC NOTEXT
  constants C_SOURCE_0PM_OBJ_STATUS type ROOSOURCER value '0PM_OBJ_STATUS'. "#EC NOTEXT

  methods DATA_TRANSFORM_2LIS_17_I3HDR
    importing
      value(I_DATASOURCE) type RSAOT_OLTPSOURCE
      value(I_UPDMODE) type SBIWA_S_INTERFACE-UPDMODE
      value(I_T_SELECT) type SBIWA_T_SELECT
      value(I_T_FIELDS) type SBIWA_T_FIELDS
    changing
      !C_T_DATA type ANY TABLE
      !C_T_MESSAGES type RSU5_T_MESSAGES optional .
  methods DATA_TRANSFORM_2LIS_17_I3OPER
    importing
      value(I_DATASOURCE) type RSAOT_OLTPSOURCE
      value(I_UPDMODE) type SBIWA_S_INTERFACE-UPDMODE
      value(I_T_SELECT) type SBIWA_T_SELECT
      value(I_T_FIELDS) type SBIWA_T_FIELDS
    changing
      !C_T_DATA type ANY TABLE
      !C_T_MESSAGES type RSU5_T_MESSAGES optional .
  methods DATA_TRANSFORM_0PM_ORD_STATUS
    importing
      value(I_DATASOURCE) type RSAOT_OLTPSOURCE
      value(I_UPDMODE) type SBIWA_S_INTERFACE-UPDMODE
      value(I_T_SELECT) type SBIWA_T_SELECT
      value(I_T_FIELDS) type SBIWA_T_FIELDS
    changing
      !C_T_DATA type ANY TABLE
      !C_T_MESSAGES type RSU5_T_MESSAGES optional .
  methods DATA_TRANSFORM_ZPM_MEASUREMENT
    importing
      value(I_DATASOURCE) type RSAOT_OLTPSOURCE
      value(I_UPDMODE) type SBIWA_S_INTERFACE-UPDMODE
      value(I_T_SELECT) type SBIWA_T_SELECT
      value(I_T_FIELDS) type SBIWA_T_FIELDS
    changing
      !C_T_DATA type ANY TABLE
      !C_T_MESSAGES type RSU5_T_MESSAGES optional .
  methods DATA_TRANSFORM_0EQUIPMENT_ATTR
    importing
      value(I_DATASOURCE) type RSAOT_OLTPSOURCE
      value(I_UPDMODE) type SBIWA_S_INTERFACE-UPDMODE
      value(I_T_SELECT) type SBIWA_T_SELECT
      value(I_T_FIELDS) type SBIWA_T_FIELDS
    changing
      !C_T_DATA type ANY TABLE
      !C_T_MESSAGES type RSU5_T_MESSAGES optional .
  methods DATA_TRANSFORM_0PM_MEASUREMENT
    importing
      value(I_DATASOURCE) type RSAOT_OLTPSOURCE
      value(I_UPDMODE) type SBIWA_S_INTERFACE-UPDMODE
      value(I_T_SELECT) type SBIWA_T_SELECT
      value(I_T_FIELDS) type SBIWA_T_FIELDS
    changing
      !C_T_DATA type ANY TABLE
      !C_T_MESSAGES type RSU5_T_MESSAGES optional .
  methods DATA_TRANSFORM_0PM_OBJ_STATUS
    importing
      value(I_DATASOURCE) type RSAOT_OLTPSOURCE
      value(I_UPDMODE) type SBIWA_S_INTERFACE-UPDMODE
      value(I_T_SELECT) type SBIWA_T_SELECT
      value(I_T_FIELDS) type SBIWA_T_FIELDS
    changing
      !C_T_DATA type ANY TABLE
      !C_T_MESSAGES type RSU5_T_MESSAGES optional .
ENDCLASS.



CLASS ZCL_IM_ZRSU5_SAPI_BADI_PM IMPLEMENTATION.


METHOD data_transform_0equipment_attr.

***********************************************************************
* Enhancement to BW extractor 0EQUIPMENT_ATTR
* Author/Date - DGRODKIE 20140616
* Purpose - The purpose of this enhancement is to enable the extraction
* of the additional Equipment Attributes, added to the work
* operation by BW.
***********************************************************************

  TYPES: BEGIN OF is_equi,
           equnr  TYPE equnr,
           herld TYPE herld,
           serge TYPE serge,
           typbz TYPE typbz,
           baujj TYPE baujj,
           baumm TYPE baumm,
           herst TYPE herst,
         END OF is_equi.

  DATA: lv_objnr  TYPE equi-objnr.

  DATA: i_t_equi TYPE STANDARD TABLE OF is_equi WITH KEY equnr.
  DATA: i_t_package TYPE STANDARD TABLE OF bwe_equi.
  FIELD-SYMBOLS: <package> TYPE bwe_equi,
                 <equi> TYPE is_equi.

* use i_t_package to select data and then release the table.
  i_t_package[] = c_t_data[].

* Select the additional fields from EQUI based upon the datapackage
  SELECT equnr
         herld
         serge
         typbz
         baujj
         baumm
         herst
    FROM equi INTO TABLE i_t_equi FOR ALL ENTRIES IN i_t_package WHERE
    equnr = i_t_package-equnr.

* clear the i_t_Package table as no longer required.
  FREE: i_t_package.

* loop over the data package and read the entries from the internal
* table.
  LOOP AT c_t_data ASSIGNING <package>.
    READ TABLE i_t_equi ASSIGNING <equi>
    WITH TABLE KEY equnr = <package>-equnr.
    IF sy-subrc = 0.
      <package>-zzherld = <equi>-herld.
      <package>-zzserge = <equi>-serge.
      <package>-zztypbz = <equi>-typbz.
      <package>-zzbaujj = <equi>-baujj.
      <package>-zzbaumm = <equi>-baumm.
      <package>-zzherst = <equi>-herst.
*{   INSERT         &$&$&$&$                                          1
*
      lv_objnr = 'IE' && <package>-equnr.
      CALL FUNCTION 'STATUS_TEXT_EDIT'
        EXPORTING
          CLIENT            = SY-MANDT
          FLG_USER_STAT     = 'X'
          OBJNR             = lv_objnr
          ONLY_ACTIVE       = 'X'
          SPRAS             = sy-langu
          BYPASS_BUFFER     = 'X'
        IMPORTING
*         ANW_STAT_EXISTING =
*         E_STSMA           =
          LINE              = <package>-zzsystxt
          USER_LINE         = <package>-zzusrtxt
*         STONR             =
        EXCEPTIONS
          OBJECT_NOT_FOUND  = 1
          OTHERS            = 2.
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
      ENDIF.

*}   INSERT
      UNASSIGN <equi>.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD DATA_TRANSFORM_0PM_MEASUREMENT.

***********************************************************************
* Enhancement to BW extractor ZPM_MEASUREMENT
* Author/Date - DGRODKIE 20140814
* Purpose - The purpose of this enhancement is to enable the extraction
* of the functional Location
***********************************************************************

  TYPES: BEGIN OF is_TPLNR,
           TPLNR  TYPE Tplnr,
           OBJNR TYPE J_OBJNR,
         END OF is_TPLNR.
*
  DATA: it_TPLNR TYPE STANDARD TABLE OF is_TPLNR WITH KEY objnr.
*{   REPLACE        D30K925560                                        3
*\  DATA: it_package TYPE STANDARD TABLE OF ZOXS010010.
  DATA: it_package TYPE STANDARD TABLE OF BWE_0PM_MEASM.
*}   REPLACE
*{   REPLACE        D30K925560                                        4
*\  FIELD-SYMBOLS: <package> TYPE ZOXS010010,
  FIELD-SYMBOLS: <package> TYPE BWE_0PM_MEASM,
*}   REPLACE
                 <TPLNR> TYPE is_TPLNR.
*{   INSERT         D30K925560                                        1
* Additional Definitions for extracting Order number
  Types:  begin of is_AUFNR,
            MDOCM type IMRC_MDOCM,
            WOOBJ type IMRC_Woobj,
          end of is_aufnr.
DATA: it_AUFNR type standard table of is_AUFNR
      with key MDOCM.
Field-Symbols: <AUFNR> type is_aufnr.

*}   INSERT
*
* use i_t_package to select data and then release the table.
  it_package[] = c_t_data[].
*
*{   DELETE         D30K925560                                        1
*\* Select the additional fields from AUFK based upon the datapackage
*\  SELECT TPLNR
*\         objnr
*\    FROM IFLOT INTO TABLE it_tplnr FOR ALL ENTRIES IN it_package WHERE
*\    objnr = it_package-MPOBJ.
*}   DELETE
*{   INSERT         D30K925560                                        2
*Select the relevant entries form the table IMRG
  SELECT mdocm
         woobj
    FROM IMRG INTO TABLE it_aufnr FOR ALL ENTRIES IN it_package WHERE
    mdocm = it_package-MDOCM.

*}   INSERT
*
* clear the i_t_Package table as no longer required.
  FREE: it_package.
*
* loop over the data package and read the entries from the internal
* table.
  LOOP AT c_t_data ASSIGNING <package>.
*{   DELETE         D30K925560                                        2
*\    READ TABLE it_tplnr ASSIGNING <TPLNR>
*\    WITH TABLE KEY objnr = <package>-MPOBJ.
*\    IF sy-subrc = 0.
*\      <package>-ZZTPLNR = <TPLNR>-TPLNR.
*\      UNASSIGN <TPLNR>.
*\    ENDIF.
*}   DELETE
*{   INSERT         D30K925560                                        3
*Read the aufnr from the internal table.
    READ TABLE it_aufnr ASSIGNING <aufnr>
    WITH TABLE KEY mdocm = <package>-mdocm.
    IF sy-subrc = 0.
      if <aufnr>-woobj(2) = 'OR'. " check to see if the Objec is an Order
      <package>-ZZAufnr = <aufnr>-woobj+6(8).
      UNASSIGN <aufnr>.
      Endif.
    ENDIF.


*}   INSERT
  ENDLOOP.


ENDMETHOD.


METHOD DATA_TRANSFORM_0PM_OBJ_STATUS.

*{   DELETE         D30K925560                                        1
*\***********************************************************************
*\* Enhancement to BW extractor 2LIS_17_I3OPER
*\* Author/Date - DGRODKIE 20140709
*\* Purpose - The purpose of this enhancement is to enable the extraction
*\* of the user who changed the status
*\***********************************************************************
*\
*\  TYPES: BEGIN OF is_ord_stat,
*\           OBJNR  TYPE J_OBJNR,
*\           STAT TYPE J_STATUS,
*\           CHGNR TYPE J_CHGNR,
*\           USNAM TYPE CDUSERNAME,
*\         END OF is_ord_stat.
*\
*\  DATA: i_t_ord_Stat TYPE STANDARD TABLE OF is_ord_stat WITH KEY objnr stat chgNR.
*\  DATA: i_t_package TYPE STANDARD TABLE OF BWE_0PM_ORDER_STATUS.
*\  FIELD-SYMBOLS: <package> TYPE BWE_0PM_ORDER_STATUS,
*\                 <ord_stat> TYPE is_ord_stat.
*\
*\* use i_t_package to select data and then release the table.
*\  i_t_package[] = c_t_data[].
*\
*\* Select the additional fields from AUFK based upon the datapackage
*\  SELECT objnr
*\         stat
*\         chgnr
*\         usnam
*\    FROM JCDS INTO TABLE i_t_ord_stat FOR ALL ENTRIES IN i_t_package WHERE
*\    objnr = i_t_package-objnr AND
*\    stat = i_t_package-stat and
*\    chgnr = i_t_package-chgnr.
*\
*\* clear the i_t_Package table as no longer required.
*\  FREE: i_t_package.
*\
*\* loop over the data package and read the entries from the internal
*\* table.
*\  LOOP AT c_t_data ASSIGNING <package>.
*\    READ TABLE i_t_ord_stat ASSIGNING <ord_stat>
*\    WITH TABLE KEY objnr = <package>-objnr
*\                   stat = <package>-stat
*\                   chgnr = <package>-chgnr.
*\    IF sy-subrc = 0.
*\      <package>-ZZUSNAM = <ord_stat>-USNAM.
*\      UNASSIGN <ord_stat>.
*\    ENDIF.
*\  ENDLOOP.
*}   DELETE
*{   INSERT         D30K925560                                        2
*
***********************************************************************
* Enhancement to BW extractor 0PM_OBJ_STATUS
* Author/Date - DGRODKIE 20150327
* Purpose - The purpose of this enhancement is to enable the extraction
* of the functional Location
***********************************************************************

  TYPES: BEGIN OF is_obj_stat,
    OBJNR  TYPE J_OBJNR,
    STAT TYPE J_STATUS,
    CHGNR TYPE J_CHGNR,
    USNAM TYPE CDUSERNAME,
  END OF is_obj_stat.
  TYPES: BEGIN OF is_FLOC,
    TPLNR TYPE TPLNR,
    OBJNR TYPE j_OBJNR,
  END OF is_FLOC.
  DATA: it_obj_Stat TYPE STANDARD TABLE OF is_obj_stat.
  DATA: it_package TYPE STANDARD TABLE OF BWE_0PM_obj_STATUS.
  DATA: it_FLOC TYPE STANDARD TABLE OF is_floc WITH KEY OBJNR.
  FIELD-symbols: <floc> TYPE is_floc.



*  DATA: i_t_ord_Stat TYPE STANDARD TABLE OF is_ord_stat WITH KEY objnr stat chgNR.
*  DATA: i_t_package TYPE STANDARD TABLE OF BWE_0PM_ORDER_STATUS.
  FIELD-SYMBOLS: <package> TYPE BWE_0PM_OBJ_STATUS.
*                 <ord_stat> TYPE is_ord_stat.

* use i_t_package to select data and then release the table.
  it_package[] = c_t_data[].

* Select the additional fields from AUFK based upon the datapackage
  SELECT TPLNR
  OBJNR
  FROM IFLOT INTO TABLE it_floc FOR ALL ENTRIES IN it_package WHERE
  objnr = it_package-objnr.

* clear the i_t_Package table as no longer required.
  FREE: it_package.

* loop over the data package and read the entries from the internal
* table.
  LOOP AT c_t_data ASSIGNING <package>.
    READ TABLE it_floc ASSIGNING <floc>
    WITH TABLE KEY objnr = <package>-objnr.
    IF sy-subrc = 0.
      <package>-ZZTPLNR = <floc>-TPLNR.
      UNASSIGN <floc>.
    ENDIF.
  ENDLOOP.



*}   INSERT

ENDMETHOD.


METHOD DATA_TRANSFORM_0PM_ORD_STATUS.

***********************************************************************
* Enhancement to BW extractor 2LIS_17_I3OPER
* Author/Date - DGRODKIE 20140709
* Purpose - The purpose of this enhancement is to enable the extraction
* of the user who changed the status
***********************************************************************

  TYPES: BEGIN OF is_ord_stat,
           OBJNR  TYPE J_OBJNR,
           STAT TYPE J_STATUS,
           CHGNR TYPE J_CHGNR,
           USNAM TYPE CDUSERNAME,
         END OF is_ord_stat.

  DATA: i_t_ord_Stat TYPE STANDARD TABLE OF is_ord_stat WITH KEY objnr stat chgNR.
  DATA: i_t_package TYPE STANDARD TABLE OF BWE_0PM_ORDER_STATUS.
  FIELD-SYMBOLS: <package> TYPE BWE_0PM_ORDER_STATUS,
                 <ord_stat> TYPE is_ord_stat.

* use i_t_package to select data and then release the table.
  i_t_package[] = c_t_data[].

* Select the additional fields from AUFK based upon the datapackage
  SELECT objnr
         stat
         chgnr
         usnam
    FROM JCDS INTO TABLE i_t_ord_stat FOR ALL ENTRIES IN i_t_package WHERE
    objnr = i_t_package-objnr AND
    stat = i_t_package-stat and
    chgnr = i_t_package-chgnr.

* clear the i_t_Package table as no longer required.
  FREE: i_t_package.

* loop over the data package and read the entries from the internal
* table.
  LOOP AT c_t_data ASSIGNING <package>.
    READ TABLE i_t_ord_stat ASSIGNING <ord_stat>
    WITH TABLE KEY objnr = <package>-objnr
                   stat = <package>-stat
                   chgnr = <package>-chgnr.
    IF sy-subrc = 0.
      <package>-ZZUSNAM = <ord_stat>-USNAM.
      UNASSIGN <ord_stat>.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD data_transform_2lis_17_i3hdr.

***********************************************************************
* Enhancement to BW extractor 2LIS_17_I3HDR
* Author/Date - DGRODKIE 20140609
* Purpose - The purpose of this enhancement is to enable the extraction
* of the additional Work Order fields, added to the work order header,
* by BW.
***********************************************************************

  TYPES: BEGIN OF is_ord_hdr,
           aufnr  TYPE aufnr,
           zzpmcompdate TYPE  zpmcompdate,
           zzpmbworkord TYPE  zpmbworkord,
           zzpmbworktype TYPE  zpmbworktype,
           zzpmbscheddate TYPE  zpmbscheddate,
           zzpmbstatus TYPE zpmbstatus,
         END OF is_ord_hdr.

  DATA: i_t_ord_sup TYPE STANDARD TABLE OF is_ord_hdr WITH KEY aufnr.
  DATA: i_t_package TYPE STANDARD TABLE OF mc17i30hdr.
  FIELD-SYMBOLS: <package> TYPE mc17i30hdr,
                 <ord_sup> TYPE is_ord_hdr.

* use i_t_package to select data and then release the table.
  i_t_package[] = c_t_data[].

* Select the additional fields from AUFK based upon the datapackage
  SELECT aufnr
         zzpmcompdate
         zzpmbworkord
         zzpmbworktype
         zzpmbscheddate
         zzpmbstatus
    FROM aufk INTO TABLE i_t_ord_sup FOR ALL ENTRIES IN i_t_package WHERE
    aufnr = i_t_package-aufnr.

* clear the i_t_Package table as no longer required.
  FREE: i_t_package.

* loop over the data package and read the entries from the internal
* table.
  LOOP AT c_t_data ASSIGNING <package>.
    READ TABLE i_t_ord_sup ASSIGNING <ord_sup>
    WITH TABLE KEY aufnr = <package>-aufnr.
    IF sy-subrc = 0.
      <package>-zzpmcompdate = <ord_sup>-zzpmcompdate.
      <package>-zzpmbworkord = <ord_sup>-zzpmbworkord.
      <package>-zzpmbworktype = <ord_sup>-zzpmbworktype.
      <package>-zzpmbscheddate = <ord_sup>-zzpmbscheddate.
      <package>-zzpmbstatus = <ord_sup>-zzpmbstatus.
      UNASSIGN <ord_sup>.
    ENDIF.
  ENDLOOP.
  UNASSIGN <package>.

* Second loop for compliance date check and population based on
* priority  if compliance date is not populated.
  LOOP AT c_t_data ASSIGNING <package>.
* check whether compliance date is populated.
    IF <package>-zzpmcompdate = ''
      OR <package>-zzpmcompdate = '00000000'.
* check whether Priority is populated
      CASE <package>-priok.
* If it is then calculate the date based on Priority and
* created_on Date.
        WHEN '1'.
* due in 1 Day
* ERDAT - Date record created.
          <package>-zzpmcompdate = <package>-erdat + 1.
        WHEN '2'.
* due in 2 Days
          <package>-zzpmcompdate = <package>-erdat + 2.
        WHEN '3'.
* due in 7 Days
          <package>-zzpmcompdate = <package>-erdat + 7.
        WHEN '4'.
* due in 1 month
          CALL FUNCTION 'BKK_ADD_MONTH_TO_DATE'
            EXPORTING
              months  = 1
              olddate = <package>-erdat
            IMPORTING
              newdate = <package>-zzpmcompdate.

        WHEN '5'.
* due in 42 days
          <package>-zzpmcompdate = <package>-erdat + 42.
        WHEN '6'.
* due in 3 months
          CALL FUNCTION 'BKK_ADD_MONTH_TO_DATE'
            EXPORTING
              months  = 3
              olddate = <package>-erdat
            IMPORTING
              newdate = <package>-zzpmcompdate.

        WHEN '7'.
* due in 1 Year
          CALL FUNCTION 'BKK_ADD_MONTH_TO_DATE'
            EXPORTING
              months  = 12
              olddate = <package>-erdat
            IMPORTING
              newdate = <package>-zzpmcompdate.

*{   INSERT         D30K926704                                        1

        When 'C'.
* priority is "C"
          <package>-zzpmcompdate = '19000101'.
*}   INSERT

        WHEN OTHERS.
* Set as created date.
          <package>-zzpmcompdate = <package>-erdat.

      ENDCASE.


    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD data_transform_2lis_17_i3oper.

***********************************************************************
* Enhancement to BW extractor 2LIS_17_I3OPER
* Author/Date - DGRODKIE 20140616
* Purpose - The purpose of this enhancement is to enable the extraction
* of the additional Work Order Operation fields, added to the work
* operation by BW.
***********************************************************************

  TYPES: BEGIN OF is_ord_oper,
           aufpl  TYPE co_aufpl,
           aplzl TYPE co_aplzl,
           zzusr00 TYPE usrchar20,
           zzusr01 TYPE usrchar20,
           zzusr02 TYPE usrchar10,
           zzusr03 TYPE usrchar10,
           zzusr04 TYPE usrquan13,
           zzuse04 TYPE usrunit,
         END OF is_ord_oper.

  DATA: i_t_ord_sup TYPE STANDARD TABLE OF is_ord_oper WITH KEY aufpl aplzl.
  DATA: i_t_package TYPE STANDARD TABLE OF mc17i30opr.
  FIELD-SYMBOLS: <package> TYPE mc17i30opr,
                 <ord_sup> TYPE is_ord_oper.

* use i_t_package to select data and then release the table.
  i_t_package[] = c_t_data[].

* Select the additional fields from AUFK based upon the datapackage
  SELECT aufpl
         aplzl
         usr00
         usr01
         usr02
         usr03
         usr04
         use04
    FROM afvu INTO TABLE i_t_ord_sup FOR ALL ENTRIES IN i_t_package WHERE
    aufpl = i_t_package-aufpl AND
    aplzl = i_t_package-aplzl.

* clear the i_t_Package table as no longer required.
  FREE: i_t_package.

* loop over the data package and read the entries from the internal
* table.
  LOOP AT c_t_data ASSIGNING <package>.
    READ TABLE i_t_ord_sup ASSIGNING <ord_sup>
    WITH TABLE KEY aufpl = <package>-aufpl
                   aplzl = <package>-aplzl.
    IF sy-subrc = 0.
      <package>-zzusr00 = <ord_sup>-zzusr00.
      <package>-zzusr01 = <ord_sup>-zzusr01.
      <package>-zzusr02 = <ord_sup>-zzusr02.
      <package>-zzusr03 = <ord_sup>-zzusr03.
      <package>-zzusr04 = <ord_sup>-zzusr04.
      <package>-zzuse04 = <ord_sup>-zzuse04.
      UNASSIGN <ord_sup>.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD DATA_TRANSFORM_ZPM_MEASUREMENT.

***********************************************************************
* Enhancement to BW extractor ZPM_MEASUREMENT
* Author/Date - DGRODKIE 20140814
* Purpose - The purpose of this enhancement is to enable the extraction
* of the functional Location
***********************************************************************

  TYPES: BEGIN OF is_TPLNR,
           TPLNR  TYPE Tplnr,
           OBJNR TYPE J_OBJNR,
         END OF is_TPLNR.
*
  DATA: it_TPLNR TYPE STANDARD TABLE OF is_TPLNR WITH KEY objnr.
  DATA: it_package TYPE STANDARD TABLE OF ZOXS010010.
  FIELD-SYMBOLS: <package> TYPE ZOXS010010,
                 <TPLNR> TYPE is_TPLNR.
*{   INSERT         D30K925560                                        1
* Additional Definitions for extracting Order number
  Types:  begin of is_AUFNR,
            MDOCM type IMRC_MDOCM,
            WOOBJ type IMRC_Woobj,
          end of is_aufnr.
DATA: it_AUFNR type standard table of is_AUFNR
      with key MDOCM.
Field-Symbols: <AUFNR> type is_aufnr.

*}   INSERT
*
* use i_t_package to select data and then release the table.
  it_package[] = c_t_data[].
*
* Select the additional fields from AUFK based upon the datapackage
  SELECT TPLNR
         objnr
    FROM IFLOT INTO TABLE it_tplnr FOR ALL ENTRIES IN it_package WHERE
    objnr = it_package-MPOBJ.
*{   INSERT         D30K925560                                        2
*Select the relevant entries form the table IMRG
  SELECT mdocm
         woobj
    FROM IMRG INTO TABLE it_aufnr FOR ALL ENTRIES IN it_package WHERE
    mdocm = it_package-MDOCM.

*}   INSERT
*
* clear the i_t_Package table as no longer required.
  FREE: it_package.
*
* loop over the data package and read the entries from the internal
* table.
  LOOP AT c_t_data ASSIGNING <package>.
    READ TABLE it_tplnr ASSIGNING <TPLNR>
    WITH TABLE KEY objnr = <package>-MPOBJ.
    IF sy-subrc = 0.
      <package>-ZZTPLNR = <TPLNR>-TPLNR.
      UNASSIGN <TPLNR>.
    ENDIF.
*{   INSERT         D30K925560                                        3
*Read the aufnr from the internal table.
    READ TABLE it_aufnr ASSIGNING <aufnr>
    WITH TABLE KEY mdocm = <package>-mdocm.
    IF sy-subrc = 0.
      if <aufnr>-woobj(2) = 'OR'. " check to see if the Objec is an Order
      <package>-ZZAufnr = <aufnr>-woobj+6(8).
      UNASSIGN <aufnr>.
      Endif.
    ENDIF.


*}   INSERT
  ENDLOOP.


ENDMETHOD.


METHOD if_ex_rsu5_sapi_badi~data_transform.


  CASE i_datasource.
    WHEN me->c_source_2lis_17_i3hdr.

      me->data_transform_2lis_17_i3hdr(
   EXPORTING
     i_datasource = i_datasource
     i_updmode    = i_updmode
     i_t_select   = i_t_select
     i_t_fields   = i_t_fields
     CHANGING
       c_t_data   = c_t_data
   ).

    WHEN me->c_source_2lis_17_i3oper.

      me->data_transform_2lis_17_i3oper(
   EXPORTING
     i_datasource = i_datasource
     i_updmode    = i_updmode
     i_t_select   = i_t_select
     i_t_fields   = i_t_fields
     CHANGING
       c_t_data   = c_t_data
   ).

    WHEN me->c_source_0pm_ord_status.

      me->data_transform_0pm_ord_status(
   EXPORTING
     i_datasource = i_datasource
     i_updmode    = i_updmode
     i_t_select   = i_t_select
     i_t_fields   = i_t_fields
     CHANGING
       c_t_data   = c_t_data
   ).

    WHEN me->c_source_zpm_measurement.

      me->data_transform_zpm_measurement(
   EXPORTING
     i_datasource = i_datasource
     i_updmode    = i_updmode
     i_t_select   = i_t_select
     i_t_fields   = i_t_fields
     CHANGING
       c_t_data   = c_t_data
   ).

    WHEN me->c_source_0equipment_attr.

      me->data_transform_0equipment_attr(
   EXPORTING
     i_datasource = i_datasource
     i_updmode    = i_updmode
     i_t_select   = i_t_select
     i_t_fields   = i_t_fields
     CHANGING
       c_t_data   = c_t_data
   ).
*{   INSERT         D30K925560                                        1
    WHEN me->c_source_0pm_measurement.

      me->data_transform_0pm_measurement(
   EXPORTING
     i_datasource = i_datasource
     i_updmode    = i_updmode
     i_t_select   = i_t_select
     i_t_fields   = i_t_fields
     CHANGING
       c_t_data   = c_t_data
   ).

    WHEN me->c_source_0pm_obj_status.

      me->data_transform_0pm_obj_status(
   EXPORTING
     i_datasource = i_datasource
     i_updmode    = i_updmode
     i_t_select   = i_t_select
     i_t_fields   = i_t_fields
     CHANGING
       c_t_data   = c_t_data
   ).

*
*}   INSERT

  ENDCASE.


ENDMETHOD.


method IF_EX_RSU5_SAPI_BADI~HIER_TRANSFORM.
endmethod.
ENDCLASS.
