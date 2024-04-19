class ZCL_IM_ZBW_RSU5_SAPI_BADI definition
  public
  final
  create public .

  public section.
*"* public components of class ZCL_IM_ZBW_RSU5_SAPI_BADI
*"* do not include other source files here!!!

    interfaces IF_EX_RSU5_SAPI_BADI .

    type-pools SBIWA .
    class-methods LIS_06_INV
      importing
        !I_UPDMODE type SBIWA_S_INTERFACE-UPDMODE
        !I_T_SELECT type SBIWA_T_SELECT
        !I_T_FIELDS type SBIWA_T_FIELDS
      changing
        !C_T_DATA type ANY TABLE
        !C_T_MESSAGES type RSU5_T_MESSAGES .
    class-methods LIS_02_SCL
      importing
        !I_UPDMODE type SBIWA_S_INTERFACE-UPDMODE
        !I_T_SELECT type SBIWA_T_SELECT
        !I_T_FIELDS type SBIWA_T_FIELDS
      changing
        !C_T_DATA type ANY TABLE
        !C_T_MESSAGES type RSU5_T_MESSAGES .
    class-methods LIS_02_ITM
      importing
        !I_UPDMODE type SBIWA_S_INTERFACE-UPDMODE
        !I_T_SELECT type SBIWA_T_SELECT
        !I_T_FIELDS type SBIWA_T_FIELDS
      changing
        !C_T_DATA type ANY TABLE
        !C_T_MESSAGES type RSU5_T_MESSAGES .
    class-methods LIS_02_ACC
      importing
        !I_UPDMODE type SBIWA_S_INTERFACE-UPDMODE
        !I_T_SELECT type SBIWA_T_SELECT
        !I_T_FIELDS type SBIWA_T_FIELDS
      changing
        !C_T_DATA type ANY TABLE
        !C_T_MESSAGES type RSU5_T_MESSAGES .
    class-methods LIS_03_BF
      importing
        !I_UPDMODE type SBIWA_S_INTERFACE-UPDMODE
        !I_T_SELECT type SBIWA_T_SELECT
        !I_T_FIELDS type SBIWA_T_FIELDS
      changing
        !C_T_DATA type ANY TABLE
        !C_T_MESSAGES type RSU5_T_MESSAGES .
    class-methods MATERIAL_TEXT
      importing
        !I_UPDMODE type SBIWA_S_INTERFACE-UPDMODE
        !I_T_SELECT type SBIWA_T_SELECT
        !I_T_FIELDS type SBIWA_T_FIELDS
      changing
        !C_T_DATA type ANY TABLE
        !C_T_MESSAGES type RSU5_T_MESSAGES .
    class-methods PLANT_ATTR
      importing
        !I_UPDMODE type SBIWA_S_INTERFACE-UPDMODE
        !I_T_SELECT type SBIWA_T_SELECT
        !I_T_FIELDS type SBIWA_T_FIELDS
      changing
        !C_T_DATA type ANY TABLE
        !C_T_MESSAGES type RSU5_T_MESSAGES .
    class-methods ZFIGL_BKPF_PARK
      importing
        !I_UPDMODE type SBIWA_S_INTERFACE-UPDMODE
        !I_T_SELECT type SBIWA_T_SELECT
        !I_T_FIELDS type SBIWA_T_FIELDS
      changing
        !C_T_DATA type ANY TABLE
        !C_T_MESSAGES type RSU5_T_MESSAGES .
  protected section.
*"* protected components of class ZCL_IM_ZBW_RSU5_SAPI_BADI
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_IM_ZBW_RSU5_SAPI_BADI
*"* do not include other source files here!!!

  type-pools SBIWA .
  methods AUD_AUDIT
    importing
      !I_UPDMODE type SBIWA_S_INTERFACE-UPDMODE
      !I_T_SELECT type SBIWA_T_SELECT
      !I_T_FIELDS type SBIWA_T_FIELDS
    changing
      !C_T_DATA type ANY TABLE
      !C_T_MESSAGES type RSU5_T_MESSAGES .
  methods AUD_ACTION
    importing
      !I_UPDMODE type SBIWA_S_INTERFACE-UPDMODE
      !I_T_SELECT type SBIWA_T_SELECT
      !I_T_FIELDS type SBIWA_T_FIELDS
    changing
      !C_T_DATA type ANY TABLE
      !C_T_MESSAGES type RSU5_T_MESSAGES .
  methods AUD_QUEST_ATTR
    importing
      !I_UPDMODE type SBIWA_S_INTERFACE-UPDMODE
      !I_T_SELECT type SBIWA_T_SELECT
      !I_T_FIELDS type SBIWA_T_FIELDS
    changing
      !C_T_DATA type ANY TABLE
      !C_T_MESSAGES type RSU5_T_MESSAGES .
ENDCLASS.



CLASS ZCL_IM_ZBW_RSU5_SAPI_BADI IMPLEMENTATION.


METHOD aud_action.
  TYPES:  BEGIN OF ty_auditcustom,
          guid TYPE plmm_audit-guid.
          INCLUDE TYPE zplm_plmm_audit_act.
  TYPES:  END OF ty_auditcustom.

  DATA: lt_plmm_audit_act TYPE TABLE OF plm_aud_act,
        ls_auditact       TYPE ty_auditcustom,
        lt_auditact       LIKE TABLE OF ls_auditact,

        lt_rootcause      TYPE TABLE OF dd07t,
        lt_status         TYPE TABLE OF dd07t,
        ls_text           TYPE dd07t.

  FIELD-SYMBOLS: <ls_plmm_audit_act> TYPE plm_aud_act.

  lt_plmm_audit_act[] = c_t_data[].

  "Get Text objects
  SELECT *
    FROM dd07t
    INTO CORRESPONDING FIELDS OF TABLE lt_rootcause
    WHERE domname = 'Z_ROOTCAUSE'
      AND ddlanguage = 'E'.

  SELECT *
    FROM dd07t
    INTO CORRESPONDING FIELDS OF TABLE lt_status
    WHERE domname = 'PLM_PROC_STATUS'
      AND ddlanguage = 'E'.

  "Get information fro DB Transactional Table
  SELECT *
    FROM plmm_audit_act
    INTO CORRESPONDING FIELDS OF TABLE lt_auditact
    FOR ALL ENTRIES IN lt_plmm_audit_act
    WHERE guid = lt_plmm_audit_act-guid_act.

  LOOP AT lt_plmm_audit_act ASSIGNING <ls_plmm_audit_act>.
    READ TABLE lt_auditact INTO ls_auditact
      WITH KEY guid = <ls_plmm_audit_act>-guid_act.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_auditact TO <ls_plmm_audit_act>.

      READ TABLE lt_status INTO ls_text
        WITH KEY domvalue_l = <ls_plmm_audit_act>-proc_status.
      IF sy-subrc = 0.
        <ls_plmm_audit_act>-z_status_t = ls_text-ddtext.
      ENDIF.

      READ TABLE lt_rootcause INTO ls_text
        WITH KEY domvalue_l = <ls_plmm_audit_act>-z_rootcause.
      IF sy-subrc = 0.
        <ls_plmm_audit_act>-z_rootcause_t = ls_text-ddtext.
      ENDIF.

      MODIFY lt_plmm_audit_act FROM <ls_plmm_audit_act>.
    ENDIF.

  ENDLOOP.
  REFRESH c_t_data.
  c_t_data[] = lt_plmm_audit_act[].
ENDMETHOD.


METHOD aud_audit.
  TYPES:  BEGIN OF ty_auditcustom,
          guid TYPE plmm_audit-guid.
          INCLUDE TYPE zplmt_audit.
  TYPES:  END OF ty_auditcustom.

  DATA: lt_plmm_audit TYPE TABLE OF plm_aud_audit,
        ls_audit      TYPE ty_auditcustom,
        lt_audit      LIKE TABLE OF ls_audit.

  FIELD-SYMBOLS: <ls_plmm_audit> TYPE plm_aud_audit.

  lt_plmm_audit[] = c_t_data[].

  SELECT *
    FROM plmm_audit
    INTO CORRESPONDING FIELDS OF TABLE lt_audit
    FOR ALL ENTRIES IN lt_plmm_audit
    WHERE guid = lt_plmm_audit-guid_audit.

  LOOP AT lt_plmm_audit ASSIGNING <ls_plmm_audit>.
    READ TABLE lt_audit INTO ls_audit
      WITH KEY guid = <ls_plmm_audit>-guid_audit.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_audit TO <ls_plmm_audit>.
      MODIFY lt_plmm_audit FROM <ls_plmm_audit>.
    ENDIF.
  ENDLOOP.
  REFRESH c_t_data.
  c_t_data[] = lt_plmm_audit[].
ENDMETHOD.


METHOD aud_quest_attr.

  DATA: lt_plmm_audit_quest TYPE TABLE OF plm_aud_audquest_attr,
        ls_auditquest       TYPE plmm_quest_i,
        lt_auditquest       LIKE TABLE OF ls_auditquest,

        lt_rootcause        TYPE TABLE OF dd07t,
        lt_status           TYPE TABLE OF dd07t,
        ls_text             TYPE dd07t.

  FIELD-SYMBOLS: <ls_plmm_audit_quest> TYPE plm_aud_audquest_attr.

  lt_plmm_audit_quest[] = c_t_data[].

  "Get information fro DB Transactional Table
  SELECT guid project_guid
    FROM plmm_quest_i
    INTO CORRESPONDING FIELDS OF TABLE lt_auditquest
    FOR ALL ENTRIES IN lt_plmm_audit_quest
    WHERE guid = lt_plmm_audit_quest-quest_ref_guid.

  LOOP AT lt_plmm_audit_quest ASSIGNING <ls_plmm_audit_quest>.
    READ TABLE lt_auditquest INTO ls_auditquest
      WITH KEY guid = <ls_plmm_audit_quest>-quest_ref_guid.
    IF sy-subrc = 0.
      SELECT SINGLE text1
        FROM cgpl_text
        INTO <ls_plmm_audit_quest>-z_questionlist_text
        WHERE guid = ls_auditquest-project_guid
          AND langu = 'E'.
      MODIFY lt_plmm_audit_quest FROM <ls_plmm_audit_quest>.
    ENDIF.

  ENDLOOP.
  REFRESH c_t_data.
  c_t_data[] = lt_plmm_audit_quest[].
ENDMETHOD.


  method IF_EX_RSU5_SAPI_BADI~DATA_TRANSFORM.
**********************************************************
* To implement an exit for a
* datasource create your own method by copying the
* method _TEMPLATE_DATASOURCE and rename it to the name
* of your datasource. In case you enhance a Business
* Content datasource skip the 0 at the beginning (e.g.
* Datasource 0FI_AR_3 -> Method FI_AR_3
* The method is then called by the Exit Framework
*********************************************************
    DATA: ls_oltpsource TYPE rsaot_s_osource,
    lv_data TYPE REF TO data,
    lv_method TYPE seocmpname,
    r_classdescr TYPE REF TO cl_abap_classdescr.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.
* check BW system
*check _CHECK_BW_SYSTEM( ) = 'X'.
* check if any data is extracted
    CHECK c_t_data IS NOT INITIAL.

    CALL FUNCTION 'RSA1_SINGLE_OLTPSOURCE_GET'
      EXPORTING
        i_oltpsource   = i_datasource
        i_objvers      = 'A'
      IMPORTING
        e_s_oltpsource = ls_oltpsource
      EXCEPTIONS
        no_authority   = 1
        not_exist      = 2
        inconsistent   = 3
        OTHERS         = 4.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
* create data for Extract Structure
    CREATE DATA lv_data TYPE TABLE OF (ls_oltpsource-exstruct).
    ASSIGN lv_data->* TO <lt_data>.
    ASSIGN c_t_data TO <lt_data>.
* get method name for datasource
    lv_method = i_datasource.
    IF lv_method(1) = '0' OR
    lv_method(1) = '2' OR lv_method(1) = '3'.
* shift by one character as methods can't start with a number
      SHIFT lv_method.
    ENDIF.

** check method is implemented
*CHECK check_method_exists( lv_method ) = 'X'.

*get method names in the class.
    r_classdescr ?= cl_abap_typedescr=>describe_by_name( 'ZCL_IM_ZBW_RSU5_SAPI_BADI' ).
    READ TABLE r_classdescr->methods WITH KEY name = lv_method TRANSPORTING NO FIELDS.
*if method implemented, call the respective enhancement.
    IF sy-subrc = 0.

      CALL METHOD (lv_method)
        EXPORTING
          i_updmode    = i_updmode
          i_t_select   = i_t_select
          i_t_fields   = i_t_fields
        CHANGING
          c_t_data     = <lt_data>
          c_t_messages = c_t_messages.

    ENDIF.

  endmethod.


  method IF_EX_RSU5_SAPI_BADI~HIER_TRANSFORM.
  endmethod.


  METHOD lis_02_acc.

    FIELD-SYMBOLS: <l_s_data> TYPE mc02m_0acc.
    DATA: l_c_t_data TYPE TABLE OF mc02m_0acc.

    TYPES : BEGIN OF ty_ekbe,
      ebeln TYPE ebeln,
      ebelp TYPE ebelp,
      zekkn TYPE dzekkn,
      vgabe TYPE vgabe,
      gjahr TYPE mjahr,
      belnr TYPE mblnr,
      buzei TYPE mblpo,
      dmbtr TYPE dmbtr,
      END OF ty_ekbe.


    TYPES : BEGIN OF ty_ekbe_sum,
      ebeln TYPE ebeln,
      ebelp TYPE ebelp,
      dmbtr TYPE dmbtr,
      END OF ty_ekbe_sum.

    DATA : t_ekbe_sum TYPE HASHED TABLE OF ty_ekbe_sum WITH UNIQUE KEY ebeln ebelp.

    DATA t_ekbe TYPE STANDARD TABLE OF ty_ekbe INITIAL SIZE 0.

    DATA lw_ekbe TYPE ty_ekbe.
    DATA lw_ekbe_sum TYPE ty_ekbe_sum.
    DATA lw_ekbe_tmp TYPE ty_ekbe_sum.

    l_c_t_data[] = c_t_data[].

    IF NOT l_c_t_data[] IS INITIAL.

      SELECT   ebeln
               ebelp
               zekkn
               vgabe
               gjahr
               belnr
               buzei
               dmbtr FROM  ekbe
        INTO TABLE t_ekbe
        FOR ALL ENTRIES IN l_c_t_data
            WHERE ebeln  = l_c_t_data-ebeln
             AND    ebelp  = l_c_t_data-ebelp
             AND    vgabe  IN ('2','3','P').

      IF sy-subrc = 0 .
        SORT t_ekbe BY ebeln ebelp.

        LOOP AT t_ekbe INTO lw_ekbe.

          lw_ekbe_tmp-ebeln = lw_ekbe-ebeln.
          lw_ekbe_tmp-ebelp = lw_ekbe-ebelp.
          lw_ekbe_tmp-dmbtr = lw_ekbe-dmbtr.
          COLLECT lw_ekbe_tmp INTO t_ekbe_sum.
          CLEAR lw_ekbe_tmp.
        ENDLOOP.
      ENDIF.

      LOOP AT l_c_t_data ASSIGNING <l_s_data>.
        CLEAR : lw_ekbe_sum.
        READ TABLE t_ekbe_sum  INTO lw_ekbe_sum WITH TABLE KEY ebeln = <l_s_data>-ebeln
                                                         ebelp = <l_s_data>-ebelp .

        IF sy-subrc = 0.
          <l_s_data>-zzdmbtr = lw_ekbe_sum-dmbtr.
        ENDIF.
      ENDLOOP.

      c_t_data[]  =  l_c_t_data[] .
      REFRESH: l_c_t_data, t_ekbe,t_ekbe_sum.
    ENDIF.
  ENDMETHOD.


  METHOD lis_02_itm.

    FIELD-SYMBOLS: <l_s_data> TYPE mc02m_0itm.
*
    TYPES : BEGIN OF ty_ekpo,
            ebeln TYPE ekpo-ebeln,
            ebelp TYPE ekpo-ebelp,
*        ZZWAERS TYPE EKKO-WAERS,
            zzattyp TYPE ekpo-attyp,
            loekz TYPE ekpo-loekz,
            knttp TYPE knttp,                               "D30K924804
            adrnr TYPE ekpo-adrnr,
            eildt TYPE ekpo-eildt,
            anzpu TYPE ekpo-anzpu,
            umsok TYPE ekpo-umsok,
            mfrgr TYPE ekpo-mfrgr,
            kzfme TYPE ekpo-kzfme,
            emnfr TYPE ekpo-emnfr,
            berid TYPE ekpo-berid,
            status TYPE ekpo-status,
            reslo TYPE ekpo-reslo,
            empst TYPE ekpo-empst,
            spe_insmk_src TYPE ekpo-spe_insmk_src,
            dpamt TYPE ekpo-dpamt,
            disub_sobkz TYPE ekpo-disub_sobkz,
            disub_pspnr TYPE ekpo-disub_pspnr,
            zzmtart TYPE ekpo-mtart,
            zzbstyp TYPE ekpo-bstyp,
         END OF ty_ekpo.
*
    TYPES: BEGIN OF ty_adrc,
            zaddrnumber TYPE ad_addrnum,
            name1 TYPE ad_name1, "(+) PANUSURI
            name2 TYPE ad_name2,
            zcity1 TYPE adrc-city1,
            zpost_code1 TYPE adrc-post_code1,
            zstreet TYPE adrc-street,
            zregion TYPE adrc-region,
         END OF ty_adrc.

    DATA:  l_cond(72) TYPE c,
           l_objectid(90) TYPE c.

    DATA : t_ekpo TYPE TABLE OF  ty_ekpo,
           wa_ekpo TYPE ty_ekpo,
           t_adrc TYPE TABLE OF  ty_adrc,
           wa_adrc TYPE ty_adrc,
           t_cond LIKE TABLE OF l_cond.

    DATA: t_cdhdr TYPE TABLE OF cdhdr,
           wa_cdhdr TYPE cdhdr.
*
    REFRESH t_cond.
    APPEND 'EBELN = C_T_DATA-EBELN'  TO  t_cond.
    APPEND 'AND EBELP = C_T_DATA-EBELP'  TO t_cond.
*  APPEND 'AND ZEILE = C_T_DATA-ZEILE'  TO t_itab.
*
    IF NOT c_t_data[] IS INITIAL.

      SELECT ebeln ebelp
             attyp
             loekz
             knttp                                          "D30K924804
             adrnr
             eildt
             anzpu
             umsok
             mfrgr
             kzfme
             emnfr
             berid
             status
             reslo
             empst
             spe_insmk_src
             dpamt
             disub_sobkz
             disub_pspnr
             mtart
             bstyp
              FROM ekpo
     INTO TABLE t_ekpo
     FOR ALL ENTRIES IN c_t_data
     WHERE (t_cond) .
*
      IF sy-subrc = 0.
        SORT t_ekpo BY ebeln ebelp.
*

*      SORT t_ekpo BY ebeln adrnr.
* Fetching the data from ADRC based on the T_EKPO.
        SELECT addrnumber
               name1  "(+) PANUSURI
               name2
               city1
               post_code1
               street
               region
               FROM adrc INTO TABLE t_adrc FOR ALL ENTRIES IN t_ekpo
               WHERE addrnumber = t_ekpo-adrnr.
        IF sy-subrc = 0.
          SORT t_adrc BY zaddrnumber.
        ENDIF.
      ENDIF.

      LOOP AT c_t_data  ASSIGNING <l_s_data>.
*
        READ TABLE t_ekpo INTO wa_ekpo WITH KEY ebeln = <l_s_data>-ebeln ebelp = <l_s_data>-ebelp BINARY SEARCH.
*
        IF sy-subrc = 0.
*      Data from EKPO
          <l_s_data>-zzattyp = wa_ekpo-zzattyp.
          <l_s_data>-loekz = wa_ekpo-loekz.
          <l_s_data>-knttp = wa_ekpo-knttp.                 "D30K924804
          <l_s_data>-zzadrnr = wa_ekpo-adrnr.
          <l_s_data>-eildt = wa_ekpo-eildt.
          <l_s_data>-anzpu = wa_ekpo-anzpu.
          <l_s_data>-umsok = wa_ekpo-umsok.
          <l_s_data>-mfrgr = wa_ekpo-mfrgr.
          <l_s_data>-kzfme = wa_ekpo-kzfme.
          <l_s_data>-emnfr = wa_ekpo-emnfr.
          <l_s_data>-berid = wa_ekpo-berid.
          <l_s_data>-status = wa_ekpo-status.
          <l_s_data>-reslo = wa_ekpo-reslo.
          <l_s_data>-empst = wa_ekpo-empst.
          <l_s_data>-spe_insmk_src = wa_ekpo-spe_insmk_src.
          <l_s_data>-dpamt = wa_ekpo-dpamt.
          <l_s_data>-disub_sobkz = wa_ekpo-disub_sobkz.
          <l_s_data>-disub_pspnr = wa_ekpo-disub_pspnr.
          <l_s_data>-zzmtart = wa_ekpo-zzmtart.
          <l_s_data>-zzbstyp = wa_ekpo-zzbstyp.
          <l_s_data>-zzwaers = <l_s_data>-waers.
        ENDIF.
*
        IF sy-subrc = 0.

          CLEAR wa_adrc.
          READ TABLE t_adrc INTO wa_adrc
                            WITH KEY zaddrnumber = <l_s_data>-zzadrnr
                            BINARY SEARCH.
          IF sy-subrc = 0.
            <l_s_data>-zzadrnr = wa_adrc-zaddrnumber.
            <l_s_data>-zzname1 = wa_adrc-name1.  "(+) PANUSURI
            <l_s_data>-zzname2 = wa_adrc-name2.
            <l_s_data>-zzcity1 = wa_adrc-zcity1.
            <l_s_data>-zzpost_code1 = wa_adrc-zpost_code1.
            <l_s_data>-zzstreet = wa_adrc-zstreet.
            <l_s_data>-zzregion = wa_adrc-zregion.
          ENDIF.
        ENDIF.

        CLEAR: wa_ekpo.

********************************************************************
        l_objectid =  <l_s_data>-ebeln.

        CALL FUNCTION 'BUS_CDOBJECTID_SELECT_WTH_EQ'
          EXPORTING
            iv_objectclas = 'EINKBELEG'
            iv_objectid   = l_objectid
          TABLES
            et_cdhdr      = t_cdhdr.

        SORT t_cdhdr BY udate DESCENDING.
        READ TABLE t_cdhdr INTO wa_cdhdr WITH KEY objectid = <l_s_data>-ebeln .
        <l_s_data>-zzcdusername = wa_cdhdr-username.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  method LIS_02_SCL.

    FIELD-SYMBOLS: <L_S_DATA> TYPE MC02M_0SCL.
    TYPES : BEGIN OF TY_EKPO,
          ZEBELN TYPE EBELN,
          ZEBELP  TYPE EBELP,
          ZADRNR TYPE ADRNR,
         END OF TY_EKPO.

    TYPES: BEGIN OF TY_ADRC,
            ZADDRNUMBER TYPE AD_ADDRNUM,
            ZCITY1 TYPE ADRC-CITY1,
            ZPOST_CODE1 TYPE ADRC-POST_CODE1,
            ZSTREET TYPE ADRC-STREET,
            ZREGION TYPE ADRC-REGION,
         END OF TY_ADRC.

    TYPES: BEGIN OF TY_EKETPDM,
           EBELN TYPE EKET-EBELN,
           ESTKZ TYPE EKET-ESTKZ,
        END OF TY_EKETPDM.


    DATA: D_C_T_DATA TYPE TABLE OF MC02M_0SCL,
          V_NAMEID(70) TYPE C,
          V_NAMEID3(70) TYPE C,
                ZTEXT(180) TYPE C,
           ZTEXT1(255) TYPE C,
            ZTEXT2(255) TYPE C,
*BOI by PKATRAGADDA
            HNTEXT(255) TYPE C,
           HNTEXT1(255) TYPE C,

*EOI by PKATRAGADDA
             V_NAMEID2(70) TYPE C,
             VTEXT(180) TYPE C,
           VTEXT1(255) TYPE C,
            VTEXT2(255) TYPE C,
*BOI by PKATRAGADDA
            HETEXT(255) TYPE C,
           HETEXT1(255) TYPE C.
*EOI by PKATRAGADDA
    DATA:LT_TEXT TYPE TDTAB_C132,
         WA_TEXT LIKE LINE OF LT_TEXT.
    DATA:   T_MC02M_0SCL TYPE TABLE OF MC02M_0SCL,
            T_ADRC TYPE TABLE OF  TY_ADRC,
            WA_ADRC TYPE TY_ADRC,
            T_EKPO TYPE TABLE OF TY_EKPO,
            T_LINES TYPE TABLE OF TLINE,
            WA_TLINES TYPE TLINE,
            IT_EKETPDM TYPE TABLE OF TY_EKETPDM,
            WA_EKETPDM TYPE TY_EKETPDM,
            T_LINES2 TYPE TABLE OF TLINE,
             WA_TLINES2 TYPE TLINE,
            T_LINES3 TYPE STANDARD TABLE OF TLINE,
             WA_TLINES3 TYPE TLINE,
            T_LINES4 TYPE TABLE OF TLINE,
             WA_TLINES4 TYPE TLINE.


    DATA :  WA_MC02M_0SCL TYPE MC02M_0SCL,
            WA_EKPO TYPE TY_EKPO.


    D_C_T_DATA[] = C_T_DATA[].

    IF NOT D_C_T_DATA[] IS INITIAL.
* Fetching data from EKPO based on the extracted records
      SELECT EBELN
             EBELP
             ADRNR
             FROM EKPO INTO TABLE T_EKPO FOR ALL ENTRIES IN D_C_T_DATA
             WHERE EBELN = D_C_T_DATA-EBELN
             AND   EBELP = D_C_T_DATA-EBELP.

      IF SY-SUBRC = 0.
        SORT T_EKPO BY ZEBELN ZADRNR.
* Fetching the data from ADRC based on the T_EKPO.
        SELECT ADDRNUMBER
               CITY1
               POST_CODE1
               STREET
               REGION
               FROM ADRC INTO TABLE T_ADRC FOR ALL ENTRIES IN T_EKPO
               WHERE ADDRNUMBER = T_EKPO-ZADRNR.
        IF SY-SUBRC = 0.
          SORT T_ADRC BY ZADDRNUMBER.
        ENDIF.
      ENDIF.

      SELECT EBELN
             ESTKZ FROM EKET
             INTO TABLE IT_EKETPDM FOR ALL ENTRIES IN D_C_T_DATA
            WHERE EBELN = D_C_T_DATA-EBELN.
    ENDIF.

    LOOP AT C_T_DATA  ASSIGNING <L_S_DATA>.

      REFRESH T_LINES[].
      CONCATENATE <L_S_DATA>-EBELN <L_S_DATA>-EBELP INTO V_NAMEID.

*    v_nameid = <l_s_data>-ebeln.

      CALL FUNCTION 'READ_TEXT'
   EXPORTING
    CLIENT                        = SY-MANDT
     ID                            = 'F03'
     LANGUAGE                      = SY-LANGU
     NAME                          =  V_NAMEID
     OBJECT                        = 'EKPO'
*   ARCHIVE_HANDLE                = 0
*   LOCAL_CAT                     = ' '
* IMPORTING
*   HEADER                        =
   TABLES
     LINES                         = T_LINES[]
  EXCEPTIONS
    ID                            = 1
    LANGUAGE                      = 2
    NAME                          = 3
    NOT_FOUND                     = 4
    OBJECT                        = 5
    REFERENCE_CHECK               = 6
    WRONG_ACCESS_TO_ARCHIVE       = 7
    OTHERS                        = 8
           .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
        CLEAR WA_TLINES.
      ELSE.
        CALL FUNCTION 'CONVERT_ITF_TO_ASCII'
          EXPORTING
            FORMATWIDTH       = 132
          IMPORTING
            C_DATATAB         = LT_TEXT
          TABLES
            ITF_LINES         = T_LINES
          EXCEPTIONS
            INVALID_TABLETYPE = 1
            OTHERS            = 2.
* New code to be insert here.
        LOOP AT LT_TEXT INTO WA_TEXT.
          <L_S_DATA>-ZZHDRNOTES =  WA_TEXT.
          CLEAR:WA_TEXT.
        ENDLOOP.


      ENDIF.
****************************
      V_NAMEID2 = V_NAMEID.
      CALL FUNCTION 'READ_TEXT'
      EXPORTING
       CLIENT                        = SY-MANDT
        ID                            = 'F01'
        LANGUAGE                      = SY-LANGU
        NAME                          =  V_NAMEID
        OBJECT                        = 'EKPO'
*   ARCHIVE_HANDLE                = 0
*   LOCAL_CAT                     = ' '
* IMPORTING
*   HEADER                        =
      TABLES
        LINES                         = T_LINES2[]
     EXCEPTIONS
       ID                            = 1
       LANGUAGE                      = 2
       NAME                          = 3
       NOT_FOUND                     = 4
       OBJECT                        = 5
       REFERENCE_CHECK               = 6
       WRONG_ACCESS_TO_ARCHIVE       = 7
       OTHERS                        = 8
              .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
        CLEAR WA_TLINES2.
      ELSE.
        CALL FUNCTION 'CONVERT_ITF_TO_ASCII'
          EXPORTING
            FORMATWIDTH       = 132
          IMPORTING
            C_DATATAB         = LT_TEXT
          TABLES
            ITF_LINES         = T_LINES2
          EXCEPTIONS
            INVALID_TABLETYPE = 1
            OTHERS            = 2.
* New code to be insert here.
        LOOP AT LT_TEXT INTO WA_TEXT.
          <L_S_DATA>-ZZHDRNOTES =  WA_TEXT.
          CLEAR:WA_TEXT.
        ENDLOOP.
*      LOOP AT T_LINES2 INTO WA_TLINES2.
*        CLEAR VTEXT2.
*        IF SY-TABIX = 1.
*          VTEXT1 = WA_TLINES2-TDLINE.
*        ELSE.
*          VTEXT2 = WA_TLINES2-TDLINE.
*        ENDIF.
*      ENDLOOP.
*
*      CONCATENATE VTEXT1 VTEXT2 INTO VTEXT.
*      REPLACE ALL OCCURRENCES OF  '<(>&<)>' IN VTEXT WITH '&'.
*      <L_S_DATA>-ZZITMTXT = VTEXT.
*      CLEAR VTEXT1.
      ENDIF.
**************************

*BOI by PKATRAGADDA
*Fetching the header notes information
      REFRESH: T_LINES3[].
      V_NAMEID3 = <L_S_DATA>-EBELN .

      CALL FUNCTION 'READ_TEXT'
      EXPORTING
       CLIENT                        = SY-MANDT
        ID                            = 'F02'
        LANGUAGE                      = SY-LANGU
        NAME                          =  V_NAMEID3
        OBJECT                        = 'EKKO'
*   ARCHIVE_HANDLE                = 0
*   LOCAL_CAT                     = ' '
* IMPORTING
*   HEADER                        =
      TABLES
        LINES                         = T_LINES3[]
     EXCEPTIONS
       ID                            = 1
       LANGUAGE                      = 2
       NAME                          = 3
       NOT_FOUND                     = 4
       OBJECT                        = 5
       REFERENCE_CHECK               = 6
       WRONG_ACCESS_TO_ARCHIVE       = 7
       OTHERS                        = 8
              .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
        CLEAR WA_TLINES3.
      ELSE.
        CALL FUNCTION 'CONVERT_ITF_TO_ASCII'
          EXPORTING
            FORMATWIDTH       = 132
          IMPORTING
            C_DATATAB         = LT_TEXT
          TABLES
            ITF_LINES         = T_LINES3
          EXCEPTIONS
            INVALID_TABLETYPE = 1
            OTHERS            = 2.
* New code to be insert here.
        LOOP AT LT_TEXT INTO WA_TEXT.
          <L_S_DATA>-ZZHDRNOTES =  WA_TEXT.
          CLEAR:WA_TEXT.
        ENDLOOP.
*      LOOP AT T_LINES3 INTO WA_TLINES3.
*        HNTEXT1 = WA_TLINES3-TDLINE.
*        IF SY-TABIX = 1.
*          HNTEXT = HNTEXT1.
*        ELSE.
*          IF STRLEN( HNTEXT ) = 180.
*            EXIT.
*          ELSE.
*            CONCATENATE HNTEXT HNTEXT1 INTO HNTEXT SEPARATED BY SPACE.
*          ENDIF.
*        ENDIF.
*        CLEAR HNTEXT1.
*      ENDLOOP.

*      CONCATENATE ztext1 ztext2 INTO ztext.  "(-) PKATRAGADDA
*      REPLACE ALL OCCURRENCES OF  '<(>&<)>' IN HNTEXT WITH '&'.
*      <L_S_DATA>-ZZHDRNOTES = HNTEXT.
*      CLEAR HNTEXT1.
      ENDIF.

*EOI by PKATRAGADDA

**************************

*BOI by PKATRAGADDA
*Fetching the Expediting / Inspection Notes information
*
      CALL FUNCTION 'READ_TEXT'
  EXPORTING
  CLIENT                        = SY-MANDT
  ID                            = 'F18'
  LANGUAGE                      = SY-LANGU
  NAME                          =  V_NAMEID3
  OBJECT                        = 'EKKO'
*   ARCHIVE_HANDLE                = 0
*   LOCAL_CAT                     = ' '
* IMPORTING
*   HEADER                        =
  TABLES
  LINES                         = T_LINES4[]
  EXCEPTIONS
  ID                            = 1
  LANGUAGE                      = 2
  NAME                          = 3
  NOT_FOUND                     = 4
  OBJECT                        = 5
  REFERENCE_CHECK               = 6
  WRONG_ACCESS_TO_ARCHIVE       = 7
  OTHERS                        = 8
    .
      IF SY-SUBRC <> 0.
* Implement suitable error handling here
        CLEAR WA_TLINES.
      ELSE.
        CALL FUNCTION 'CONVERT_ITF_TO_ASCII'
          EXPORTING
            FORMATWIDTH       = 132
          IMPORTING
            C_DATATAB         = LT_TEXT
          TABLES
            ITF_LINES         = T_LINES4
          EXCEPTIONS
            INVALID_TABLETYPE = 1
            OTHERS            = 2.
* New code to be insert here.
        LOOP AT LT_TEXT INTO WA_TEXT.
          <L_S_DATA>-ZZHDRE_INS_NOTES =  WA_TEXT.
          CLEAR:WA_TEXT.
        ENDLOOP.

      ENDIF.
*EOI by PKATRAGADDA
**************************

      CLEAR WA_EKPO.
      READ TABLE T_EKPO INTO WA_EKPO
                        WITH KEY ZEBELN = <L_S_DATA>-EBELN  ZEBELP = <L_S_DATA>-EBELP
                        BINARY SEARCH.
      <L_S_DATA>-ZZADRNR = WA_EKPO-ZADRNR.
      IF SY-SUBRC = 0.

        CLEAR WA_ADRC.
        READ TABLE T_ADRC INTO WA_ADRC
                          WITH KEY ZADDRNUMBER = <L_S_DATA>-ZZADRNR
                          BINARY SEARCH.
        IF SY-SUBRC = 0.
          <L_S_DATA>-ZZADRNR = WA_ADRC-ZADDRNUMBER.
          <L_S_DATA>-ZZCITY1 = WA_ADRC-ZCITY1.
          <L_S_DATA>-ZZPOST_CODE1 = WA_ADRC-ZPOST_CODE1.
          <L_S_DATA>-ZZSTREET = WA_ADRC-ZSTREET.
          <L_S_DATA>-ZZREGION = WA_ADRC-ZREGION.
        ENDIF.
      ENDIF.
      READ TABLE IT_EKETPDM INTO WA_EKETPDM WITH KEY EBELN = <L_S_DATA>-EBELN.

      IF SY-SUBRC = 0.
        <L_S_DATA>-ESTKZ = WA_EKETPDM-ESTKZ.

      ENDIF.
    ENDLOOP.


  endmethod.


  METHOD LIS_03_BF.

    FIELD-SYMBOLS: <l_s_data> TYPE mc03bf0 .
*
    TYPES : BEGIN OF ty_mseg,
             mblnr TYPE mseg-mblnr,
             mjahr TYPE mseg-mjahr,
             zeile TYPE mseg-zeile,
             insmk TYPE mseg-insmk,
             waers TYPE mseg-waers,
             bualt TYPE mseg-bualt,
             meins TYPE mseg-meins,
             bpmng TYPE mseg-bpmng,
             bprme TYPE mseg-bprme,
             lfbja TYPE mseg-lfbja,
             sjahr TYPE mseg-sjahr,
             smbln TYPE mseg-smbln,
             elikz TYPE mseg-elikz,
             equnr TYPE mseg-equnr,
             xruej TYPE mseg-xruej,
             belum TYPE mseg-belum,
             pbamg TYPE mseg-pbamg,
             kzstr TYPE mseg-kzstr,
             umsok TYPE mseg-umsok,
             ubnum TYPE mseg-ubnum,
             bstmg TYPE mseg-bstmg,
             bstme TYPE mseg-bstme,
             pprctr TYPE mseg-pprctr,
             lbkum TYPE mseg-lbkum,
             salk3 TYPE mseg-salk3,
             fkber TYPE mseg-fkber,
             txjcd TYPE mseg-txjcd,
             ematn TYPE mseg-ematn,
             llief TYPE mseg-llief,
             xmacc TYPE mseg-xmacc,
             ed_user TYPE mseg-/bev2/ed_user,
             ed_aedat TYPE mseg-/bev2/ed_aedat,
            END OF ty_mseg.
*
    DATA: l_mseg TYPE ty_mseg,
          l_cond(72) TYPE c.
*
    DATA : t_mseg TYPE TABLE OF  ty_mseg,
           t_itab LIKE TABLE OF l_cond.
*
    REFRESH t_itab.
    APPEND 'MBLNR = C_T_DATA-MBLNR'  TO  t_itab.
    APPEND 'AND MJAHR = C_T_DATA-MJAHR'  TO t_itab.
    APPEND 'AND ZEILE = C_T_DATA-ZEILE'  TO t_itab.


*
    SELECT
    mblnr
    mjahr
    zeile
    insmk
    waers
    bualt
    meins
    bpmng
    bprme
    lfbja
    sjahr
    smbln
    elikz
    equnr
    xruej
    belum
    pbamg
    kzstr
    umsok
    ubnum
    bstmg
    bstme
    pprctr
    lbkum
    salk3
    fkber
    txjcd
    ematn
    llief
    xmacc
    /bev2/ed_user
    /bev2/ed_aedat
    FROM mseg
    INTO TABLE t_mseg
    FOR ALL ENTRIES IN c_t_data
    WHERE (t_itab).
*
    SORT t_mseg BY MBLNR MJAHR ZEILE.

*
    LOOP AT c_t_data  ASSIGNING <l_s_data>.

      READ TABLE t_mseg INTO l_mseg WITH KEY MBLNR = <l_s_data>-MBLNR MJAHR = <l_s_data>-MJAHR ZEILE = <l_s_data>-ZEILE BINARY SEARCH.

      IF sy-subrc = 0.

        <l_s_data>-ZZINSMK = l_mseg-INSMK.
        <l_s_data>-ZZWAERS = l_mseg-WAERS.
        <l_s_data>-ZZBUALT = l_mseg-BUALT.
        <l_s_data>-ZZMEINS = l_mseg-MEINS.
        <l_s_data>-ZZBPMNG = l_mseg-BPMNG.
        <l_s_data>-ZZBPRME = l_mseg-BPRME.
        <l_s_data>-ZZLFBJA = l_mseg-LFBJA.
        <l_s_data>-ZZSJAHR = l_mseg-SJAHR.
        <l_s_data>-ZZSMBLN = l_mseg-SMBLN.
        <l_s_data>-ZZELIKZ = l_mseg-ELIKZ.
        <l_s_data>-ZZEQUNR = l_mseg-EQUNR.
        <l_s_data>-ZZXRUEJ = l_mseg-XRUEJ.
        <l_s_data>-ZZBELUM = l_mseg-BELUM.
        <l_s_data>-ZZPBAMG = l_mseg-PBAMG.
        <l_s_data>-ZZKZSTR = l_mseg-KZSTR.
        <l_s_data>-ZZUMSOK = l_mseg-UMSOK.
        <l_s_data>-ZZUBNUM = l_mseg-UBNUM.
        <l_s_data>-ZZBSTMG = l_mseg-BSTMG.
        <l_s_data>-ZZBSTME = l_mseg-BSTME.
        <l_s_data>-ZZPPRCTR = l_mseg-PPRCTR.
        <l_s_data>-ZZLBKUM = l_mseg-LBKUM.
        <l_s_data>-ZZSALK3 = l_mseg-SALK3.
        <l_s_data>-ZZFKBER = l_mseg-FKBER.
        <l_s_data>-ZZTXJCD = l_mseg-TXJCD.
        <l_s_data>-ZZEMATN = l_mseg-EMATN.
        <l_s_data>-ZZLLIEF = l_mseg-LLIEF.
        <l_s_data>-ZZXMACC = l_mseg-XMACC.
        <l_s_data>-ZZED_USER = l_mseg-ED_USER.
        <l_s_data>-ZZED_AEDAT = l_mseg-ED_AEDAT.

      ENDIF.

      CLEAR: l_mseg.

    ENDLOOP.

  ENDMETHOD.


  method LIS_06_INV.
    FIELD-SYMBOLS: <l_s_data> TYPE mc06m_0itm.
*
    TYPES : BEGIN OF ty_temp,
              bukrs TYPE bseg-bukrs,
              belnr TYPE bseg-belnr,
              gjahr TYPE bseg-gjahr,
              awkey TYPE bkpf-awkey,
              zlspr TYPE bseg-zlspr,
             END OF ty_temp.

    TYPES : BEGIN OF ty_bkpf,
              bukrs TYPE bseg-bukrs,
              belnr TYPE bseg-belnr,
              gjahr TYPE bseg-gjahr,
              awkey TYPE bkpf-awkey,
             END OF ty_bkpf.
    DATA : t_temp TYPE TABLE OF ty_temp,
           l_temp TYPE ty_temp,
           t_bkpf TYPE TABLE OF ty_temp,
           l_bkpf TYPE ty_temp,
           t_bseg TYPE TABLE OF ty_temp,
           l_bseg TYPE ty_temp.
*
*  LOOP AT c_t_data  ASSIGNING <l_s_data>.
*
*    l_temp-bukrs = <l_s_data>-bukrs.
*    l_temp-belnr = <l_s_data>-belnr.
*    l_temp-gjahr = <l_s_data>-gjahr.
*    CONCATENATE <l_s_data>-belnr <l_s_data>-gjahr INTO  l_temp-awkey.
*
*    APPEND l_temp TO t_temp.
*
*  ENDLOOP.

*  DELETE ADJACENT DUPLICATES FROM t_temp COMPARING bukrs belnr gjahr .
*
*
*  SELECT bukrs belnr gjahr awkey FROM bkpf INTO CORRESPONDING FIELDS OF TABLE t_bkpf
*    FOR ALL ENTRIES IN t_temp
*    WHERE bukrs = t_temp-bukrs
*    AND gjahr = t_temp-gjahr
*    AND awkey = t_temp-awkey.
*
*  SELECT bukrs belnr gjahr zlspr FROM bseg INTO CORRESPONDING FIELDS OF TABLE t_bseg
*   FOR ALL ENTRIES IN t_bkpf
*   WHERE bukrs = t_bkpf-bukrs
*   AND belnr = t_bkpf-belnr
*   AND gjahr = t_bkpf-gjahr
*   and BSCHL = '31' .
*
*  LOOP AT c_t_data  ASSIGNING <l_s_data>.
*
*    DATA: l_awkey TYPE bkpf-awkey.
*
*    CONCATENATE <l_s_data>-belnr <l_s_data>-gjahr INTO l_awkey.
*
*    READ TABLE t_bkpf INTO l_bkpf WITH KEY bukrs = <l_s_data>-bukrs   gjahr = <l_s_data>-gjahr awkey = l_awkey.
*
*    IF sy-subrc = 0.
*
*      READ TABLE t_bseg INTO l_bseg WITH KEY bukrs = <l_s_data>-bukrs  belnr = l_bkpf-belnr gjahr = <l_s_data>-gjahr.
*
*      IF sy-subrc = 0.
*
*        <l_s_data>-zlspr = l_bseg-zlspr.
*
*      ENDIF.
*
*    ENDIF.
*
*    CLEAR: l_bseg, l_bkpf, l_awkey.

*  ENDLOOP.
  endmethod.


  method MATERIAL_TEXT.
    FIELD-SYMBOLS: <l_s_data> TYPE BIW_MAKT_S.

    DATA: d_c_t_data TYPE TABLE OF BIW_MAKT_S,
          var_name(70) TYPE c,
          ztext(180) TYPE c.


    DATA: t_lines TYPE TABLE OF tline,
          wa_tlines TYPE tline.

    d_c_t_data[] = c_t_data[].

    LOOP AT c_t_data  ASSIGNING <l_s_data>.

      REFRESH t_lines[].
      var_name = <l_s_data>-MATNR.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client                        = sy-mandt
          id                            = 'GRUN'
          language                      = sy-langu
          name                          =  var_name
          object                        = 'MATERIAL'
*       ARCHIVE_HANDLE                = 0
*       LOCAL_CAT                     = ' '
*     IMPORTING
*       HEADER                        =
        TABLES
          lines                         = t_lines[]
        EXCEPTIONS
          id                            = 1
          language                      = 2
          name                          = 3
          not_found                     = 4
          object                        = 5
          reference_check               = 6
          wrong_access_to_archive       = 7
          OTHERS                        = 8.

      IF sy-subrc <> 0.
* Implement suitable error handling here
        CLEAR wa_tlines.
      ELSE.
        Loop AT t_lines INTO wa_tlines.
          IF sy-tabix = 1.
            ztext = wa_tlines-tdline.
          ENDIF.
        ENDLOOP.
        <l_s_data>-ZZTEXTLG = ztext.
        CLEAR ztext.
      ENDIF.



    ENDLOOP.
  endmethod.


  method PLANT_ATTR.
    FIELD-SYMBOLS: <l_s_data> TYPE BIW_T001WS .

    TYPES : BEGIN OF ty_T001K,
             BWKEY TYPE T001K-BWKEY,
             BUKRS TYPE T001K-BUKRS,
            END OF ty_T001K.

    DATA: l_T001K TYPE ty_T001K,
          l_cond(72) TYPE c.

    DATA : t_T001K TYPE TABLE OF  ty_T001K,
           t_itab LIKE TABLE OF l_cond.


    REFRESH T_ITAB.
    APPEND 'BWKEY = C_T_DATA-WERKS'  TO  T_ITAB.

    SELECT BWKEY BUKRS FROM T001K
    INTO TABLE t_T001K
    FOR ALL ENTRIES IN c_t_data
    WHERE (t_itab).

    SORT t_T001K  BY BWKEY.

    LOOP AT c_t_data  ASSIGNING <l_s_data>.

      READ TABLE t_T001K  INTO l_T001K  WITH KEY BWKEY = <l_s_data>-WERKS BINARY SEARCH.

      IF sy-subrc = 0.

        <L_S_DATA>-ZZBUKRS = l_T001K-BUKRS.

      ENDIF.

      CLEAR:  l_T001K.

    ENDLOOP.
  endmethod.


  method ZFIGL_BKPF_PARK.
    FIELD-SYMBOLS: <L_S_DATA> TYPE ZOXD300029.
    DATA: T_BKPF TYPE TABLE OF BKPF,
          S_BKPF LIKE LINE OF T_BKPF,
          S_VBSEGK TYPE VBSEGK-LIFNR,
          LW_WIID TYPE SWW_WIID,
          LT_CONT TYPE TABLE OF SWR_CONT,
          S_CONT TYPE SWR_CONT,
          LT_OBJTYPE TYPE SWOTOBJID-OBJTYPE,
          LW_OBJKEY   TYPE SWOTOBJID-OBJKEY,
          LT_WORKLIST TYPE SWRTWIHDR,
          LW_WORKLIST TYPE SWR_WIHDR,
          LV_TOP TYPE SWWWIHEAD-WI_ID,
          T_SWWWIHEAD TYPE  TABLE OF SWWWIHEAD,
          L_SWWWIHEAD TYPE SWWWIHEAD.

    LOOP AT C_T_DATA  ASSIGNING <L_S_DATA>.

*Reading vbsegk - Vendor Information for only Parked Items
      IF <L_S_DATA>-BSTAT = 'V'.

        SELECT SINGLE LIFNR FROM VBSEGK INTO S_VBSEGK
                             WHERE BUKRS = <L_S_DATA>-BUKRS AND
                   BELNR = <L_S_DATA>-BELNR AND
                   GJAHR = <L_S_DATA>-GJAHR.
        IF SY-SUBRC = 0.

          <L_S_DATA>-ZZLIFNR = S_VBSEGK.

        ENDIF.

      ENDIF.


      SELECT SINGLE * FROM BKPF INTO S_BKPF
             WHERE BUKRS = <L_S_DATA>-BUKRS AND
                   BELNR = <L_S_DATA>-BELNR AND
                   GJAHR = <L_S_DATA>-GJAHR.

      IF SY-SUBRC = '0'.
        IF <L_S_DATA>-BLART = 'RE' OR <L_S_DATA>-BLART = 'ZR' .
          LT_OBJTYPE = 'BUS2081'.
          LW_OBJKEY = S_BKPF-AWKEY+0(14).
        ELSE.
          LT_OBJTYPE = 'FIPP'.
          LW_OBJKEY+0(4) = <L_S_DATA>-BUKRS.
          LW_OBJKEY+4(10) = <L_S_DATA>-BELNR.
          LW_OBJKEY+14(4) = <L_S_DATA>-GJAHR.
        ENDIF.
* ENTRY DATE
        <L_S_DATA>-ZZCPUDT = S_BKPF-CPUDT.
      ENDIF.

      CALL FUNCTION 'SAP_WAPI_WORKITEMS_TO_OBJECT'
        EXPORTING
          OBJTYPE                  = LT_OBJTYPE
          OBJKEY                   = LW_OBJKEY
          TOP_LEVEL_ITEMS          = ' '
          SELECTION_STATUS_VARIANT = '0000'
        TABLES
          WORKLIST                 = LT_WORKLIST.

      SORT LT_WORKLIST BY WI_CD WI_CT ASCENDING.

      IF <L_S_DATA>-BLART = 'RE' OR <L_S_DATA>-BLART = 'ZR'.
        READ TABLE LT_WORKLIST INTO LW_WORKLIST WITH KEY WI_RH_TASK = 'WS02000003' WI_STAT = 'COMPLETED'.
        IF SY-SUBRC = 0.
          LV_TOP = LW_WORKLIST-WI_ID.
*Parking Date
          <L_S_DATA>-ZZPDATE = LW_WORKLIST-WI_CD.
*Parked BY
          <L_S_DATA>-ZZUPARK = LW_WORKLIST-WI_CREATOR+2.

*Entered By
          SELECT * FROM SWWWIHEAD
                              INTO TABLE T_SWWWIHEAD
                WHERE WI_TYPE   = 'W' AND
                     WI_RH_TASK = 'TS02000010' AND
                     TOP_WI_ID  = LV_TOP.

          IF SY-SUBRC = 0.
            SORT T_SWWWIHEAD BY WI_CD DESCENDING WI_CT DESCENDING.
            READ TABLE T_SWWWIHEAD  INTO L_SWWWIHEAD INDEX 1.
            <L_S_DATA>-ZZUPOST = L_SWWWIHEAD-WI_AAGENT.
          ENDIF.
        ENDIF.
      ELSE.
        READ TABLE LT_WORKLIST INTO LW_WORKLIST WITH KEY WI_RH_TASK = 'WS02000002' WI_STAT = 'COMPLETED'.
        IF SY-SUBRC = 0.
          LV_TOP = LW_WORKLIST-WI_ID.
*Parking Date
          <L_S_DATA>-ZZPDATE = LW_WORKLIST-WI_CD.
*Parked BY
          <L_S_DATA>-ZZUPARK = LW_WORKLIST-WI_CREATOR+2.

*Entered By
          SELECT *  FROM SWWWIHEAD
                                 INTO TABLE T_SWWWIHEAD
                   WHERE WI_TYPE = 'W' AND
                        WI_RH_TASK = 'TS02000013' AND
                       TOP_WI_ID = LV_TOP.
          IF SY-SUBRC = 0.

            SORT T_SWWWIHEAD BY WI_CD WI_CT ASCENDING.

            LOOP AT T_SWWWIHEAD INTO L_SWWWIHEAD.

              LW_WIID = L_SWWWIHEAD-WI_ID.
              REFRESH: LT_CONT.
              CLEAR:S_CONT .
              CALL FUNCTION 'SAP_WAPI_READ_CONTAINER'
                EXPORTING
                  WORKITEM_ID      = LW_WIID
                TABLES
                  SIMPLE_CONTAINER = LT_CONT.
              READ TABLE LT_CONT INTO S_CONT WITH KEY ELEMENT = 'DECISIONTYPE'.
              IF SY-SUBRC = 0.
                IF S_CONT-VALUE = 'POST'.
                  <L_S_DATA>-ZZUPOST = L_SWWWIHEAD-WI_AAGENT.
                ENDIF.
              ENDIF.

            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.


* Direct posts.
      IF <L_S_DATA>-ZZUPOST IS INITIAL.

        <L_S_DATA>-ZZUPOST =  S_BKPF-USNAM.

      ENDIF.

      CLEAR: LT_OBJTYPE, LW_OBJKEY, S_BKPF, LV_TOP, S_VBSEGK, L_SWWWIHEAD.
      REFRESH: LT_WORKLIST, T_SWWWIHEAD.
      "    UNASSIGN: <SWWWIHEAD>.
    ENDLOOP.
  endmethod.
ENDCLASS.
