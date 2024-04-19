*----------------------------------------------------------------------*
* Report Name: ZXRSAU02
* Author:	     KBANERJEE-Kaushiki Banerjee
* Date:	       November 20th,2018
*
* Logical Database: NA
* SAPScript name:   NA
* Application Area: BW
* Description:  This enhancement is used to fill the below additional
*               fields added to datasource  BWE_QMEL
*               System status
*               User status
*               Created by
*               Detection Method
*               Object Part
*               Activity
************************************************************************
*                           Modification Log                           *
* Changed On   Changed By  CTS         Description                     *
* -----------  ----------  ----------  ------------------------------- *
* 20-NOV-2018  KBANERJEE    D30K929264 CHG0127010:Initial development  *
* 27-DEC-2018  KBANERJEE   D30K929405,                                 *
*                          D30K929403 CHG0127010:Changes post UT       *
* 31-May-2019  SUBRAMS2    D30K929892 CHG0148345: ZOBJPART field update*
*                          Enhance 0FUNCT_LOC_ATTR field ZEQUNR        *
************************************************************************
  TYPES:BEGIN OF lty_status_text,
       stat TYPE j_istat,
       spras TYPE spras,
       desc  TYPE j_txt04,
    END OF lty_status_text,
    BEGIN OF lty_ernam,
       qmnum TYPE qmnum,
       ernam TYPE ernam,
    END OF lty_ernam,
    BEGIN OF lty_cdgrp,
      qmnum  TYPE qmnum,
      fenum  TYPE felfd,
      otgrp  TYPE otgrp,
     END OF lty_cdgrp,
     BEGIN OF lty_activity,
       qmnum TYPE qmnum,
       manum TYPE aknum,
       fenum TYPE felfd,
      END OF lty_activity,
**--START OF CHANGES FOR CHG0127010 BY KBANERJEE
      BEGIN OF lty_wrkcntr,
        bname TYPE xubname,
        parid TYPE memoryid,
        parva TYPE xuvalue,
        END OF lty_wrkcntr.
**--END OF CHANGES FOR CHG0127010 BY KBANERJEE
***--Start OF CHANGES FOR CHG0148345 BY SUBRAMS2
TYPES: Begin of lty_iloa,
       iloan TYPE iloan,
       tplnr TYPE tplnr,
       END OF lty_iloa,

       BEGIN OF lty_equz,
         equnr TYPE equnr,
         datbi TYPE datbi,
         iloan TYPE iloan,
       END OF lty_equz.
***--End OF CHANGES FOR CHG0148345 BY SUBRAMS2
  DATA:lt_qmel_data    TYPE STANDARD TABLE OF bwe_qmel         INITIAL SIZE 0,
       lt_temp_data    TYPE STANDARD TABLE OF bwe_qmel         INITIAL SIZE 0,
       lt_objnr        TYPE STANDARD TABLE OF jsto_pre         INITIAL SIZE 0,
       lt_status       TYPE STANDARD TABLE OF jest             INITIAL SIZE 0,
       lt_sys_status   TYPE STANDARD TABLE OF jest             INITIAL SIZE 0,
       lt_usr_status   TYPE STANDARD TABLE OF jest             INITIAL SIZE 0,
       lt_sysstat_text TYPE STANDARD TABLE OF lty_status_text  INITIAL SIZE 0,
       lt_usrstat_text TYPE STANDARD TABLE OF lty_status_text  INITIAL SIZE 0,
       lt_ernam        TYPE STANDARD TABLE OF lty_ernam        INITIAL SIZE 0,
       lt_ernam_tmp    TYPE STANDARD TABLE OF lty_ernam        INITIAL SIZE 0,
       lt_cdgrp        TYPE STANDARD TABLE OF lty_cdgrp        INITIAL SIZE 0,
       lt_cdgrp1       TYPE STANDARD TABLE OF lty_cdgrp        INITIAL SIZE 0,
       lt_wrkcntr      TYPE STANDARD TABLE OF lty_wrkcntr      INITIAL SIZE 0,
       lt_activity     TYPE STANDARD TABLE OF lty_activity     INITIAL SIZE 0,
***--Start OF CHANGES FOR CHG0148345 BY SUBRAMS2
       lt_func_loc     TYPE STANDARD TABLE OF BWE_IFLOT        INITIAL SIZE 0,
       lt_iloa         TYPE STANDARD TABLE OF lty_iloa         INITIAL SIZE 0,
       lt_equz         TYPE STANDARD TABLE OF lty_equz         INITIAL SIZE 0.
***--End OF CHANGES FOR CHG0148345 BY SUBRAMS2
  DATA: ls_qmel_data    TYPE bwe_qmel,
        ls_status       TYPE jest,
        ls_ernam        TYPE lty_ernam,
        ls_cdgrp        TYPE lty_cdgrp,
        ls_activity     TYPE lty_activity,
        ls_objnr        TYPE jsto_pre,
        ls_wrkcntr      TYPE lty_wrkcntr,
        ls_usrstat_text TYPE lty_status_text,
        ls_sysstat_text TYPE lty_status_text,
***--Start OF CHANGES FOR CHG0148345 BY SUBRAMS2
        ls_func_loc     TYPE BWE_IFLOT,
        ls_iloa         TYPE lty_iloa,
        ls_equz         TYPE lty_equz.
***--End OF CHANGES FOR CHG0148345 BY SUBRAMS2
  CONSTANTS:lc_sys   TYPE flag     VALUE 'I',
            lc_x     TYPE flag     VALUE 'X',
            lc_1     TYPE felfd    VALUE '0001',
            lc_usr   TYPE flag     VALUE 'E',
            lc_rmdm  TYPE otgrp    VALUE 'RM-DM',
            lc_del   TYPE otgrp    VALUE 'DEL',
            lc_obj   TYPE otgrp    VALUE 'OBJ',
**--START OF CHANGES FOR CHG0127010 BY KBANERJEE
            lc_vap   TYPE memoryid VALUE 'VAP',
**--END OF CHANGES FOR CHG0127010 BY KBANERJEE
            lc_act   TYPE char3    VALUE 'ACT'.
  FIELD-SYMBOLS:<lfs_qmel_data> TYPE bwe_qmel,
                 <lfs_func_loc> TYPE bwe_iflot. "CHG0148345 SUBRAMS2
  DATA lv_cdgrp_recs TYPE i.
  DATA: lv_tabix     TYPE i.                    "CHG0148345 SUBRAMS2

  CASE i_datasource.
    WHEN '0NOTIFICATN_ATTR'.
      CLEAR lt_qmel_data.
      lt_qmel_data[] = i_t_data[].
*Fetch user and system statuses
      LOOP AT lt_qmel_data INTO ls_qmel_data.
        ls_objnr-objnr = ls_qmel_data-objnr.
        APPEND ls_objnr TO lt_objnr.
        CLEAR:ls_objnr,ls_qmel_data.
      ENDLOOP.
      IF lt_objnr IS NOT INITIAL.
        CALL FUNCTION 'STATUS_READ_MULTI'
          EXPORTING
            client      = sy-mandt
            only_active = lc_x
          TABLES
            objnr_tab   = lt_objnr
            status      = lt_status.
        IF lt_status IS NOT INITIAL.
*Fetch user status description from TJ30T
          SORT lt_status BY stat.
          lt_usr_status = lt_status.
          DELETE lt_usr_status WHERE stat(1) NE lc_usr.
          DELETE ADJACENT DUPLICATES FROM lt_sys_status COMPARING stat.
          IF lt_usr_status IS NOT INITIAL.
            SELECT estat
                   spras
                   txt04
              FROM tj30t
              INTO TABLE lt_usrstat_text
              FOR ALL ENTRIES IN lt_usr_status
              WHERE stsma = 'UG_NO_01'
                AND estat = lt_usr_status-stat
                AND spras = sy-langu.
            IF sy-subrc IS INITIAL.
              SORT lt_usrstat_text BY stat.
            ENDIF.
          ENDIF.
*Fetch system status text from TJ02T
          lt_sys_status = lt_status.
          DELETE lt_sys_status WHERE stat(1) NE lc_sys.
          DELETE ADJACENT DUPLICATES FROM lt_sys_status COMPARING stat.
          IF lt_sys_status IS NOT INITIAL.
            SELECT istat
                   spras
                   txt04
              FROM tj02t
              INTO TABLE lt_sysstat_text
              FOR ALL ENTRIES IN lt_sys_status
              WHERE istat = lt_sys_status-stat
                AND spras = sy-langu.
            IF sy-subrc IS INITIAL.
              SORT lt_sysstat_text BY stat.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*Fetch created by from QMEL
      lt_temp_data = lt_qmel_data.
      SORT lt_temp_data BY qmnum.
      DELETE ADJACENT DUPLICATES FROM lt_temp_data COMPARING qmnum.
      IF lt_temp_data IS NOT INITIAL.
        SELECT  qmnum ernam
          FROM  qmel
          INTO  TABLE lt_ernam
          FOR ALL ENTRIES IN lt_temp_data
          WHERE qmnum = lt_temp_data-qmnum.
        IF sy-subrc IS INITIAL .
          SORT lt_ernam BY qmnum.
        ENDIF.
*Fetch Code group details from QMFE
        SELECT qmnum fenum otgrp
          FROM qmfe
          INTO TABLE lt_cdgrp
          FOR ALL ENTRIES IN lt_temp_data
          WHERE qmnum = lt_temp_data-qmnum.
**--START OF CHANGES FOR CHG0127010 BY KBANERJEE
        "and fenum  = lc_1.
**--END OF CHANGES FOR CHG0127010 BY KBANERJEE
        IF sy-subrc IS INITIAL.
          SORT lt_cdgrp BY qmnum fenum.
        ENDIF.
      ENDIF.
*Fetch Activity details from QMMA
      SELECT qmnum manum fenum
        FROM qmma
        INTO TABLE lt_activity
        FOR ALL ENTRIES IN lt_temp_data
        WHERE qmnum = lt_temp_data-qmnum.
      IF sy-subrc IS INITIAL.
        SORT lt_activity BY qmnum fenum.
      ENDIF.
*Fetch the Workcenter details from USR05
      IF lt_ernam IS NOT INITIAL.
        lt_ernam_tmp = lt_ernam.
        SORT lt_ernam_tmp BY ernam.
        DELETE ADJACENT DUPLICATES FROM lt_ernam_tmp COMPARING ernam.
        SELECT bname parid parva
          FROM usr05
          INTO TABLE lt_wrkcntr
             FOR ALL ENTRIES IN lt_ernam_tmp
          WHERE bname = lt_ernam_tmp-ernam
            AND parid = lc_vap.
        IF sy-subrc = 0.
          SORT lt_wrkcntr BY  bname.
        ENDIF.
      ENDIF.
*Change data in DS
      SORT :lt_qmel_data BY objnr,
            lt_status    BY objnr.
      LOOP AT lt_status INTO ls_status.
        READ TABLE lt_qmel_data ASSIGNING <lfs_qmel_data>
                                WITH KEY objnr = ls_status-objnr
                                BINARY SEARCH .
        IF sy-subrc IS INITIAL AND <lfs_qmel_data> IS ASSIGNED.
          READ TABLE lt_usrstat_text INTO ls_usrstat_text
                                     WITH KEY stat = ls_status-stat
                                     BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            <lfs_qmel_data>-zbwusesta = ls_usrstat_text-desc.
          ENDIF.
          READ TABLE lt_sysstat_text INTO ls_sysstat_text
                                     WITH KEY stat = ls_status-stat
                                     BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            IF <lfs_qmel_data>-zbwsyssta IS INITIAL.
              <lfs_qmel_data>-zbwsyssta = ls_sysstat_text-desc.
            ELSE.
              CONCATENATE <lfs_qmel_data>-zbwsyssta ls_sysstat_text-desc
                     INTO <lfs_qmel_data>-zbwsyssta
             SEPARATED BY space.
            ENDIF."endif <lfs_qmel_data>-zbwsyssta IS INITIAL.
          ENDIF."endif sy-subrc check on lt_sysstat_text
          AT END OF objnr.
            READ TABLE lt_ernam INTO ls_ernam
                                WITH KEY qmnum = <lfs_qmel_data>-qmnum
                                BINARY SEARCH .
            IF sy-subrc IS INITIAL.
              <lfs_qmel_data>-zernam = ls_ernam-ernam.
            ENDIF.
            READ TABLE lt_cdgrp INTO ls_cdgrp
                                WITH KEY qmnum = <lfs_qmel_data>-qmnum
                                BINARY SEARCH.
            IF sy-subrc IS INITIAL.
***--START OF CHANGES FOR CHG0148345 BY SUBRAMS2
*              lt_cdgrp1 = lt_cdgrp.
*              DELETE lt_cdgrp1 WHERE qmnum NE <lfs_qmel_data>-qmnum.
*              DESCRIBE TABLE lt_cdgrp1 LINES lv_cdgrp_recs.
*              IF ls_cdgrp-otgrp = lc_rmdm.
***--START OF CHANGES FOR CHG0127010 BY KBANERJEE
*                <lfs_qmel_data>-zotgrp = lc_del.
*                IF lv_cdgrp_recs GT 1.
*                  <lfs_qmel_data>-zobjpart = lc_obj.
*                ELSE.
*                  CLEAR <lfs_qmel_data>-zobjpart.
*                ENDIF.
***--END OF CHANGES FOR CHG0127010 BY KBANERJEE
*              ELSE.
*                CLEAR <lfs_qmel_data>-zotgrp.
*              ENDIF.
*            ELSE.
*              <lfs_qmel_data>-zobjpart = lc_obj.

* If an entry exists in notifications items, but that entry is not equal to 'RM-DM'
* update ZOBJPART field as 'OBJ'. If OTGRP is RM-DM then update ZOTGRP = 'DEL'
* If code group is Blank then do not update ZOTGRP and ZOBJPART
             LOOP AT lt_cdgrp INTO ls_cdgrp FROM sy-tabix.
              IF ls_cdgrp-qmnum NE <lfs_qmel_data>-qmnum.
                EXIT.
              ENDIF.
              IF ls_cdgrp-otgrp = lc_rmdm.
                <lfs_qmel_data>-zotgrp = lc_del.
              ELSE.
                IF NOT ls_cdgrp-otgrp IS INITIAL.
                 <lfs_qmel_data>-zobjpart = lc_obj.
                ENDIF.
              ENDIF.
              IF <lfs_qmel_data>-zotgrp IS NOT INITIAL AND
                <lfs_qmel_data>-zobjpart IS NOT INITIAL.
                EXIT.
              ENDIF.
              CLEAR ls_cdgrp.
             ENDLOOP.
            ELSE.
              CLEAR <lfs_qmel_data>-zobjpart.
***--END OF CHANGES FOR CHG0148345 BY SUBRAMS2
            ENDIF.
            READ TABLE lt_activity INTO ls_activity
                                   WITH KEY qmnum = <lfs_qmel_data>-qmnum
**--START OF CHANGES FOR CHG0127010 BY KBANERJEE
                                            "fenum = ls_cdgrp-fenum
**--END OF CHANGES FOR CHG0127010 BY KBANERJEE
                                   BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <lfs_qmel_data>-zactivity = lc_act.
            ENDIF."endif sy-subrc check on  lt_activity
**--START OF CHANGES FOR CHG0127010 BY KBANERJEE
            READ TABLE lt_wrkcntr INTO ls_wrkcntr
                                  WITH KEY bname = <lfs_qmel_data>-zernam
                                  BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <lfs_qmel_data>-zworkcenter = ls_wrkcntr-parva.
            ENDIF.
**--END OF CHANGES FOR CHG0127010 BY KBANERJEE
          ENDAT.
        ENDIF."endif sy-subrc check on lt_qmel_data
      ENDLOOP.
      IF lt_qmel_data IS NOT INITIAL.
        REFRESH i_t_data.
        i_t_data[] = lt_qmel_data[].
      ENDIF.
***--Start OF CHANGES FOR CHG0148345 BY SUBRAMS2
      WHEN '0FUNCT_LOC_ATTR'.
      CLEAR lt_func_loc.
      lt_func_loc[] = i_t_data[].
      SORT lt_func_loc BY tplnr.
      DELETE ADJACENT DUPLICATES FROM lt_func_loc COMPARING tplnr.
      IF lt_func_loc[] IS NOT INITIAL.
*      Fetch location data from ILOA table
       SELECT iloan tplnr
         FROM iloa
         INTO TABLE lt_iloa
         FOR ALL ENTRIES IN lt_func_loc
         WHERE tplnr = lt_func_loc-tplnr.
       IF sy-subrc eq 0 AND lt_iloa IS NOT INITIAL.
*        Fetch Equipment for the loaction from EQUZ table
         SELECT equnr datbi iloan
           FROM equz
           INTO TABLE lt_equz
           FOR ALL ENTRIES IN lt_iloa
           WHERE iloan = lt_iloa-iloan
             AND datbi = '99991231'.
         IF sy-subrc eq 0.
          SORT lt_equz BY iloan.
         ENDIF.
       ENDIF.
       SORT lt_iloa BY tplnr.
       LOOP AT i_t_data ASSIGNING <lfs_func_loc>.
        IF <lfs_func_loc>-tplnr IS NOT INITIAL.
*         For a Functional location there are many Locations assigned in ILOA table
*         to read all locations and get the equipment number (parellel cursor is used for performance)
          READ TABLE lt_iloa WITH KEY tplnr = <lfs_func_loc>-tplnr TRANSPORTING NO FIELDS
                         BINARY SEARCH.
          IF sy-subrc eq 0.
            lv_tabix = sy-tabix.
            LOOP AT lt_iloa INTO ls_iloa FROM lv_tabix.
             IF ls_iloa-tplnr NE <lfs_func_loc>-tplnr.
              EXIT.
             ENDIF.
             READ TABLE lt_equz INTO ls_equz WITH KEY iloan = ls_iloa-iloan BINARY SEARCH.
             IF sy-subrc eq 0.
               <lfs_func_loc>-zequnr = ls_equz-equnr.
               EXIT.
             ENDIF.
            ENDLOOP. "End of Parellel cursor
          ENDIF.
        ENDIF.
       ENDLOOP.
      ENDIF.
***--END OF CHANGES FOR CHG0148345 BY SUBRAMS2
    WHEN OTHERS.
*Do nothing
  ENDCASE.
