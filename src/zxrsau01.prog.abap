*&---------------------------------------------------------------------*
*&  Include           ZXRSAU01
*&---------------------------------------------------------------------*
CASE i_datasource.

* BOC 11-Apr-2012 RBHATT TR DECK905426

  WHEN '0FI_AP_4'.

    DATA : gwa_dtfiap_3    LIKE dtfiap_3,
           c_t_data1       LIKE STANDARD TABLE OF dtfiap_3,
           gwa_payr        LIKE payr,
           gv_bvorg        TYPE bvorg,
           gwa_bkpf        LIKE bkpf,
           gv_tabix        TYPE sy-tabix,
           gv_strbvorg(16) TYPE c,
           git_bsak        TYPE TABLE OF bsak,
           gwa_bsak        LIKE bsak.

    CONSTANTS : co_19(2) TYPE c VALUE '19',
                co_zf(2) TYPE c VALUE 'ZF',
                co_i(1)  TYPE c VALUE 'I',
                co_c(1)  TYPE c VALUE 'C',
                co_p(1)  TYPE c VALUE 'P'.

    CLEAR : c_t_data1.

    c_t_data1[] = c_t_data[].

    LOOP AT c_t_data1 INTO gwa_dtfiap_3 WHERE blart = co_zf.

      CLEAR : gv_bvorg, gv_strbvorg, gwa_bsak, gwa_bkpf.

      gv_tabix = sy-tabix.

***Fetch Clearing document

**Fetch Cross company code from bkpf table

      SELECT SINGLE * FROM bkpf INTO gwa_bkpf
      WHERE    bukrs = gwa_dtfiap_3-bukrs
        AND    belnr = gwa_dtfiap_3-augbl
        AND    gjahr = gwa_dtfiap_3-gjahr.

*      SELECT SINGLE * FROM bkpf INTO gwa_bkpf
*      WHERE    bukrs = gwa_dtfiap_3-bukrs
*        AND    belnr = gwa_dtfiap_3-belnr
*        AND    gjahr = gwa_dtfiap_3-gjahr.


      IF sy-subrc = 0 AND gwa_bkpf-bvorg IS NOT INITIAL.

        gv_strbvorg = gwa_bkpf-bvorg.
        SHIFT gv_strbvorg LEFT DELETING LEADING '0'.
        CONDENSE gv_strbvorg.

**Checking 19 series vendor
        IF gv_strbvorg+0(2) = co_19.
          gwa_dtfiap_3-zzaugbl = gwa_dtfiap_3-augbl.

***********************logic for company code*********************
*          IF gwa_dtfiap_3-zlsch <> co_i AND
*             gwa_dtfiap_3-zlsch <> co_c AND gwa_dtfiap_3-zlsch <> co_p.
          gwa_dtfiap_3-zzbukrs = gwa_bkpf-bvorg+10(4).  "payment company code

*          ELSEIF gwa_dtfiap_3-zlsch = co_i AND
*            gwa_dtfiap_3-zlsch = co_c AND gwa_dtfiap_3-zlsch = co_p..
*            gwa_dtfiap_3-zzbukrs = gwa_dtfiap_3-bukrs.

*          ENDIF.
**********************************************************************
          MODIFY c_t_data FROM gwa_dtfiap_3 INDEX gv_tabix.
        ELSE.

          gwa_dtfiap_3-zzaugbl = gwa_bkpf-bvorg+0(10).

***********************logic for company code*********************
*          IF gwa_dtfiap_3-zlsch <> co_i AND
*             gwa_dtfiap_3-zlsch <> co_c AND gwa_dtfiap_3-zlsch <> co_p.
          gwa_dtfiap_3-zzbukrs = gwa_bkpf-bvorg+10(4).  "payment company code

*          ELSEIF gwa_dtfiap_3-zlsch = co_i AND
*             gwa_dtfiap_3-zlsch = co_c AND gwa_dtfiap_3-zlsch = co_p..
*            gwa_dtfiap_3-zzbukrs = gwa_dtfiap_3-bukrs.

*          ENDIF.

**********************************************************************
          MODIFY c_t_data FROM gwa_dtfiap_3 INDEX gv_tabix.

        ENDIF.

      ELSE.

        gwa_dtfiap_3-zzaugbl = gwa_dtfiap_3-augbl.

**************************************
*           IF gwa_dtfiap_3-zlsch = co_i AND
*             gwa_dtfiap_3-zlsch = co_c AND gwa_dtfiap_3-zlsch = co_p.
*            gwa_dtfiap_3-zzbukrs = gwa_dtfiap_3-bukrs.
*          ENDIF.
******************************************


        MODIFY c_t_data FROM gwa_dtfiap_3 INDEX gv_tabix.

      ENDIF.


    ENDLOOP.

  WHEN '0FI_GL_14' OR '3FI_GL_P1_SI'.

    DATA: extstruc LIKE faglposbw,
          l_tabix LIKE sy-tabix,
          it_data2 TYPE STANDARD TABLE OF faglposbw.

    TYPES : BEGIN OF ty_data1,
           bukrs TYPE bseg-bukrs,
           belnr TYPE bseg-belnr,
           buzei TYPE bseg-buzei,
           altkt TYPE bseg-altkt,
           anbwa TYPE bseg-anbwa,
          augcp TYPE bseg-augcp,
          bpmng TYPE bseg-bpmng,
          bprme TYPE bseg-bprme,
          bustw TYPE bseg-bustw,
          bvtyp TYPE bseg-bvtyp,
          bwkey TYPE bseg-bwkey,
          bzdat TYPE bseg-bzdat,
          empfb TYPE bseg-empfb,
          fdgrp TYPE bseg-fdgrp,
          fdlev TYPE bseg-fdlev,
          fdtag TYPE bseg-fdtag,
          fdwbt TYPE bseg-fdwbt,
          fipos TYPE bseg-fipos,
          hbkid TYPE bseg-hbkid,
          lokkt TYPE bseg-lokkt,
          lstar TYPE bseg-lstar,
          matnr TYPE bseg-matnr,
          pernr TYPE bseg-pernr,
          prodper TYPE bseg-prodper,
          qsznr TYPE bseg-qsznr,
          rdif2 TYPE bseg-rdif2,
          rdif3 TYPE bseg-rdif3,
          rdiff TYPE bseg-rdiff,
          rewwr TYPE bseg-rewwr,
          rpacq TYPE bseg-rpacq,
          ryacq TYPE bseg-ryacq,
          sknt2 TYPE bseg-sknt2,
          sknt3 TYPE bseg-sknt3,
          txjcd TYPE bseg-txjcd,
          abper TYPE bseg-abper,
          lifnr TYPE bseg-lifnr,                            "SR 4417
          koart TYPE bseg-koart,                            "SR 4417
          hkont TYPE bseg-hkont,
         xref1 TYPE bseg-xref1,
*phase 4
         nplnr TYPE bseg-nplnr,
*********Added by HSHETTY 25/7/2012 - for BW VAL report**********
      txbfw TYPE bseg-txbfw,
*********Added by HSHETTY 25/7/2012 - for BW VAL report**********
*********Added by HSHETTY 01/08/2012 - for BW VAL report**********
      zztxgrp TYPE bseg-txgrp,
*********Added by HSHETTY 01/08/2012 - for BW VAL report**********
      aufnr TYPE bseg-aufnr,
      gjahr TYPE bseg-gjahr,
        END OF ty_data1.

    DATA:  it_data1 TYPE TABLE OF ty_data1.

    DATA : BEGIN OF it_data5 OCCURS 0,
            racct TYPE faglflexa-racct,                     "SR 10970
            rldnr TYPE faglflexa-rldnr,
            rbukrs TYPE faglflexa-rbukrs,
            docnr TYPE faglflexa-docnr,
            docln TYPE faglflexa-docln,
            usnam TYPE faglflexa-usnam,
            drcrk TYPE faglflexa-drcrk,
           rtcur TYPE faglflexa-rtcur,
           tsl TYPE faglflexa-tsl,
           hsl TYPE faglflexa-hsl,
           ksl TYPE faglflexa-ksl,
           osl TYPE faglflexa-osl,
*Addition for tmestamp chanbges *
           ztimestamp TYPE faglflexa-timestamp,
*Addition for tmestamp chanbges *
      END OF it_data5.

*******BOC Reddy Srinivasa 26-Apr-2012*******************
    TYPES: BEGIN OF ty_gwa_bkpf1,
            zbukrs TYPE bkpf-bukrs,
            zbelnr TYPE bkpf-belnr,
            zgjahr TYPE bkpf-gjahr,
            zbvorg TYPE bkpf-bvorg,
      END OF ty_gwa_bkpf1.

    TYPES: BEGIN OF ty_vendor,
           bukrs1 TYPE bukrs,
           vendor1 TYPE lifnr,
           belnr1 TYPE belnr_d,
            year1 TYPE gjahr,
           xref11 TYPE xref1,
           koart1 TYPE koart,
         END OF ty_vendor.



    TYPES: BEGIN OF ty_itdata6,                             "SR 10970
           ktopl TYPE ska1-ktopl,
           saknr TYPE ska1-saknr,
            bilkt TYPE ska1-bilkt,
           END OF ty_itdata6.                               "SR 10970

    DATA: t_gw_bkpf1 TYPE TABLE OF ty_gwa_bkpf1,
          wa_gw_bkpf1  TYPE ty_gwa_bkpf1.
*          t_gw_bkpf2 TYPE TABLE OF ty_gwa_bkpf1,
*          wa_gw_bkpf2  TYPE ty_gwa_bkpf1.

    DATA: lv_comp TYPE bukrs,
          lv_year TYPE gjahr,
          lv_document TYPE belnr_d,
          lv_vendor TYPE lifnr,
          lv_comp1 TYPE bukrs,
          lv_document1 TYPE belnr_d,
          lv_vendor1 TYPE lifnr,
          lv_document2 TYPE belnr_d,
          lv_comp2 TYPE bukrs,
         lv_year2 TYPE gjahr.
    DATA: t_vendor TYPE TABLE OF ty_vendor,
          wa_vendor TYPE ty_vendor.
    DATA: it_data6 TYPE STANDARD TABLE OF ty_itdata6.
******EOC Reddy Srinivasa 26-Apr-2012*******************

    DATA : wa_data2 LIKE faglposbw,
           wa_data1 TYPE faglposbw,
           wa_data3 TYPE ty_data1,
           wa_data5 LIKE it_data5,
           gv_docln LIKE faglflexa-docln,
           wa_data6 TYPE ty_itdata6.


    DATA: doclnum1(7) TYPE c,
          doclnum2(7) TYPE c,
          refkey1(12) TYPE c,
          lv_ref1(12) TYPE c.

    TYPES : BEGIN OF ty_bvor,
            zbvorg   TYPE bvor-bvorg,
            zbukrs   TYPE bvor-bukrs,
            zgjahr   TYPE bvor-gjahr,
            zbelnr   TYPE bvor-belnr,
           END OF ty_bvor.

    TYPES : BEGIN OF ty_faglflexa,
               docnr      TYPE faglflexa-docnr,
               rldnr      TYPE faglflexa-rldnr,
               rbukrs     TYPE faglflexa-rbukrs,
               docln      TYPE faglflexa-docln,
               rtcur      TYPE faglflexa-rtcur,
               racct      TYPE faglflexa-racct,
               tsl        TYPE faglflexa-tsl,
               hsl        TYPE faglflexa-hsl,
               ksl        TYPE faglflexa-ksl,
               osl        TYPE faglflexa-osl,
               drcrk      TYPE faglflexa-drcrk,
               usnam      TYPE faglflexa-usnam,
               timestamp  TYPE faglflexa-timestamp,
            END OF ty_faglflexa.

    DATA : t_gw_bkpf1_temp TYPE STANDARD TABLE OF ty_gwa_bkpf1,
           t_gw_bkpf2      TYPE STANDARD TABLE OF ty_bvor,
           git_faglflexa   TYPE STANDARD TABLE OF ty_faglflexa,
           gwa_faglflexa   TYPE ty_faglflexa,
           wa_gw_bkpf2     TYPE ty_bvor.


    CLEAR : wa_data1, wa_data2 ,wa_data3, l_tabix, it_data1, it_data2, wa_data6, it_data6.

    it_data2[] = c_t_data[] .

********BOC Reddy Srinivasa 26-Apr-2012*******************
    IF NOT it_data2[] IS INITIAL.

      SELECT bukrs
             belnr
             gjahr
             bvorg
        FROM bkpf INTO TABLE t_gw_bkpf1 FOR ALL ENTRIES IN it_data2
        WHERE  bukrs = it_data2-bukrs
            AND belnr = it_data2-belnr
             AND  gjahr = it_data2-gjahr.

      IF sy-subrc = 0.
        REFRESH : t_gw_bkpf1_temp.
        t_gw_bkpf1_temp[] = t_gw_bkpf1[].

        DELETE t_gw_bkpf1_temp WHERE ( zbvorg = '' ) OR
                                     ( zbvorg IS INITIAL ).

        IF t_gw_bkpf1_temp[] IS NOT INITIAL.
          SELECT bvorg bukrs gjahr belnr
            FROM bvor
            INTO TABLE t_gw_bkpf2
            FOR ALL ENTRIES IN t_gw_bkpf1_temp
            WHERE bvorg = t_gw_bkpf1_temp-zbvorg.
        ENDIF.

*        SELECT bukrs
*           belnr
*           gjahr
*           bvorg
*      FROM bkpf INTO TABLE t_gw_bkpf2 FOR ALL ENTRIES IN t_gw_bkpf1
*      WHERE  bvorg = t_gw_bkpf1-zbvorg.
      ENDIF.
    ENDIF.
*****EOC Reddy Srinivasa 26-Apr-2012*******************
****** Changes by HSHETTY - TXBFW field added in Select statement - 25/7/2012***********
*    SELECT altkt bukrs belnr buzei aufnr gjahr anbwa augcp bpmng bprme bustw bvtyp bwkey bzdat empfb fdgrp
*                    fdlev fdtag fdwbt fipos hbkid lokkt lstar matnr pernr prodper qsznr rdif2 rdif3 rdiff rewwr rpacq ryacq sknt2 sknt3 txjcd abper
*             lifnr koart hkont xref1 txbfw txgrp AS zztxgrp FROM bseg INTO
*    CORRESPONDING FIELDS OF TABLE it_data1 FOR ALL ENTRIES IN
*    it_data2
*              WHERE bukrs = it_data2-bukrs
*                AND belnr = it_data2-belnr
*                AND gjahr = it_data2-gjahr
*                AND buzei = it_data2-buzei.
***                 or hkont = it_data2-hkont.
    IF it_data2[] IS NOT INITIAL.


      SELECT bukrs belnr buzei altkt anbwa augcp bpmng bprme bustw bvtyp bwkey bzdat empfb fdgrp
                fdlev fdtag fdwbt fipos hbkid lokkt lstar matnr pernr prodper qsznr rdif2 rdif3 rdiff rewwr rpacq ryacq sknt2 sknt3 txjcd abper
               lifnr koart hkont xref1 nplnr txbfw txgrp AS zztxgrp aufnr gjahr FROM bseg INTO TABLE it_data1
      FOR ALL ENTRIES IN     it_data2
                WHERE bukrs = it_data2-bukrs
                  AND belnr = it_data2-belnr
                  AND gjahr = it_data2-gjahr
                  AND buzei = it_data2-buzei.

**                 or hkont = it_data2-hkont.

*IF It_data1 is initial.
**SR 10970
      SELECT ktopl
           saknr
            bilkt FROM ska1 INTO TABLE it_data6 FOR ALL ENTRIES IN it_data2
      WHERE ktopl = it_data2-ktopl
        OR saknr = it_data2-saknr.
*ENDIF.
**SR 10970

*    SELECT usnam rldnr rbukrs docnr docln rtcur racct drcrk tsl hsl ksl osl timestamp AS ztimestamp
*        FROM faglflexa INTO CORRESPONDING FIELDS OF TABLE it_data5 FOR ALL ENTRIES IN it_data2 WHERE
*                 rbukrs = it_data2-bukrs
*                 AND rldnr = it_data2-rldnr
*                 AND docnr = it_data2-docnr
*                 AND docln = it_data2-docln.

      SELECT docnr rldnr rbukrs docln rtcur racct tsl hsl ksl osl drcrk usnam timestamp "AS ztimestamp
        FROM faglflexa
        INTO TABLE git_faglflexa
        FOR ALL ENTRIES IN it_data2
        WHERE ryear   = it_data2-ryear  AND
              docnr   = it_data2-docnr  AND
              rldnr   = it_data2-rldnr  AND
              rbukrs  = it_data2-bukrs  AND
              docln   = it_data2-docln.
      IF sy-subrc = 0.
        REFRESH : it_data5[].
        LOOP AT git_faglflexa INTO gwa_faglflexa.

          it_data5-docnr      =    gwa_faglflexa-docnr.
          it_data5-rldnr      =    gwa_faglflexa-rldnr.
          it_data5-rbukrs     =    gwa_faglflexa-rbukrs.
          it_data5-docln      =    gwa_faglflexa-docln.
          it_data5-rtcur      =    gwa_faglflexa-rtcur.
          it_data5-racct      =    gwa_faglflexa-racct.
          it_data5-tsl        =    gwa_faglflexa-tsl.
          it_data5-hsl        =    gwa_faglflexa-hsl.
          it_data5-ksl        =    gwa_faglflexa-ksl.
          it_data5-osl        =    gwa_faglflexa-osl.
          it_data5-drcrk      =    gwa_faglflexa-drcrk.
          it_data5-usnam      =    gwa_faglflexa-usnam.
          it_data5-ztimestamp =    gwa_faglflexa-timestamp.
          APPEND  it_data5.
          CLEAR : it_data5.

        ENDLOOP.
      ENDIF.

      IF t_gw_bkpf2[] IS NOT INITIAL.

        SELECT bukrs lifnr belnr gjahr xref1 koart
          FROM bseg
          INTO TABLE t_vendor
          FOR ALL ENTRIES IN t_gw_bkpf2
          WHERE  bukrs = t_gw_bkpf2-zbukrs  AND
                 belnr = t_gw_bkpf2-zbelnr  AND
                 gjahr = t_gw_bkpf2-zgjahr .

      ENDIF.
    ENDIF.
    LOOP AT it_data2 INTO wa_data2.
      l_tabix = sy-tabix.
      wa_data1 = wa_data2.

*******BOC Reddy Srinivasa 26-Apr-2012*******************

*START SDP :82035
*USER : PGOHIL 02/19/2015
*Description : Add Year clause in the read statement to fix the bug of multiple Years for same document
*TR : C11K930806 : SL:TECH:BW: 0FI_GL_14 - Vendor Fix ##82035##
*BEFORE     READ TABLE it_data1 INTO wa_data3 WITH KEY bukrs = wa_data1-bukrs
*BEFORE                 belnr = wa_data1-belnr
*BEFORE                 buzei = wa_data1-buzei.

      READ TABLE it_data1 INTO wa_data3 WITH KEY   "After
                  bukrs = wa_data1-bukrs           "After
                  gjahr = wa_data1-gjahr           "After (Addition)
                  belnr = wa_data1-belnr           "After
                  buzei = wa_data1-buzei.          "After
*START SDP :82035

      IF sy-subrc EQ 0 .

        wa_data1-altkt = wa_data3-altkt.
        wa_data1-anbwa = wa_data3-anbwa.
        wa_data1-augcp = wa_data3-augcp.
        wa_data1-bpmng = wa_data3-bpmng.
        wa_data1-bprme = wa_data3-bprme.
        wa_data1-bustw = wa_data3-bustw.
        wa_data1-bvtyp = wa_data3-bvtyp.
        wa_data1-bwkey = wa_data3-bwkey.
        wa_data1-bzdat = wa_data3-bzdat.
        wa_data1-empfb = wa_data3-empfb.
        wa_data1-fdgrp = wa_data3-fdgrp.
        wa_data1-fdlev = wa_data3-fdlev.
        wa_data1-fdtag = wa_data3-fdtag.
        wa_data1-fdwbt = wa_data3-fdwbt.
        wa_data1-fipos = wa_data3-fipos.
        wa_data1-hbkid = wa_data3-hbkid.
        wa_data1-lokkt = wa_data3-lokkt.
        wa_data1-lstar = wa_data3-lstar.
        wa_data1-matnr = wa_data3-matnr.
        wa_data1-pernr = wa_data3-pernr.
        wa_data1-prodper = wa_data3-prodper.
        wa_data1-qsznr = wa_data3-qsznr.
        wa_data1-rdif2 = wa_data3-rdif2.
        wa_data1-rdif3 = wa_data3-rdif3.
        wa_data1-rdiff = wa_data3-rdiff.
        wa_data1-rewwr = wa_data3-rewwr.
        wa_data1-rpacq = wa_data3-rpacq.
        wa_data1-ryacq = wa_data3-ryacq.
        wa_data1-sknt2 = wa_data3-sknt2.
        wa_data1-sknt3 = wa_data3-sknt3.
        wa_data1-txjcd = wa_data3-txjcd.
        wa_data1-abper = wa_data3-abper.
        wa_data1-nplnr = wa_data3-nplnr.

*********Added by HSHETTY 25/7/2012 - for BW VAL report**********
        wa_data1-txbfw = wa_data3-txbfw.
*********Added by HSHETTY 25/7/2012 - for BW VAL report**********
*********Added by HSHETTY 25/7/2012 - for BW VAL report**********
        wa_data1-zztxgrp = wa_data3-zztxgrp.
*********Added by HSHETTY 25/7/2012 - for BW VAL report**********
      ENDIF.
*******EOC Reddy Srinivasa 26-Apr-2012*******************


******BOC Reddy Srinivasa 26-Apr-2012*******************
      IF wa_data3-lifnr IS INITIAL.

*START SDP :82035
*USER : PGOHIL 02/19/2015
*Description : Add Year clause in the read statement to fix the bug of multiple Years for same document
*TR : C11K930806 : SL:TECH:BW: 0FI_GL_14 - Vendor Fix ##82035##
* BEFORE       READ TABLE it_data1 INTO wa_data3 WITH KEY bukrs = wa_data1-bukrs
* BEFORE            belnr = wa_data1-belnr
* BEFORE            koart = 'K'.
        READ TABLE it_data1 INTO wa_data3 WITH KEY "After
             bukrs = wa_data1-bukrs                "After
             belnr = wa_data1-belnr                "After
             gjahr = wa_data1-gjahr                "After addtition
             koart = 'K'.
**END SDP :82035

        IF sy-subrc = 0.
          IF wa_data3-lifnr IS NOT INITIAL .
            wa_data1-lifnr = wa_data3-lifnr.
          ENDIF.
          IF wa_data3-xref1 IS NOT INITIAL.
            lv_ref1 = wa_data3-xref1.
          ENDIF.
        ENDIF.
      ELSE.
        wa_data1-lifnr = wa_data3-lifnr.
      ENDIF.

      IF lv_ref1 IS INITIAL.         " SR 24137 "
        lv_ref1 = wa_data1-xref1.
      ENDIF.

      IF wa_data3-lifnr IS INITIAL.
        CLEAR: lv_document, lv_document1, lv_document2,     "(+) SDP 68208 SSHETTY
               lv_comp,     lv_comp1,     lv_comp2,         "(+) SDP 68208 SSHETTY
               lv_year,     lv_year2.                       "(+) SDP 68208 SSHETTY
        READ TABLE t_gw_bkpf1 INTO wa_gw_bkpf1 WITH KEY
                        zbukrs = wa_data2-bukrs
                        zbelnr = wa_data2-belnr.

        IF sy-subrc = 0  AND wa_gw_bkpf1-zbvorg IS NOT INITIAL..
          LOOP AT t_gw_bkpf2 INTO wa_gw_bkpf2 WHERE zbvorg = wa_gw_bkpf1-zbvorg.
            lv_document1 = wa_gw_bkpf1-zbvorg+0(10).
            lv_comp1 =  wa_gw_bkpf1-zbvorg+10(4).

            IF wa_gw_bkpf2-zbelnr EQ lv_document1 AND
                 wa_gw_bkpf2-zbukrs  EQ lv_comp1.
              lv_document2 = wa_gw_bkpf2-zbelnr.
              lv_comp2 = wa_gw_bkpf2-zbukrs.
              lv_year2 = wa_gw_bkpf2-zgjahr.
*              CONTINUE.
***- Begin of Change by SSHETTY SDP 68208
*            ELSEIF wa_gw_bkpf2-zbelnr NE lv_document1 AND
*                  wa_gw_bkpf2-zbukrs  NE lv_comp1.
            ELSEIF wa_gw_bkpf2-zbelnr NE lv_document1 OR  "AND
                  wa_gw_bkpf2-zbukrs  NE lv_comp1.
***- End of Change by SSHETTY SDP 68208
              lv_document = wa_gw_bkpf2-zbelnr.
              lv_comp = wa_gw_bkpf2-zbukrs.
              lv_year = wa_gw_bkpf2-zgjahr.

            ENDIF.
            CLEAR wa_gw_bkpf2.
          ENDLOOP.

          IF lv_document IS NOT INITIAL.

            CLEAR wa_vendor.
*            SELECT bukrs lifnr belnr gjahr xref1 koart FROM bseg INTO TABLE t_vendor
*                         WHERE ( bukrs = lv_comp OR bukrs = lv_comp2 ) AND
*                               ( belnr = lv_document OR belnr = lv_document2 )
*              AND
*                               ( gjahr = lv_year OR gjahr = lv_year2 ).

            READ TABLE t_vendor INTO wa_vendor WITH KEY belnr1 = lv_document koart1 = 'K' bukrs1 = lv_comp.

            IF wa_vendor-vendor1 IS NOT INITIAL.
              wa_data1-lifnr = wa_vendor-vendor1.
            ELSE."if wa_vendor-vendor1 is initial.
              READ TABLE t_vendor INTO wa_vendor WITH KEY belnr1 = lv_document2 koart1 = 'K' bukrs1 = lv_comp2.
              wa_data1-lifnr = wa_vendor-vendor1.
            ENDIF.


            IF lv_ref1 IS INITIAL.
              READ TABLE t_vendor INTO wa_vendor WITH KEY belnr1 = lv_document koart1 = 'K' bukrs1 = lv_comp.
              IF sy-subrc = 0.
                lv_ref1 = wa_vendor-xref11.
              ENDIF.
            ENDIF.
            IF lv_ref1 IS INITIAL.
              READ TABLE t_vendor INTO wa_vendor WITH KEY belnr1 = lv_document2 koart1 = 'K' bukrs1 = lv_comp2.
              IF sy-subrc = 0 .
                lv_ref1 = wa_vendor-xref11.
              ENDIF.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.

*******EOC Reddy Srinivasa 26-Apr-2012*******************

      READ TABLE it_data5 INTO wa_data5 WITH KEY rbukrs = wa_data1-bukrs
      docln = wa_data1-docln
      docnr = wa_data1-docnr
      rldnr = wa_data1-rldnr.

      IF sy-subrc EQ 0.
        wa_data1-usnam = wa_data5-usnam.
        wa_data1-drcrk = wa_data5-drcrk.
        wa_data1-rtcur = wa_data5-rtcur.
        wa_data1-tsl = wa_data5-tsl.
        wa_data1-hsl = wa_data5-hsl.
        wa_data1-ksl = wa_data5-ksl.
        wa_data1-osl = wa_data5-osl.
*changes for timestamp
        wa_data1-ztimestamp = wa_data5-ztimestamp.


** SR 10970
        READ TABLE it_data6 INTO wa_data6 WITH KEY saknr = wa_data1-hkont ktopl = wa_data1-ktopl.

        IF sy-subrc = 0.
          IF wa_data3-altkt = ' ' .
            wa_data1-altkt = wa_data6-bilkt.

          ENDIF.
        ENDIF.
      ENDIF.

** SR 10970

      IF lv_ref1 IS INITIAL.
        READ TABLE it_data1 INTO wa_data3 WITH KEY belnr = wa_data2-belnr  bukrs = wa_data2-bukrs koart = 'K'.
        IF sy-subrc = 0.
          lv_ref1 = wa_data3-xref1.
        ENDIF.
      ENDIF.

      wa_data1-xref1 =  lv_ref1.

*******************************
*Start SR 32504
*Reddy Srinivasa
*Capgemini India
*******************************
      IF wa_data2-aufnr IS INITIAL.
        READ TABLE it_data1 INTO wa_data3 WITH KEY belnr = wa_data2-belnr bukrs = wa_data2-bukrs buzei = wa_data2-buzei gjahr = wa_data2-gjahr.
        wa_data1-aufnr = wa_data3-aufnr.
      ENDIF.


*******************************
*End SR 32504
*Reddy Srinivasa
*Capgemini India
*******************************

      MODIFY it_data2 FROM wa_data1 INDEX l_tabix  .


      CLEAR : wa_data1, wa_data2 ,wa_data3, l_tabix, lv_ref1.
    ENDLOOP.

    c_t_data[] = it_data2[] .


*-----------------------------------------------------
  WHEN '0CO_OM_CCA_1'.
    DATA: l_tabix2    LIKE sy-tabix,
          it_codata3  TYPE STANDARD TABLE OF icctrcst.

    DATA : BEGIN OF it_codata4 OCCURS 0,
            wrttp     TYPE cosp-wrttp,
            versn     TYPE cosp-versn,
            kstar     TYPE cosp-kstar,
            hrkft     TYPE cosp-hrkft,
            bukrs     TYPE cosp-bukrs,
            lednr     TYPE cosp-lednr,
            vbund     TYPE cosp-vbund,
           END OF it_codata4.

    DATA : BEGIN OF it_codata1 OCCURS 0,
            wrttp     TYPE cosp-wrttp,
            versn     TYPE cosp-versn,
            kstar     TYPE cosp-kstar,
            hrkft     TYPE cosp-hrkft,
            parob     TYPE coss-parob,
    END OF it_codata1.

    DATA : wa_codata4 LIKE icctrcst,
           wa_codata5 TYPE icctrcst,
           wa_codata6 LIKE it_codata4,
           wa_codata1 LIKE it_codata1.

    CLEAR : wa_codata4, wa_codata5, wa_codata1,
            l_tabix2,   it_codata3, it_codata4,
            it_codata1.

    it_codata3[] = c_t_data[] .

    SELECT  wrttp versn kstar hrkft bukrs lednr vbund
            FROM cosp
            INTO CORRESPONDING FIELDS OF TABLE it_codata4
            FOR ALL ENTRIES IN it_codata3
            WHERE lednr = 51
            AND   objnr NE space
            AND   gjahr > 0
            AND   wrttp = it_codata3-wrttp
            AND   versn = it_codata3-versn
            AND   kstar = it_codata3-kstar.
*            AND   hrkft = it_codata3-hrkft.
    IF sy-subrc = 0.
      SORT it_codata4 BY wrttp versn kstar hrkft.
    ENDIF.

    SELECT wrttp versn kstar hrkft parob
           FROM coss
           INTO CORRESPONDING FIELDS OF TABLE it_codata1
           FOR ALL ENTRIES IN it_codata3
           WHERE lednr = 51
           AND   objnr NE space
           AND   gjahr > 0
           AND   wrttp = it_codata3-wrttp
           AND   versn = it_codata3-versn
           AND   kstar = it_codata3-kstar.
*           AND   hrkft = it_codata3-hrkft.
    IF sy-subrc = 0.
      SORT it_codata1 BY wrttp versn kstar hrkft.
    ENDIF.

    LOOP AT it_codata3 INTO wa_codata4.
      l_tabix2   = sy-tabix.
      wa_codata5 = wa_codata4.

      READ TABLE it_codata4 INTO wa_codata6
                            WITH KEY wrttp = wa_codata5-wrttp
                                     versn = wa_codata5-versn
                                     kstar = wa_codata5-kstar
                                     hrkft = wa_codata5-hrkft
                            BINARY SEARCH.
      IF sy-subrc = 0.
        wa_codata5-bukrs = wa_codata6-bukrs.
        wa_codata5-lednr = wa_codata6-lednr.
        wa_codata5-vbund = wa_codata6-vbund.
*        wa_codata5-parob = wa_codata1-parob.
        wa_codata5-zwrttp = wa_codata6-wrttp.

        MODIFY it_codata3 FROM wa_codata5 INDEX l_tabix2  .
      ENDIF.

      READ TABLE it_codata1 INTO wa_codata1
                            WITH KEY wrttp = wa_codata5-wrttp
                                     versn = wa_codata5-versn
                                     kstar = wa_codata5-kstar
                                     hrkft = wa_codata5-hrkft
                            BINARY SEARCH.
      IF sy-subrc EQ 0 .
*        wa_codata5-bukrs = wa_codata6-bukrs.
*        wa_codata5-lednr = wa_codata6-lednr.
*        wa_codata5-vbund = wa_codata6-vbund.
        wa_codata5-parob = wa_codata1-parob.
*        wa_codata5-zwrttp = wa_codata6-wrttp.

        MODIFY it_codata3 FROM wa_codata5 INDEX l_tabix2  .
      ENDIF.
      CLEAR : wa_codata4, wa_codata5, l_tabix2,
              wa_codata1, wa_codata6.
    ENDLOOP.

*   Filter For Only LEDNR = 50
    SORT it_codata3 BY lednr.
    DELETE it_codata3 WHERE lednr NE 51.

*   Filter For Only VTYPE = 10 AND 20
    SORT it_codata3 BY vtype.
    DELETE it_codata3 WHERE ( vtype NE 10 AND vtype NE 20 ).

*   Filter For Only Currency TYPE = 10 AND 20
    SORT it_codata3 BY curtype.
    DELETE it_codata3 WHERE ( curtype NE 10 AND curtype NE 20 ).

    c_t_data[] = it_codata3[] .
*-----------------------------------------------------
*KBANERJEE
  WHEN '0NOTIFICATN_ATTR'.
    LOOP AT c_t_data1 INTO gwa_dtfiap_3.
    ENDLOOP.
*KBANERJEE
ENDCASE.
