class ZCL_PARKOUR_CUSTOM_SEARCH definition
  public
  final
  create public .

public section.

  types:
******Org.Cust Details
    BEGIN OF ty_kna1,
             kunnr TYPE  kunnr,
             land1 TYPE  land1_gp,
             name1 TYPE  name1_gp,
             name2 TYPE  name2_gp,
             ort01 TYPE  ort01_gp,
             pstlz TYPE  pstlz,
             regio TYPE  regio,
             telf1 TYPE  telf1,
             adrnr TYPE  adrnr,
             ktokd TYPE  ktokd,
             loevm TYPE  loevm_x,
           END OF ty_kna1 .
  types:
*------Knvk Table
    BEGIN OF ty_knvk,
             parnr           TYPE parnr,
             kunnr           TYPE kunnr,
             namev           TYPE namev_vp,
             name1           TYPE name1_gp,
             ort01           TYPE ort01_gp,
             adrnd           TYPE adrnd,
             adrnp           TYPE adrnp,
             abtpa           TYPE abtei_pa,
             abtnr           TYPE abtnr_pa,
             telf1           TYPE telf1,
             pavip           TYPE pavip,
             parau           TYPE parau,
             zzguid          TYPE char50,
             job_description TYPE vtext,
           END OF ty_knvk .
  types:
    BEGIN OF ty_mail,
          addrnumber TYPE ad_addrnum,
          consnumber TYPE ad_consnum,
          flgdefault TYPE ad_flgdfad,
          flg_nouse  TYPE ad_flnouse,
          smtp_addr  TYPE ad_smtpadr,
        END OF ty_mail .
  types:
******Org.cust first name last name
    BEGIN OF ty_adrc,
          addrnumber TYPE ad_addrnum,
          country    TYPE land1,
          region     TYPE  regio,
        END OF ty_adrc .
  types:
******Org.cust Phone number
    BEGIN OF ty_phone,
          addrnumber TYPE ad_addrnum,
          tel_number TYPE ad_tlnmbr,
        END OF ty_phone .
  types:
******Department  Description
    BEGIN OF ty_tsabt,
          abtnr TYPE abtnr,
          vtext TYPE vtext,
        END OF ty_tsabt .
  types:
******Customer GUID
    BEGIN OF ty_zcustomerguid,
          customer_number TYPE kunnr,
          guid            TYPE zcustomerguid_de,
        END OF ty_zcustomerguid .
  types:
******Cusotmer Group  description
    BEGIN OF ty_knvv,
          kunnr TYPE kunnr,
          kdgrp TYPE kdgrp,
        END OF ty_knvv .
  types:
*********Mail list
    BEGIN OF ty_adr6,
          addrnumber TYPE ad_addrnum,
          persnumber TYPE ad_persnum,
        END OF ty_adr6 .
  types:
*********Org. GUID
    BEGIN OF ty_zoneconthdr,
          contract          TYPE zone_contract, " + ESSCF-11097
          enduser           TYPE kunnr,
          organization_guid TYPE zdetl_orgguid,
        END OF ty_zoneconthdr .
  types:
******Contact person Details
    BEGIN OF ty_contract,
             kunnr           TYPE  kunnr,
             contact_no      TYPE  char10,
             first_name      TYPE  namev_vp,
             last_name       TYPE  name1_gp,
             job_function    TYPE  char4,
             job_description TYPE  vtext,
             email           TYPE ad_smtpadr,
             phoneno         TYPE ad_tlnmbr,
             imsguid         TYPE char45,
           END OF ty_contract .
  types:
    Tr_RANGES TYPE RANGE OF kna1-ktokd .
  types:
    tt_kna1 type standard table of ty_kna1 .      "kna1 .
  types:
    tt_knvk type standard table of ty_knvk .
  types:
    tt_adrc type standard table of TY_ADRC .
  types:
    tt_mail type standard table of TY_MAIL .
  types:
    tt_phone type standard table of TY_PHONE .
  types:
    tt_knvv type standard table of TY_KNVV .
  types:
    tt_zoneconthdr type standard table of TY_ZONECONTHDR .
  types:
    tt_tsabt type standard table of TY_TSABT .
  types:
    tt_zcustomerguid type standard table of ty_zcustomerguid .
  types:
    tt_adr6 TYPE STANDARD TABLE OF ty_adr6 .

  data GS_REQUEST type ZSPARKOUR_REQUEST .
  data GS_RESPONSE type ZSPARKOUR_RESPONSE .
  data GS_COMBO type ZMAPVAL1 .
  constants CS_EQ type CHAR02 value 'EQ' ##NO_TEXT.
  constants CS_CP type CHAR02 value 'CP' ##NO_TEXT.
  constants CS_SI type CHAR01 value 'I' ##NO_TEXT.
  data GT_KNA1 type TT_KNA1 .
  data GT_KNVK type TT_KNVK .
  data GS_KNA1 type TY_KNA1 .
  data GS_KNVK type TY_KNVK .
  data GT_ADRC type TT_ADRC .
  data GT_MAIL type TT_MAIL .
  data GT_PHONE type TT_PHONE .
  data GT_KNVV type TT_KNVV .
  data GT_ZONECONTHDR type TT_ZONECONTHDR .
  data GT_TSABT type TT_TSABT .
  data GT_KNVVC type TT_KNVV .
  data GT_ZCUSTOMERGUID type TT_ZCUSTOMERGUID .
  data GT_ADR6 type TT_ADR6 .
  data GT_RANGES type TR_RANGES .

  methods GET_ORGLEAVEL_SEARCH .
  methods GET_CONTACTLEVEL_SEARCH .
  methods GET_ORG_CONTACT_SEARCH .
  methods GET_SPECIAL_SEARCH .
  methods GET_MIXED_SEARCH .
  methods GET_SEARCH_LOGIC
    importing
      value(IM_REQUEST) type STRING optional
    exporting
      !EM_SEARCH type ZMAPVAL1
      !EM_CONT_WITH type CHAR02 .
  methods SET_RESPONSE_DATA
    exporting
      !EM_RESPONSE type ZTPARKOUR_RESPONSE .
  methods GET_KNA1_DATA .
  methods GET_KNVK_DATA .
  methods GET_SERIALNO_DATA .
  methods GET_CONTACT_DATA .
  methods GET_ORDER_DATA .
  methods GET_ADDITIONAL_DATA .
  methods GET_BAMA_DATA .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PARKOUR_CUSTOM_SEARCH IMPLEMENTATION.


  METHOD GET_ADDITIONAL_DATA.

    DATA: _v_orderno   TYPE vbeln,
          _v_kunnrno   TYPE kunnr,
          _v_ktokd     TYPE ktokd,
          _v_posnr     TYPE posnr,
          _v_searchfg  TYPE flag,
          _v_dycond    TYPE string.


    CHECK gt_kna1[] IS NOT INITIAL OR gt_knvk[] IS NOT INITIAL.
    CASE gs_combo.
      WHEN 'ORG_SEARCH'.
        CALL METHOD me->get_knvk_data( ).
      WHEN 'CONTACT_SEARCH'.
      WHEN 'SPECIAL_SEARCH'.
        CALL METHOD me->get_knvk_data( ).
      WHEN 'MIXED_SEARCH'.
      WHEN OTHERS.
    ENDCASE.

    SORT  gt_kna1[] BY kunnr.
    DELETE ADJACENT DUPLICATES FROM gt_kna1[] COMPARING kunnr.
    SORT gt_knvk[] BY kunnr parnr.
    DELETE ADJACENT DUPLICATES FROM gt_knvk[] COMPARING parnr.


**********Country and Region
    SELECT addrnumber country region FROM adrc
             INTO TABLE gt_adrc
             FOR ALL ENTRIES IN gt_kna1
             WHERE addrnumber =  gt_kna1-adrnr
             AND nation = ' '.
    SORT gt_adrc[] BY addrnumber.

**********Email Id's
    SELECT addrnumber consnumber flgdefault flg_nouse smtp_addr FROM adr6
                            INTO TABLE gt_mail
                            FOR ALL ENTRIES IN gt_kna1
                            WHERE addrnumber = gt_kna1-adrnr
                            AND persnumber = ' '.
    SORT gt_mail BY addrnumber.
****Tele Phone Number
    SELECT addrnumber tel_number FROM adr2 INTO TABLE gt_phone
                                 FOR ALL ENTRIES IN gt_kna1
                                 WHERE addrnumber = gt_kna1-adrnr
                                 AND persnumber = ' '.
    SORT gt_phone BY addrnumber.
*-------No formating only number
    LOOP AT gt_phone ASSIGNING FIELD-SYMBOL(<fs_phone>).

      DATA(_l_count)  = strlen( <fs_phone>-tel_number ).
      DO  _l_count  TIMES.
        IF <fs_phone>-tel_number(1) CA '+0123456789'.
          CONCATENATE _v_dycond <fs_phone>-tel_number(1) INTO _v_dycond.
          CONDENSE _v_dycond NO-GAPS.
        ENDIF.
        SHIFT <fs_phone>-tel_number LEFT CIRCULAR.
      ENDDO.
      CLEAR : <fs_phone>-tel_number.
      <fs_phone>-tel_number = _v_dycond.
    ENDLOOP.

*---> Contact
    SELECT kunnr kdgrp FROM knvv INTO TABLE gt_knvv
                       FOR ALL ENTRIES IN gt_kna1
                       WHERE kunnr = gt_kna1-kunnr.
*--->GUI_ID
    IF gs_request-contract IS INITIAL.
      SELECT contract enduser organization_guid FROM zoneconthdr
                                 INTO TABLE gt_zoneconthdr
                                 FOR ALL ENTRIES IN gt_kna1
                                 WHERE enduser = gt_kna1-kunnr.

      SORT gt_zoneconthdr BY enduser.
    ENDIF.

    IF gt_knvk IS NOT INITIAL.
*--->Department Description
      SELECT abtnr vtext FROM tsabt INTO TABLE gt_tsabt
                         FOR ALL ENTRIES IN gt_knvk
                         WHERE spras = 'EN'
                         AND abtnr = gt_knvk-abtnr.
*--->Contact and Customer BP GUID.
      SELECT customer_number guid FROM zcustomerguid INTO TABLE gt_zcustomerguid
                                  FOR ALL ENTRIES IN gt_knvk
                                  WHERE customer_number = gt_knvk-kunnr.
*--->Customer group in header
      SELECT kunnr kdgrp FROM knvv INTO TABLE gt_knvvc
                         FOR ALL ENTRIES IN  gt_knvk
                         WHERE kunnr = gt_knvk-kunnr.

      LOOP AT gt_knvk ASSIGNING FIELD-SYMBOL(<fs_knvk>).
        CLEAR :_l_count ,_v_dycond.
        IF <fs_knvk>-zzguid IS NOT INITIAL.
          <fs_knvk>-zzguid = |{ <fs_knvk>-zzguid }{ '@AdobeID' }|.
        ELSEIF <fs_knvk>-zzguid IS INITIAL.
        ENDIF.
        TRY.
            <fs_knvk>-job_description = gt_tsabt[ abtnr = <fs_knvk>-abtnr ]-vtext.
          CATCH   cx_root INTO DATA(lo_error05).
        ENDTRY.
        _l_count  = strlen( <fs_knvk>-telf1 ).

        DO  _l_count  TIMES.
          IF <fs_knvk>-telf1(1) CA '+0123456789'.
            CONCATENATE _v_dycond <fs_knvk>-telf1(1) INTO _v_dycond.
            CONDENSE _v_dycond NO-GAPS.
          ENDIF.
          SHIFT <fs_knvk>-telf1 LEFT CIRCULAR.
        ENDDO.
        CLEAR : <fs_knvk>-telf1.
        <fs_knvk>-telf1 = _v_dycond.
      ENDLOOP.
    ENDIF.



  ENDMETHOD.


  METHOD get_bama_data.
    DATA : lt_bamaacc  TYPE STANDARD TABLE OF zlicbamaccount,
           lt_bamaacc1 TYPE STANDARD TABLE OF zlicbamaccount,
           ls_bamaacc  TYPE zlicbamaccount,
           im_url      TYPE zbamaurl,
           em_account  TYPE zbamaacctlist_tab,
           em_status   TYPE char2,
           em_message  TYPE string,
           em_bapiret2 TYPE bapiret2_t,
           ls_extmap   TYPE zextmap.

    SELECT SINGLE * FROM zextmap INTO ls_extmap
                                WHERE value_id = 'BAMA_ACCOUNT'
                                  AND s_system = sy-sysid.

    SELECT * FROM zlicbamaccount INTO TABLE lt_bamaacc
                                 WHERE url = gs_request-domainurl
                                   AND cove_options = 'A'.
    IF lt_bamaacc[] IS NOT INITIAL.
      IF gs_request-domainurl CA 'mcl'.
        LOOP AT lt_bamaacc ASSIGNING FIELD-SYMBOL(<fs_bamaacc>).
          REPLACE '.net' IN gs_request-domainurl WITH '*'.
          im_url = gs_request-domainurl.
          CALL FUNCTION 'Z_BAMA_GET_ACCOUNT_LIST' DESTINATION ls_extmap-mapval1
            EXPORTING
              i_url           = im_url
            IMPORTING
              et_bamaacctlist = em_account
              e_status        = em_status
              e_message       = em_message
              et_bapiret2     = em_bapiret2
            EXCEPTIONS
              no_access       = 1
              login_failed    = 2.
          IF em_account IS NOT INITIAL.
            ASSIGN em_account[ 1 ] TO FIELD-SYMBOL(<fs_account>).
            IF sy-subrc = 0.
            ls_bamaacc-accountid = <fs_account>-accountid.
            APPEND ls_bamaacc TO lt_bamaacc1.
            CLEAR: ls_bamaacc.
           ENDIF.
          ENDIF.
        ENDLOOP.
        IF lt_bamaacc1[] IS NOT INITIAL.
          REFRESH lt_bamaacc[].
          SELECT * FROM zlicbamaccount INTO TABLE lt_bamaacc
                                   FOR ALL ENTRIES IN lt_bamaacc1
                                   WHERE accountid = lt_bamaacc1-accountid.
        ENDIF.
      ENDIF.
      IF lt_bamaacc[] IS NOT INITIAL.
        SELECT kunnr
            land1
            name1
            name2
            ort01
            pstlz
            regio
            telf1
            adrnr
            ktokd
            loevm FROM KNA1 INTO TABLE gt_kna1
                       FOR ALL ENTRIES IN lt_bamaacc
                       WHERE kunnr = lt_bamaacc-enduser
                         AND  ktokd IN gt_ranges.
       IF gt_kna1[] IS NOT INITIAL.
        CALL METHOD me->get_knvk_data( ).
       ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_contactlevel_search.
    TYPES: BEGIN OF ty_addressno,
             addrnumber TYPE ad_addrnum,
           END OF ty_addressno,
           BEGIN OF ty_contract,
             parnr TYPE parnr,
           END OF ty_contract.

    DATA: _v_str_email TYPE string,
          _v_str_phone TYPE string,
          _v_email_low TYPE string,
          _v_fl_name   TYPE string,
          _v_smtp_srch TYPE ad_smtpad2,
          _t_addressno TYPE STANDARD TABLE OF ty_addressno,
          _t_contract  TYPE STANDARD TABLE OF ty_contract,
          _w_knvk      TYPE ty_contract,
          _r_condname1 TYPE RANGE OF knvk-namev,
          _r_condname2 TYPE RANGE OF knvk-name1,
          _wa_condname LIKE LINE  OF _r_condname1.


*** Email ID
    IF gs_request-emailid IS NOT INITIAL.
      IF gs_request-org_no IS NOT INITIAL.
        _v_str_email = | { 'KUNNR EQ' } { `'` }{ gs_request-org_no }{ `'` }|.
        _v_str_email = |{  _v_str_email } { 'AND PARAU IN' } { '(' }{ `'` }{ gs_request-emailid }{ `'` }|.
      ELSE.
        _v_str_email = |{ 'PARAU IN' } { '(' }{ `'` }{ gs_request-emailid }{ `'` }|.
      ENDIF.

      _v_email_low = gs_request-emailid.
      TRANSLATE _v_email_low TO UPPER CASE.
      IF _v_email_low NE gs_request-emailid.
        _v_str_email = |{  _v_str_email }{ ',' } { `'` }{ _v_email_low }{ `'` }|.
      ENDIF.
      TRANSLATE _v_email_low TO LOWER CASE.
      IF _v_email_low NE gs_request-emailid.
        _v_str_email = |{  _v_str_email }{ ',' } { `'` }{ _v_email_low }{ `'` }|.
      ENDIF.
      _v_str_email = |{  _v_str_email }{ ')' }|.

      SELECT parnr kunnr namev name1 ort01 adrnd adrnp abtpa abtnr
            telf1 pavip parau zzguid FROM knvk INTO TABLE gt_knvk WHERE (_v_str_email).
      IF gt_knvk[] IS NOT INITIAL.
      call method me->get_kna1_data( ).
      ENDIF.
      EXIT.
    ENDIF.
***Phone Number
    IF gs_request-phoneno IS NOT INITIAL AND gs_request-phoneno <> '0114000001'.
      SELECT addrnumber FROM adr2 INTO TABLE  _t_addressno
                                  WHERE telnr_call = gs_request-phoneno.
****Phone Number Address.
    IF _t_addressno[] IS NOT INITIAL.
      SELECT kunnr land1 name1 name2 ort01 pstlz regio telf1
             adrnr ktokd loevm  FROM kna1 INTO TABLE gt_kna1
               FOR ALL ENTRIES IN _t_addressno
                            WHERE adrnr = _t_addressno-addrnumber
                              AND ktokd IN gt_ranges.
    ENDIF.
**** Org.Level data
    IF gt_knvk[] IS NOT INITIAL.
      Call METHOD me->get_org_contact_search( ).
    ENDIF.
    EXIT.
    ENDIF.

    IF gs_request-first_name IS NOT INITIAL OR  gs_request-last_name IS NOT INITIAL.
      IF gs_request-first_name CA '%' OR  gs_request-last_name CA '%'.
        IF gs_request-first_name IS NOT INITIAL.
          CLEAR : _v_fl_name.
          IF gs_request-first_name CA '*'.
            _wa_condname-sign     = cs_si.
            _wa_condname-option   = cs_cp. "'CP'.
          ELSE.
            _wa_condname-sign     = cs_si.
            _wa_condname-option   = cs_eq.
          ENDIF.
          _wa_condname-low      = gs_request-first_name.
          _v_fl_name            = gs_request-first_name.
          APPEND  _wa_condname TO _r_condname1.
          CLEAR  _wa_condname-low.
          TRANSLATE _v_fl_name TO UPPER CASE.
          IF _v_fl_name = gs_request-first_name.
            TRANSLATE _v_fl_name TO LOWER CASE.
            _wa_condname-low      = gs_request-first_name.
            APPEND  _wa_condname TO _r_condname1.
            CLEAR  _wa_condname-low.
          ENDIF.
        ENDIF.

        IF gs_request-last_name IS NOT INITIAL.
          CLEAR : _v_fl_name, _wa_condname.
          IF gs_request-last_name CA '*'.
            _wa_condname-sign     = cs_si.
            _wa_condname-option   = cs_cp. "'CP'.
          ELSE.
            _wa_condname-sign     = cs_si.
            _wa_condname-option   = cs_eq.
          ENDIF.
          _wa_condname-low      = gs_request-last_name.
          _v_fl_name            = gs_request-last_name.
          APPEND  _wa_condname TO _r_condname1.
          CLEAR  _wa_condname-low.
          TRANSLATE _v_fl_name TO UPPER CASE.
          IF _v_fl_name = gs_request-last_name.
            TRANSLATE _v_fl_name TO LOWER CASE.
            _wa_condname-low      = gs_request-last_name.
            APPEND  _wa_condname TO _r_condname2.
            CLEAR  _wa_condname-low.
          ENDIF.
        ENDIF.
        IF _r_condname1 IS NOT INITIAL AND _r_condname2 IS NOT INITIAL.
          SELECT parnr kunnr namev name1 ort01 adrnd adrnp abtpa abtnr
              telf1 pavip parau zzguid FROM knvk INTO TABLE gt_knvk
                                    WHERE namev IN _r_condname1
                                      AND name1 IN _r_condname2.
        ELSEIF _r_condname1 IS NOT INITIAL AND _r_condname2 IS INITIAL.
          SELECT parnr kunnr namev name1 ort01 adrnd adrnp abtpa abtnr
              telf1 pavip parau zzguid FROM knvk INTO TABLE gt_knvk
                                    WHERE namev IN _r_condname1.

        ELSEIF  _r_condname1 IS INITIAL AND _r_condname2 IS NOT INITIAL.
          SELECT parnr kunnr namev name1 ort01 adrnd adrnp abtpa abtnr
              telf1 pavip parau zzguid FROM knvk INTO TABLE gt_knvk
                                    WHERE name1 IN _r_condname2.

        ENDIF.

      ELSE.
        TRANSLATE gs_request-first_name TO UPPER CASE.
        TRANSLATE gs_request-last_name TO UPPER CASE.
        IF gs_request-first_name CA '*' AND gs_request-last_name CA '*'.
          REPLACE ALL OCCURRENCES OF '*' IN gs_request-first_name WITH '%'.
          REPLACE ALL OCCURRENCES OF '*' IN gs_request-last_name WITH '%'.
          EXEC SQL.
            OPEN dbcur_fname FOR
           select parnr from knvk
           where upper(namev) LIKE :gs_request-first_name
             and upper(name1) LIKE :gs_request-last_name
          ENDEXEC.
        ELSEIF gs_request-first_name CA '*' AND gs_request-last_name IS NOT INITIAL.
          REPLACE ALL OCCURRENCES OF '*' IN gs_request-first_name WITH '%'.
          EXEC SQL.
            OPEN dbcur_fname FOR
           select parnr from knvk
           where upper(namev) LIKE :gs_request-first_name
             and upper(name1) = :gs_request-last_name
          ENDEXEC.
        ELSEIF gs_request-first_name CA '*' AND gs_request-last_name IS INITIAL.
         REPLACE ALL OCCURRENCES OF '*' IN gs_request-first_name WITH '%'.
          EXEC SQL.
            OPEN dbcur_fname FOR
           select parnr from knvk
           where upper(namev) LIKE :gs_request-first_name
          ENDEXEC.

        ELSEIF gs_request-first_name IS NOT INITIAL AND gs_request-last_name CA '*'.
          REPLACE ALL OCCURRENCES OF '*' IN gs_request-last_name WITH '%'.
          EXEC SQL.
            OPEN dbcur_fname FOR
           select parnr from knvk
           where upper(namev) = :gs_request-first_name
             and upper(name1) LIKE :gs_request-last_name
          ENDEXEC.
        ELSEIF gs_request-first_name IS INITIAL AND gs_request-last_name CA '*'.
          REPLACE ALL OCCURRENCES OF '*' IN gs_request-last_name WITH '%'.
          EXEC SQL.
            OPEN dbcur_fname FOR
           select parnr from knvk
           where upper(name1) LIKE :gs_request-last_name
          ENDEXEC.

        ELSEIF gs_request-first_name IS NOT INITIAL AND gs_request-last_name IS NOT INITIAL.
          EXEC SQL.
            OPEN dbcur_fname FOR
           select parnr from knvk
           where upper(namev) = :gs_request-first_name
             and upper(name1) = :gs_request-last_name
          ENDEXEC.

        ELSEIF gs_request-first_name IS INITIAL AND gs_request-last_name IS NOT INITIAL.
          EXEC SQL.
            OPEN dbcur_fname FOR
           select parnr from knvk
           where upper(name1) = :gs_request-last_name
          ENDEXEC.

        ELSEIF gs_request-first_name IS NOT INITIAL AND gs_request-last_name IS INITIAL.
          EXEC SQL.
            OPEN dbcur_fname FOR
           select parnr from knvk
           where upper(namev) = :gs_request-first_name
          ENDEXEC.
        ENDIF.

        DO.
          EXEC SQL.
            FETCH NEXT  dbcur_fname into :_w_knvk-parnr
          ENDEXEC.
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          APPEND _w_knvk TO _t_contract.
        ENDDO.

        EXEC SQL.
          CLOSE dbcur_fname
        ENDEXEC.

        IF _t_contract[] IS NOT INITIAL.
          SELECT parnr kunnr namev name1 ort01 adrnd adrnp abtpa abtnr
                telf1 pavip parau zzguid FROM knvk INTO TABLE gt_knvk
                                      FOR ALL ENTRIES IN _t_contract
                                      WHERE parnr = _t_contract-parnr.
        IF gt_knvk[] IS NOT INITIAL.
        CALL METHOD me->get_kna1_data( ).
        ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  method GET_CONTACT_DATA.
   TYPES : BEGIN OF ty_contract,
           enduser TYPE kunnr,
           END OF ty_contract.
   DATA: _t_contract TYPE STANDARD TABLE OF ty_contract.

       SELECT enduser
         FROM zoneconthdr
         INTO TABLE _t_contract
         WHERE contract = gs_request-contract.
       IF _t_contract[] IS NOT INITIAL .
         SELECT kunnr land1 name1 name2 ort01 pstlz
            regio telf1 adrnr ktokd loevm FROM kna1 INTO TABLE gt_kna1
                        FOR ALL ENTRIES IN _t_contract
                        WHERE kunnr = _t_contract-enduser
                          AND  ktokd IN GT_RANGES.
       ENDIF.

  endmethod.


  method GET_KNA1_DATA.
  CHECK gt_kna1 IS INITIAL AND gt_knvk IS NOT INITIAL.
    SELECT kunnr
            land1
            name1
            name2
            ort01
            pstlz
            regio
            telf1
            adrnr
            ktokd
            loevm FROM KNA1 INTO TABLE gt_kna1
                       FOR ALL ENTRIES IN gt_knvk
                       WHERE kunnr = gt_knvk-kunnr
                         AND  ktokd IN gt_ranges.

  endmethod.


  method GET_KNVK_DATA.

   CHECK gt_kna1 IS NOT INITIAL AND gt_knvk IS INITIAL.
    SELECT * FROM knvk INTO CORRESPONDING FIELDS OF TABLE gt_knvk
                       FOR ALL ENTRIES IN gt_kna1
                       WHERE kunnr = gt_kna1-kunnr.


  endmethod.


  method GET_MIXED_SEARCH.

****Org Name & first name & last name
  IF gs_request-org_name1 IS NOT INITIAL AND
   ( gs_request-first_name IS NOT INITIAL OR gs_request-last_name IS NOT INITIAL ).
    CALL METHOD me->get_orgleavel_search( ).
    CALL METHOD me->get_org_contact_search( ).
  ENDIF.

 IF gs_request-deal_reg_id IS NOT INITIAL AND gs_request-order_number IS NOT INITIAL AND
    gs_request-serial_number IS INITIAL.
     CALL METHOD me->get_order_data( ).
     CALL METHOD me->get_knvk_data( ).
 ELSEIF gs_request-deal_reg_id IS NOT INITIAL AND gs_request-order_number IS NOT INITIAL AND
    gs_request-serial_number IS NOT INITIAL .
    CALL METHOD me->get_serialno_data( ).
    CALL METHOD me->get_knvk_data( ).
 ENDIF.

  endmethod.


  method GET_ORDER_DATA.
      DATA : _v_string  TYPE string.

      TYPES : BEGIN OF ty_customer,
               kunnr TYPE kunnr,
              END OF ty_customer.
      TYPES : BEGIN OF ty_salesorder,
               vbeln TYPE vbeln_va,
              END OF ty_salesorder.

      DATA :_t_customer TYPE STANDARD TABLE OF ty_customer,
            _t_salesorder TYPE STANDARD TABLE OF ty_salesorder.


     IF gs_request-order_number IS NOT INITIAL AND gs_request-deal_reg_id IS INITIAL..
      SELECT kunnr FROM vbpa INTO TABLE _t_customer WHERE vbeln = gs_request-order_number.

     ELSEIF gs_request-order_number IS NOT INITIAL AND gs_request-deal_reg_id IS NOT INITIAL.
      SELECT vbeln FROM vbak INTO TABLE _t_salesorder WHERE vbeln = gs_request-order_number
                                                        and auart <> 'ZAV'
                                                        AND zz_deal_reg_id = gs_request-deal_reg_id.
     ELSEIF gs_request-order_number IS INITIAL AND gs_request-deal_reg_id IS NOT INITIAL.
      SELECT vbeln FROM vbak INTO TABLE _t_salesorder WHERE auart <> 'ZAV'
                                                        AND zz_deal_reg_id = gs_request-deal_reg_id.
     ENDIF.

     IF _t_salesorder[] IS NOT INITIAL.
     SELECT kunnr FROM vbpa INTO TABLE _t_customer
          FOR ALL ENTRIES IN _t_salesorder
        WHERE vbeln = _t_salesorder-vbeln.
     ENDIF.

     IF _t_customer[] IS NOT INITIAL.
         IF gs_request-org_no IS NOT INITIAL.
          DELETE _t_customer[] WHERE kunnr <> gs_request-org_no.
         ENDIF.
       SELECT kunnr land1 name1 name2 ort01 pstlz
            regio telf1 adrnr ktokd loevm FROM kna1 INTO TABLE gt_kna1
                        FOR ALL ENTRIES IN _t_customer
                                     WHERE kunnr = _t_customer-kunnr
                                       AND  ktokd IN gt_ranges.

     ENDIF.

  endmethod.


  METHOD get_orgleavel_search.
    DATA: _r_condname1 TYPE RANGE OF knvk-namev,
          _w_condname  LIKE LINE  OF _r_condname1.....

    IF gs_request-org_name1 IS NOT INITIAL.
      IF gs_request-org_name1 CA '*'.
        _w_condname-sign     = cs_si.
        _w_condname-option   = cs_cp.
      ELSE.
        _w_condname-sign     = cs_si.
        _w_condname-option   = cs_eq.
      ENDIF.
      _w_condname-low      = gs_request-org_name1.
      APPEND  _w_condname TO _r_condname1.

      TRANSLATE gs_request-org_name1 TO UPPER CASE.
      IF _w_condname-low = gs_request-org_name1.
        TRANSLATE gs_request-org_name1 TO LOWER CASE.
        _w_condname-low  = gs_request-org_name1.
      ELSE.
        _w_condname-low      = gs_request-org_name1.
        APPEND  _w_condname TO _r_condname1.
      ENDIF.
    ENDIF.

    IF gs_request-org_no IS NOT INITIAL AND _r_condname1 IS NOT INITIAL.
    SELECT kunnr land1 name1 name2 ort01 pstlz regio
           telf1 adrnr ktokd loevm FROM kna1 INTO TABLE gt_kna1
                     WHERE kunnr = gs_request-org_no
                       AND name1 IN _r_condname1
                       AND  ktokd IN GT_RANGES.
   ELSEIF  gs_request-org_no IS INITIAL AND _r_condname1 IS NOT INITIAL.
    SELECT kunnr land1 name1 name2 ort01 pstlz regio
           telf1 adrnr ktokd loevm FROM kna1 INTO TABLE gt_kna1
                     WHERE name1 IN _r_condname1
                        AND  ktokd IN GT_RANGES..
   ELSEIF  gs_request-org_no IS NOT INITIAL AND _r_condname1 IS INITIAL.
   SELECT kunnr land1 name1 name2 ort01 pstlz regio
           telf1 adrnr ktokd loevm FROM kna1 INTO TABLE gt_kna1
                     WHERE kunnr = gs_request-org_no
                       AND  ktokd IN GT_RANGES.
   ENDIF.

  ENDMETHOD.


  method GET_ORG_CONTACT_SEARCH.
     DATA : _r_condname1 TYPE RANGE OF knvk-namev,
            _r_condname2 TYPE RANGE OF knvk-name1,
            _wa_condname LIKE LINE  OF _r_condname1,
            _v_fl_name   TYPE string.

     CHECK gt_kna1[] IS NOT INITIAL.
      IF gs_request-first_name IS NOT INITIAL OR  gs_request-last_name IS NOT INITIAL.
        IF gs_request-first_name IS NOT INITIAL.
          CLEAR : _v_fl_name.
          IF gs_request-first_name CA '*'.
            _wa_condname-sign     = cs_si.
            _wa_condname-option   = cs_cp. "'CP'.
          ELSE.
            _wa_condname-sign     = cs_si.
            _wa_condname-option   = cs_eq.
          ENDIF.
          _wa_condname-low      = gs_request-first_name.
          _v_fl_name            = gs_request-first_name.
          APPEND  _wa_condname TO _r_condname1.
          CLEAR  _wa_condname-low.
          TRANSLATE _v_fl_name TO UPPER CASE.
          IF _v_fl_name = gs_request-first_name.
            TRANSLATE _v_fl_name TO LOWER CASE.
            _wa_condname-low      = gs_request-first_name.
            APPEND  _wa_condname TO _r_condname1.
            CLEAR  _wa_condname-low.
          ENDIF.
        ENDIF.

        IF gs_request-last_name IS NOT INITIAL.
          CLEAR : _v_fl_name, _wa_condname.
          IF gs_request-last_name CA '*'.
            _wa_condname-sign     = cs_si.
            _wa_condname-option   = cs_cp. "'CP'.
          ELSE.
            _wa_condname-sign     = cs_si.
            _wa_condname-option   = cs_eq.
          ENDIF.
          _wa_condname-low      = gs_request-last_name.
          _v_fl_name            = gs_request-last_name.
          APPEND  _wa_condname TO _r_condname1.
          CLEAR  _wa_condname-low.
          TRANSLATE _v_fl_name TO UPPER CASE.
          IF _v_fl_name = gs_request-last_name.
            TRANSLATE _v_fl_name TO LOWER CASE.
            _wa_condname-low      = gs_request-last_name.
            APPEND  _wa_condname TO _r_condname2.
            CLEAR  _wa_condname-low.
          ENDIF.
        ENDIF.
        IF _r_condname1 IS NOT INITIAL AND _r_condname2 IS NOT INITIAL.
          SELECT parnr kunnr namev name1 ort01 adrnd adrnp abtpa abtnr
              telf1 pavip parau zzguid FROM knvk INTO TABLE gt_knvk
                                    FOR ALL ENTRIES IN gt_kna1
                                    WHERE kunnr = gt_kna1-kunnr
                                      AND namev IN _r_condname1
                                      AND name1 IN _r_condname2.
        ELSEIF _r_condname1 IS NOT INITIAL AND _r_condname2 IS INITIAL.
          SELECT parnr kunnr namev name1 ort01 adrnd adrnp abtpa abtnr
              telf1 pavip parau zzguid FROM knvk INTO TABLE gt_knvk
                                    FOR ALL ENTRIES IN gt_kna1
                                    WHERE kunnr = gt_kna1-kunnr
                                      AND namev IN _r_condname1.

        ELSEIF  _r_condname1 IS INITIAL AND _r_condname2 IS NOT INITIAL.
          SELECT parnr kunnr namev name1 ort01 adrnd adrnp abtpa abtnr
              telf1 pavip parau zzguid FROM knvk INTO TABLE gt_knvk
                                      FOR ALL ENTRIES IN gt_kna1
                                    WHERE kunnr = gt_kna1-kunnr
                                      AND name1 IN _r_condname2.

        ENDIF.
     ENDIF.


  endmethod.


  METHOD get_search_logic.
    DATA: _v_data     TYPE string,
          _s_stru     TYPE REF TO cl_abap_structdescr,
          _l_col_name LIKE LINE OF _s_stru->components,
          _s_combo    TYPE zparkour_combo,
          _t_table    TYPE STANDARD TABLE OF zparkour_combo,
          _s_table    TYPE zparkour_table,
          _struct     TYPE REF TO data,
          _t_ranges   TYPE LINE  OF tr_ranges.

    FIELD-SYMBOLS: <gs_request> TYPE any,
                   <cell>       TYPE data.
* Input Structure
    CREATE DATA _struct TYPE zsparkour_request.
    ASSIGN _struct->* TO <gs_request>.

    DATA(_o_json) =  zcl_json_document=>create_with_json( EXPORTING json = im_request ).
    TRY.
        _o_json->get_data( IMPORTING data = <gs_request> ).
      CATCH zcx_json_document.
    ENDTRY.

    CHECK  <gs_request> IS NOT INITIAL.
    MOVE-CORRESPONDING  <gs_request> TO gs_request.
    IF gs_request-org_no IS NOT INITIAL.
      gs_request-org_no = |{ gs_request-org_no ALPHA = IN }|.
    ENDIF.
    IF gs_request-order_number IS NOT INITIAL.
    gs_request-order_number = |{ gs_request-order_number ALPHA = IN }|.
   ENDIF.

    _s_stru ?= cl_abap_typedescr=>describe_by_data(  <gs_request> ).
    LOOP AT _s_stru->components INTO _l_col_name.
      ASSIGN COMPONENT _l_col_name-name OF STRUCTURE  <gs_request> TO <cell>.
      _v_data =  <cell>.
      IF _v_data IS NOT INITIAL AND _l_col_name-name <> 'NO_OF_RECORDS'.
        IF _s_combo-field01 IS INITIAL.
          _s_combo-field01 = _l_col_name-name.
        ELSEIF _s_combo-field02 IS INITIAL.
          _s_combo-field02 = _l_col_name-name.
        ELSEIF _s_combo-field03 IS INITIAL.
          _s_combo-field03 = _l_col_name-name.
        ELSEIF _s_combo-field04 IS INITIAL.
          _s_combo-field04 = _l_col_name-name.
        ELSEIF _s_combo-field05 IS INITIAL.
          _s_combo-field05 = _l_col_name-name.
        ELSEIF _s_combo-field06 IS INITIAL.
          _s_combo-field06 = _l_col_name-name.
        ELSEIF _s_combo-field07 IS INITIAL.
          _s_combo-field07 = _l_col_name-name.
        ELSEIF _s_combo-field08 IS INITIAL.
          _s_combo-field08 = _l_col_name-name.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CHECK _s_combo IS NOT INITIAL.
    SELECT SINGLE search_seq tableseq FROM zparkour_combo
                           INTO ( em_search, em_cont_with )
                          WHERE field01 = _s_combo-field01
                            AND field02 = _s_combo-field02
                            AND field03 = _s_combo-field03
                            AND field04 = _s_combo-field04
                            AND field05 = _s_combo-field05
                            AND field06 = _s_combo-field06
                            AND field07 = _s_combo-field07
                            AND field08 = _s_combo-field08.
    IF sy-subrc = 0.
       gs_combo = em_search.
    ENDIF.



      SELECT * FROM zparkour_combo INTO TABLE _t_table
                                 WHERE field01 = 'ACCOUNT_GROUP'.

    IF _t_table[] IS NOT INITIAL.

    LOOP AT _t_table ASSIGNING FIELD-SYMBOL(<fs_table>).
     _t_ranges-sign = 'I'.
     _t_ranges-option = 'EQ'.
     _t_ranges-low = <fs_table>-field02.
     APPEND  _t_ranges TO  GT_RANGES .
     CLEAR :  _t_ranges.
    ENDLOOP.
 ENDIF.

  ENDMETHOD.


  METHOD get_serialno_data.

    TYPES : BEGIN OF ty_serialno ,
             salesorder TYPE vbeln_va,
             soldto     TYPE kunnr,
           END OF ty_serialno,

           BEGIN OF ty_salesorder,
            vbeln TYPE vbeln_va,
            kunnr TYPE kunnr,
          END OF ty_salesorder.

  DATA: _t_serialno TYPE STANDARD TABLE OF ty_serialno,
        _t_salesorder TYPE STANDARD TABLE OF ty_salesorder.

    SELECT zliccerthdr~salesorder
           zliccerthdr~soldto INTO TABLE _t_serialno
                          FROM zliccertsn
                          INNER JOIN zliccerthdr
                          ON ( zliccerthdr~certificate = zliccertsn~certificate
                          AND  zliccerthdr~sub_id = zliccertsn~sub_id )
                          WHERE zliccertsn~serial_number = gs_request-serial_number
                            AND zliccerthdr~salesorder   = gs_request-order_number.

    IF gs_request-deal_reg_id IS NOT INITIAL AND _t_serialno[] IS NOT INITIAL.
      IF gs_request-org_no IS INITIAL.
      SELECT vbeln kunnr FROM vbak INTO TABLE _t_salesorder
                         FOR ALL ENTRIES IN _t_serialno
                               WHERE vbeln = _t_serialno-salesorder
                                 AND kunnr = _t_serialno-soldto
                                 AND zz_deal_reg_id = gs_request-deal_reg_id.
     ELSE.
      SELECT vbeln kunnr FROM vbpa INTO TABLE _t_salesorder
                         FOR ALL ENTRIES IN _t_serialno
                               WHERE vbeln = _t_serialno-salesorder
                                 AND kunnr = gs_request-org_no.

     ENDIF.
    ENDIF.

    IF _t_salesorder IS  NOT INITIAL.
        SELECT kunnr land1 name1 name2 ort01 pstlz
               regio telf1 adrnr ktokd loevm FROM kna1 INTO TABLE gt_kna1
                           FOR ALL ENTRIES IN _t_salesorder
                           WHERE kunnr = _t_salesorder-kunnr
                             AND  ktokd IN GT_RANGES.


    ELSEIF _t_serialno[] IS NOT INITIAL.
      SELECT kunnr land1 name1 name2 ort01 pstlz
               regio telf1 adrnr ktokd loevm FROM kna1 INTO TABLE gt_kna1
                         FOR ALL ENTRIES IN _t_serialno
                         WHERE kunnr = _t_serialno-soldto
                           AND ktokd IN GT_RANGES..

    ENDIF.

  ENDMETHOD.


  METHOD get_special_search.
    DATA: _v_string TYPE string.
*** Serial Number
    IF gs_request-serial_number IS NOT INITIAL .
      CALL METHOD me->get_serialno_data( ).
*** Contact VIP ID
   ELSEIF gs_request-contract IS NOT INITIAL.
     CALL METHOD me->get_contact_data( ).
***Order Number or  Deal reg. number
   ELSEIF gs_request-order_number IS NOT INITIAL OR gs_request-deal_reg_id IS NOT INITIAL.
      CALL METHOD me->get_order_data( ).
    ELSEIF gs_request-domainurl IS NOT INITIAL.
      CALL METHOD me->get_bama_data( ).
    ENDIF.

  ENDMETHOD.


  method SET_RESPONSE_DATA.
DATA:  _p_output_response TYPE zsparkour_response,
       _t_email_id        TYPE zttparkour_email_id,
       _t_phone_no        TYPE zttparkour_phone_det,
       _t_contact         TYPE STANDARD TABLE OF zscontacts.


CHECK gt_kna1[] IS NOT INITIAL.
     LOOP AT gt_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>).
       IF <fs_kna1>-name1 EQ 'DELETED' OR <fs_kna1>-name2 EQ 'DELETED'.
        CONTINUE.
       ENDIF.
       REFRESH : _t_email_id , _t_phone_no,_t_contact.
       TRY.
         _p_output_response-customer_group  = gt_knvv[ kunnr = <fs_kna1>-kunnr ]-kdgrp.
         CATCH cx_root INTO DATA(lo_error02).
       ENDTRY.
       _p_output_response-request_id   = gs_request-request_id.
       _p_output_response-source       = 'ECC'.
       _p_output_response-org_no       = |{ <fs_kna1>-kunnr ALPHA = IN }|.
       _p_output_response-org_name1    = <fs_kna1>-name1.
       _p_output_response-org_name2    = <fs_kna1>-name2.
       _p_output_response-account_group  = <fs_kna1>-ktokd.
       _p_output_response-city           = <fs_kna1>-ort01.

       _t_email_id = VALUE #( FOR wa_mail IN gt_mail WHERE ( addrnumber = <fs_kna1>-adrnr )
                            ( consnumber  = wa_mail-consnumber
                              smtp_addr   = wa_mail-smtp_addr ) ).

       _t_phone_no = VALUE #( FOR wa_phone IN gt_phone WHERE ( addrnumber = <fs_kna1>-adrnr )
                             ( phone_number = wa_phone-tel_number ) ).

       TRY.
           ASSIGN gt_adrc[ addrnumber = <fs_kna1>-adrnr ] TO FIELD-SYMBOL(<fs_adrc>).
           IF sy-subrc = 0.
             _p_output_response-regions =  <fs_adrc>-region.
             _p_output_response-country  = <fs_adrc>-country.
           ENDIF.
         CATCH cx_root INTO DATA(lo_error01).
       ENDTRY.

       TRY.
         ASSIGN gt_zoneconthdr[ enduser = <fs_kna1>-kunnr ] TO FIELD-SYMBOL(<fs_onecon>).
         IF sy-subrc = 0.
           _p_output_response-org_guid = <fs_onecon>-organization_guid.
           _p_output_response-contract = <fs_onecon>-contract.
         ENDIF.
         CATCH   cx_root INTO DATA(lo_error04).
       ENDTRY.

       _p_output_response-emails  = _t_email_id.
       _p_output_response-phoneno = _t_phone_no.

       _t_contact = VALUE #( FOR wa_knvk IN gt_knvk WHERE ( kunnr = <fs_kna1>-kunnr )
                            (    contact_no   =  wa_knvk-parnr
                                 first_name   =  wa_knvk-namev
                                 last_name    =  wa_knvk-name1
                                 job_function =  wa_knvk-abtnr
                                 job_description = wa_knvk-job_description
                                 email        =  wa_knvk-parau
                                 phoneno      =  wa_knvk-telf1
                                 rengaid      =  wa_knvk-zzguid ) ).

       _p_output_response-contacts = _t_contact.
       APPEND _p_output_response TO em_response.
       CLEAR _p_output_response.
     ENDLOOP.

  endmethod.
ENDCLASS.
