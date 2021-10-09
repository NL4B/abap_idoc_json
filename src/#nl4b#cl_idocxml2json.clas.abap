class /NL4B/CL_IDOCXML2JSON definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  types TT_SEGMENT type DCXMLIDSEG .
  types:
    tt_xmltable TYPE STANDARD TABLE OF smum_xmltb WITH DEFAULT KEY .
  types TS_XMLTABLE type SMUM_XMLTB .
  types:
    BEGIN OF ts_map_idoc_names,
             xmlsegment TYPE char30,
             xmlfield   TYPE char30,
             jsonname   TYPE text30.
    TYPES: END OF ts_map_idoc_names .
  types:
    tt_map_idoc_names TYPE STANDARD TABLE OF ts_map_idoc_names WITH DEFAULT KEY .

  methods CONVERT_IDOC_XML_TO_JSON
    importing
      value(I_IDOC_XML) type XSTRING
      value(I_INCLUDE_CONTROL_RECORD) type XFELD default SPACE
      value(I_MAP_NAMES_TAB) type STRING optional
    returning
      value(R_JSON) type XSTRING .
  methods CONVERT_JSON_STRING_TO_XSTRING
    importing
      value(I_JSON_STRING) type STRING
    returning
      value(R_JSON_XSTRING) type XSTRING .
  PROTECTED SECTION.
private section.

  data XMLTABLE_T type TT_XMLTABLE .
  data EDIDC40_R type EDI_DC40 .

  methods GET_IDOC_CONTROL_FROM_IDOCXML
    importing
      value(I_IDOC_XML) type XSTRING
    returning
      value(R_EDIDC40) type EDI_DC40 .
  methods GET_SEGMENTS_FROM_IDOC_CONTROL
    importing
      !I_EDIDC40 type EDI_DC40
    returning
      value(R_SEGMENT_T) type TT_SEGMENT .
  methods GET_XMLTABLE_FROM_IDOCXML
    importing
      value(I_IDOC_XML) type XSTRING
    returning
      value(R_XMLTABLE) type TT_XMLTABLE .
ENDCLASS.



CLASS /NL4B/CL_IDOCXML2JSON IMPLEMENTATION.


  METHOD convert_idoc_xml_to_json.

    DATA tag_closing_stack_t TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
    DATA segment_stack_t TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    DATA(edidc40_r) = get_idoc_control_from_idocxml( EXPORTING i_idoc_xml = i_idoc_xml ).
    DATA(segment_t) = get_segments_from_idoc_control( EXPORTING i_edidc40 = edidc40_r ).
    DATA(xmltable_t) = get_xmltable_from_idocxml( EXPORTING i_idoc_xml = i_idoc_xml ).

    DELETE xmltable_t WHERE hier < 3.

    DATA(json_string) = |\{|.
    INSERT |\}| INTO tag_closing_stack_t INDEX 1.
    IF i_include_control_record NE space.
      json_string = |{ json_string } "IDOC":\{|.
      INSERT |\}| INTO tag_closing_stack_t INDEX 1.
    ENDIF.

    LOOP AT xmltable_t ASSIGNING FIELD-SYMBOL(<xmltable_r>).
      AT FIRST.

      ENDAT.

    ENDLOOP.
    if sy-subrc eq 0.
      loop at tag_closing_stack_t assigning field-symbol(<tag>).
        json_string = |{ json_string }{ <tag> }|.
      endloop.
    endif.


  ENDMETHOD.


  METHOD convert_json_string_to_xstring.
    CLEAR r_json_xstring.
    DATA(is_json) = /ui5/cl_json_util=>is_wellformed( i_json_string ).
    IF is_json EQ abap_true.
      TRY.
          DATA(conv) = cl_abap_conv_out_ce=>create( encoding = 'UTF-8' ignore_cerr = 'X' ).
          conv->write( data = i_json_string ).
          r_json_xstring = conv->get_buffer( ).
        CATCH cx_root INTO DATA(ex).
*        Raise exception

      ENDTRY.
    ELSE.
*     Raise exception
    ENDIF.


  ENDMETHOD.


  METHOD get_idoc_control_from_idocxml.

    IF me->edidc40_r IS INITIAL.

      DATA(xmltable_t) = get_xmltable_from_idocxml( EXPORTING i_idoc_xml = i_idoc_xml ).
      READ TABLE xmltable_t INTO DATA(xmltable_r) WITH KEY hier = 2.
      IF sy-subrc NE 0 OR xmltable_r-cname NE 'IDOC'.
*     Raise Exception no IDOC-XML file
        RETURN.
      ENDIF.
      DELETE xmltable_t WHERE hier < 3.

      CLEAR r_edidc40.

      LOOP AT xmltable_t ASSIGNING FIELD-SYMBOL(<xmltable_r>).
        AT FIRST.
          IF <xmltable_r>-cname NE 'EDI_DC40'.
*        exit, not a xml-idoc.
            EXIT.
          ENDIF.

          CONTINUE.
        ENDAT.
        CHECK <xmltable_r>-type NE 'A'.
        IF <xmltable_r>-type EQ space.
*       new segment, exit loop.
          EXIT.
        ENDIF.
        IF <xmltable_r>-type EQ 'V'.
          DATA(fieldname) = |R_EDIDC40-{  <xmltable_r>-cname }|.
          ASSIGN (fieldname) TO FIELD-SYMBOL(<field>).
          <field> =  <xmltable_r>-cvalue.
        ENDIF.
      ENDLOOP.
      IF sy-subrc NE 0 OR  r_edidc40 IS INITIAL.
*     raise exception
      ENDIF.
      me->edidc40_r =  r_edidc40.
    ELSE.
      r_edidc40 = me->edidc40_r.
    ENDIF.

  ENDMETHOD.


  method GET_SEGMENTS_FROM_IDOC_CONTROL.
  endmethod.


  METHOD get_xmltable_from_idocxml.
    DATA: bapiret2_t TYPE bapiret2_t.

    IF me->xmltable_t IS INITIAL.
      r_xmltable =  me->xmltable_t.
      RETURN.
    ELSE.
      CLEAR r_xmltable.
      CALL FUNCTION 'SMUM_XML_PARSE'
        EXPORTING
          xml_input = i_idoc_xml
        TABLES
          xml_table = me->xmltable_t
          return    = bapiret2_t.

      IF lines( xmltable_t ) = 0.
        CLEAR me->xmltable_t.
*     Raise Exception no XML file
      ELSE.
        r_xmltable = me->xmltable_t.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD if_http_extension~handle_request.
  data(request) = server->request.
  data(payload) = request->get_data( ).
  ENDMETHOD.
ENDCLASS.
