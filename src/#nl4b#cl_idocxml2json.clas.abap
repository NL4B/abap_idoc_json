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
      value(R_JSON) type XSTRING
    raising
      /NL4B/CX_IDOCXML2JSON .
  methods CONVERT_JSON_STRING_TO_XSTRING
    importing
      value(I_JSON_STRING) type STRING
    returning
      value(R_JSON_XSTRING) type XSTRING .
  methods CONVERT_IDOC_TO_IDOC_XML
    importing
      value(I_DOCNUM) type EDI_DOCNUM
    returning
      value(R_IDOC_XML) type XSTRING .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA xmltable_t TYPE tt_xmltable .
    DATA edidc40_r TYPE edi_dc40 .

    METHODS get_idoc_control_from_idocxml
      IMPORTING
        VALUE(i_idoc_xml) TYPE xstring
      RETURNING
        VALUE(r_edidc40)  TYPE edi_dc40 .
    METHODS get_segments_from_idoc_control
      IMPORTING
        !i_edidc40         TYPE edi_dc40
      RETURNING
        VALUE(r_segment_t) TYPE tt_segment .
    METHODS get_xmltable_from_idocxml
      IMPORTING
        VALUE(i_idoc_xml) TYPE xstring
      RETURNING
        VALUE(r_xmltable) TYPE tt_xmltable .
    CLASS-METHODS xml_processing_get_element
      IMPORTING
        VALUE(i_node) TYPE REF TO if_ixml_node
      CHANGING
        !c_xmltable   TYPE tt_xmltable .
    CLASS-METHODS xml_processing_parsing
      IMPORTING
        !i_xml            TYPE xstring
      RETURNING
        VALUE(r_xmltable) TYPE tt_xmltable .
ENDCLASS.



CLASS /NL4B/CL_IDOCXML2JSON IMPLEMENTATION.


  METHOD convert_idoc_to_idoc_xml.

    DATA: idoc TYPE REF TO cl_idoc_xml1.

    CREATE OBJECT idoc
      EXPORTING
        docnum             = i_docnum
      EXCEPTIONS
        error_loading_idoc = 1
        error_building_xml = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
*   raise exception.
      RETURN.
    ENDIF.

    CALL METHOD idoc->get_xmldata_as_xstring
      IMPORTING
        data_string = r_idoc_xml.

  ENDMETHOD.


  METHOD convert_idoc_xml_to_json.

    DATA tag_closing_stack_t TYPE STANDARD TABLE OF string.
    DATA segment_stack_t TYPE STANDARD TABLE OF string.

    DATA(map_is_supplied) = abap_false.
    IF i_map_names_tab IS SUPPLIED.
      map_is_supplied = abap_true.
    ENDIF.

    DATA(edidc40_r) = get_idoc_control_from_idocxml( EXPORTING i_idoc_xml = i_idoc_xml ).
    DATA(segment_t) = get_segments_from_idoc_control( EXPORTING i_edidc40 = edidc40_r ).
    DATA(xmltable_t) = get_xmltable_from_idocxml( EXPORTING i_idoc_xml = i_idoc_xml ).
    DATA(previous_type) = space.
    DATA(tag_level) = -1.
    DATA previous_tag TYPE string.
    DATA tag_name TYPE string.

    LOOP AT segment_t ASSIGNING FIELD-SYMBOL(<seg>).
      LOOP AT xmltable_t ASSIGNING FIELD-SYMBOL(<line>) WHERE cname EQ <seg>-segmenttyp AND type EQ 'V'.
        <line>-type = space.
      ENDLOOP.
    ENDLOOP.

    DELETE xmltable_t WHERE hier < 3.

    DATA(json_string) = |\{|.
    INSERT |\}| INTO tag_closing_stack_t INDEX 1.
    IF i_include_control_record NE space.
      json_string = |{ json_string } "IDOC":\{|.
      INSERT |\}| INTO tag_closing_stack_t INDEX 1.
    ENDIF.

    LOOP AT xmltable_t ASSIGNING FIELD-SYMBOL(<xmltable_r>).
      DATA(hier) = <xmltable_r>-hier - 2.
      AT FIRST.
        DATA(is_first_segm_field) = abap_true.
        DATA(is_first) = abap_true.
        IF <xmltable_r>-type EQ space AND hier EQ 1.
          DATA(level_up)   = hier + 1.
          DATA(level_down) = hier - 1.
          DATA(level_segment) = hier.
          DATA(current_segment) = <xmltable_r>-cname.
        ELSE.
*        raise exception, not a starting segment
          EXIT.
        ENDIF.
      ENDAT.

*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*     Get xml tag name
*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tag_name = <xmltable_r>-cname.

*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*     Process a xml attribute
*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF <xmltable_r>-type EQ 'A' AND previous_type EQ space.
        previous_type = <xmltable_r>-type.
        CONTINUE.
      ENDIF.

*      IF <xmltable_r>-type EQ 'A' AND previous_type EQ 'V'.
*        current_segment = previous_tag.
*
*        json_string = substring( val = json_string off = 0 len = strlen( json_string ) - 1 ).
*
*
*        READ TABLE segment_t INTO DATA(segment_r) WITH KEY segmenttyp = previous_tag.
*        IF sy-subrc EQ 0.
*          IF CONV i( segment_r-occmax ) > 1.
*            json_string = |{ json_string }[]|.
*          ELSE.
*            json_string = |{ json_string }\{\}|.
*          ENDIF.
*        ENDIF.
*        previous_type = <xmltable_r>-type.
*        CONTINUE.
*      ENDIF.

*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*     Process a xml segment
*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF <xmltable_r>-type EQ space.
        IF tag_name EQ 'EDI_DC40'.
          IF i_include_control_record EQ space.
            previous_type = <xmltable_r>-type.
            DATA(ignore_current_segment) = abap_true.
            CONTINUE.
          ENDIF.
          DATA(is_json_array) = abap_false.


        ELSEIF tag_name EQ 'EDI_DS40'.
          previous_type = <xmltable_r>-type.
          ignore_current_segment = abap_true.
          current_segment = tag_name .
          CONTINUE.
        ELSE.
          READ TABLE segment_t INTO DATA(segment_r) WITH KEY segmenttyp = tag_name.
          IF sy-subrc EQ 0.
            IF CONV i( segment_r-occmax ) > 1.
              is_json_array = abap_true.
            ELSE.
              is_json_array = abap_false.
            ENDIF.
          ELSE.
            RAISE EXCEPTION TYPE /nl4b/cx_idocxml2json
              EXPORTING
                textid = /nl4b/cx_idocxml2json=>no_xml.
          ENDIF.

        ENDIF.

        IF  map_is_supplied EQ abap_true.


        ELSE.
          ignore_current_segment = abap_false.
*          current_segment = tag_name .
        ENDIF.


        IF ignore_current_segment EQ abap_true.
          CONTINUE.
        ENDIF.


        IF is_first_segm_field EQ abap_true AND is_first EQ abap_true.
          INSERT current_segment INTO segment_stack_t INDEX 1.
          IF  is_json_array EQ abap_true.
            json_string = |{ json_string } "{ tag_name }":[\{|.
            INSERT |\}]| INTO tag_closing_stack_t INDEX 1.
          ELSE.
            json_string = |{ json_string } "{ tag_name }":\{|.
            INSERT |\}| INTO tag_closing_stack_t INDEX 1.
          ENDIF.
          current_segment = tag_name.
          is_first = abap_false.
          CONTINUE.
        ENDIF.

        IF current_segment EQ tag_name AND level_segment = hier.
          json_string = |{ json_string }\},\{|.
        ENDIF.

        IF current_segment NE tag_name AND level_segment = hier.
          READ TABLE tag_closing_stack_t INTO DATA(tag) INDEX 1.
          IF sy-subrc EQ 0.
            json_string = |{ json_string }{ tag } |.
            DELETE tag_closing_stack_t INDEX 1.
          ENDIF.
          DELETE segment_stack_t INDEX 1.
          current_segment = tag_name.
          INSERT current_segment INTO segment_stack_t INDEX 1.
          IF  is_json_array EQ abap_true.
            json_string = |{ json_string }, "{ tag_name }":[\{|.
            INSERT |\}]| INTO tag_closing_stack_t INDEX 1.
          ELSE.
            json_string = |{ json_string }, "{ tag_name }":\{|.
            INSERT |\}| INTO tag_closing_stack_t INDEX 1.
          ENDIF.
        ENDIF.

        IF current_segment NE tag_name AND level_up = hier.
          level_segment = hier.
          current_segment = tag_name.
          INSERT current_segment INTO segment_stack_t INDEX 1.
          IF  is_json_array EQ abap_true.
            json_string = |{ json_string }, "{ tag_name }":[\{|.
            INSERT |\}]| INTO tag_closing_stack_t INDEX 1.
          ELSE.
            json_string = |{ json_string }, "{ tag_name }":\{|.
            INSERT |\}| INTO tag_closing_stack_t INDEX 1.
          ENDIF.
        ENDIF.

        IF current_segment NE tag_name AND hier < level_segment.
          DATA(steps_back) = level_segment - hier.
          DO steps_back TIMES.
            READ TABLE tag_closing_stack_t INTO tag INDEX 1.
            IF sy-subrc EQ 0.
              json_string = |{ json_string }{ tag }|.
              DELETE tag_closing_stack_t INDEX 1.
            ENDIF.
            DELETE segment_stack_t INDEX 1.
          ENDDO.
          level_segment = hier.
          current_segment = tag_name.
          INSERT current_segment INTO segment_stack_t INDEX 1.
          IF  is_json_array EQ abap_true.
            json_string = |{ json_string }, "{ tag_name }":[\{|.
            INSERT |\}]| INTO tag_closing_stack_t INDEX 1.
          ELSE.
            json_string = |{ json_string }, "{ tag_name }":\{|.
            INSERT |\}| INTO tag_closing_stack_t INDEX 1.
          ENDIF.
          is_first_segm_field = abap_true.
        ENDIF.
      ENDIF.
      IF ignore_current_segment EQ abap_true.
        CONTINUE.
      ENDIF.

*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*     Process a xml value
*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      IF <xmltable_r>-type EQ 'V'.

        previous_type = <xmltable_r>-type.
        IF ignore_current_segment EQ abap_true.
          CONTINUE.
        ENDIF.
        IF i_include_control_record EQ space AND current_segment EQ |EDI_DC40|.
          CONTINUE.
        ENDIF.
        IF  map_is_supplied EQ abap_true.





        ENDIF.

        IF previous_type EQ 'V' AND tag_level EQ hier AND is_first_segm_field EQ abap_false.
*         IF previous_type EQ 'V' AND is_first_segm_field EQ abap_false.
          json_string = |{ json_string }, |.
        ENDIF.
        json_string = |{ json_string }"{ tag_name }":"{ <xmltable_r>-cvalue }" |.
        is_first_segm_field = abap_false.
      ENDIF.
      tag_level = hier.
      previous_tag = tag_name.
    ENDLOOP.
    IF sy-subrc EQ 0.
*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*     Adding closing tages
*     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      LOOP AT tag_closing_stack_t ASSIGNING FIELD-SYMBOL(<tag>).
        json_string = |{ json_string }{ <tag> }|.
      ENDLOOP.
    ENDIF.


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


*    DATA(ixml) = cl_ixml=>create( ).
*    DATA(stream_factory) = ixml->create_stream_factory( ).
*    DATA(istream) = stream_factory->create_istream_xstring( i_idoc_xml ).
*    DATA(document) = ixml->create_document( ).
*    DATA(parser) = ixml->create_parser(
*                             document = document
*                             stream_factory = stream_factory
*                             istream = istream
*                          ).
*    IF parser->parse( ) NE 0.
*      IF parser->num_errors( ) NE 0.
*
*
*
*
*
*
*
*
*      ENDIF.
*      EXIT.
*    ENDIF.
*    CALL METHOD istream->close( ).
*    CLEAR istream.
*
*    DATA(iterator) = document->create_iterator( ).
*    DATA node TYPE REF TO if_ixml_node.
*
*    node = iterator->get_next( ).
*
*    DATA(node_type)  = node->get_type( ).
*    DATA(node_name)  = node->get_name( ).
*    DATA(node_value) = node->get_value( ).
*    DATA(is_root) = node->is_root(  ).
*
*    IF ( node->get_name( ) = '#document' AND node->get_type( ) = 1 AND node->is_root( ) EQ abap_true ).
*      DATA(node_list) = node->get_children( ).
*      IF node_list->get_length( ) EQ 1.
*        DATA(idoc_node) = node_list->get_item( index = 0 ).
*        DATA(idoc_node_type)  = idoc_node->get_type( ). "4
*        DATA(idoc_node_name)  = idoc_node->get_name( ). "BANK_SAVEREPLICA01
*        DATA(idoc_node_list)  = idoc_node->get_children( ).
*        DATA(list_lenght) = idoc_node_list->get_length( ). "1
*        IF idoc_node_list->get_length( ) = 1 .
*          DATA(idoc) = idoc_node_list->get_item( index = 0 ).
*          DATA(idoc_type)  = idoc->get_type( ).  "4
*          DATA(idoc_name)  = idoc->get_name( ).  "IDOC
*          DATA(idoc_node_records)  = idoc->get_children( ).
*          IF idoc_node_records->get_length( ) > 0 .
*            DATA(edi_dc40_node) = idoc_node_records->get_item( index = 0 ).
*            DATA(edi_dc40_node_type)  = edi_dc40_node->get_type( ).  "4
*            DATA(edi_dc40_node_name)  = edi_dc40_node->get_name( ).  "EDI_DC40
*
*          ELSE.
*
**         Document has multiple documents, only one document supported
*          ENDIF.
*
*
*        ELSE.
**         Document has multiple documents, only one document supported
*          RETURN.
*        ENDIF.
*
*      ENDIF.
*
*
*
*    ENDIF.
*
*    iterator = edi_dc40_node->CREATE_ITERATOR( ).
*    DO.
*      node = iterator->get_next( ).
*      IF node IS INITIAL.
*        EXIT.
*      ENDIF.
*      node_type  = node->get_type( ).
*    node_name  = node->get_name( ).
*    node_value = node->get_value( ).
*
*      IF node->get_type( ) = if_ixml_node=>co_node_text.
*        node->set_value( to_upper( node->get_value( ) ) ).
*      ENDIF.
*    ENDDO.



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


  METHOD get_segments_from_idoc_control.

    CALL FUNCTION 'IDOCTYPE_READ_COMPLETE'
      EXPORTING
        pi_idoctyp         = i_edidc40-idoctyp
        pi_cimtyp          = i_edidc40-cimtyp
*       PI_RELEASE         = SY-SAPRL
*       PI_APPLREL         =
*       PI_VERSION         = '3'
*       PI_READ_UNREL      =
*      IMPORTING
*        pe_header          =
      TABLES
        pt_segments        = r_segment_t
*       PT_FIELDS          =
*       PT_FVALUES         =
*       PT_MESSAGES        =
      EXCEPTIONS
        object_unknown     = 1
        segment_unknown    = 2
        relation_not_found = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.




  ENDMETHOD.


  METHOD get_xmltable_from_idocxml.
    DATA: bapiret2_t TYPE bapiret2_t.

    IF me->xmltable_t IS NOT INITIAL.
      r_xmltable =  me->xmltable_t.
      RETURN.
    ELSE.
      CLEAR r_xmltable.
      me->xmltable_t = xml_processing_parsing( i_xml = i_idoc_xml ).
      IF lines( xmltable_t ) = 0.
        CLEAR me->xmltable_t.
*     Raise Exception no XML file
      ELSE.
        r_xmltable = me->xmltable_t.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD if_http_extension~handle_request.
    DATA(request) = server->request.
    DATA(payload) = request->get_data( ).
  ENDMETHOD.


  METHOD xml_processing_get_element.
    CONSTANTS strmaxlen TYPE i VALUE 255.
    DATA: text_node       TYPE REF TO if_ixml_text.
    DATA: index       TYPE i.
    DATA(level) = i_node->get_height( ).
    DATA xmltable_r TYPE ts_xmltable.

*   DATA(node_type)  = i_node->get_type( ).
*    DATA(node_name)  = i_node->get_name( ).
*    DATA(node_value) = i_node->get_value( ).
*DATA(node_depth) = i_node->GET_DEPTH( ).

    CASE i_node->get_type( ).
      WHEN if_ixml_node=>co_node_element.
        DATA(string) = i_node->get_name( ).
        CLEAR xmltable_r.
        xmltable_r-hier = level.
        xmltable_r-cname = string.
        IF i_node->get_value( ) EQ space AND i_node->get_depth( ) EQ 0.
          xmltable_r-cvalue = space.
          xmltable_r-type = 'V'.
        ENDIF.
        APPEND xmltable_r TO  c_xmltable.
        DATA(attributes) = i_node->get_attributes( ).
        IF NOT attributes IS INITIAL.
          DATA(attr_len) = attributes->get_length( ).
          WHILE index < attr_len.
            DATA(child) = attributes->get_item( index ).
            CLEAR xmltable_r.
            xmltable_r-hier = level.
            xmltable_r-type = 'A'.
            xmltable_r-cname = child->get_name( ).
            xmltable_r-cvalue = child->get_value( ).
            APPEND xmltable_r TO c_xmltable.
            index = index + 1.
          ENDWHILE.
        ENDIF.
      WHEN if_ixml_node=>co_node_text OR if_ixml_node=>co_node_cdata_section.

        IF i_node->get_type( ) EQ if_ixml_node=>co_node_text.
          text_node ?= i_node->query_interface( ixml_iid_text ).
        ELSE.
          text_node ?= i_node->query_interface( ixml_iid_cdata_section ).
        ENDIF.

        IF text_node->ws_only( ) IS INITIAL.
          string = i_node->get_value( ).
*
          DESCRIBE TABLE c_xmltable LINES index.
          READ TABLE c_xmltable INTO xmltable_r INDEX index.
          WHILE NOT xmltable_r-type IS INITIAL.
            index = index - 1.
            READ TABLE c_xmltable INTO xmltable_r INDEX index.
          ENDWHILE.
          DATA(len) = strlen( string ).
          IF len < strmaxlen.
            xmltable_r-cvalue = string.
            xmltable_r-type = 'V'.
          ELSE.
            xmltable_r-cvalue = string(strmaxlen).
            xmltable_r-type = '+'.
          ENDIF.
          MODIFY c_xmltable FROM xmltable_r INDEX index.
          len = strlen( string ) - strmaxlen.
          WHILE len > 0.
            string = string+strmaxlen.
            IF len < strmaxlen.
              xmltable_r-cvalue = string.
              xmltable_r-type = 'V'.
            ELSE.
              xmltable_r-cvalue = string(strmaxlen).
              xmltable_r-type = '+'.
            ENDIF.
            index = index + 1.
            INSERT xmltable_r INTO c_xmltable INDEX index.
            len = len - strmaxlen.
          ENDWHILE.
        ENDIF.
    ENDCASE.

    i_node = i_node->get_first_child( ).
    WHILE NOT i_node IS INITIAL.
      xml_processing_get_element( EXPORTING i_node = i_node CHANGING c_xmltable = c_xmltable ).
      i_node = i_node->get_next( ).
    ENDWHILE.


  ENDMETHOD.


  METHOD xml_processing_parsing.
    DATA(ixml) = cl_ixml=>create( ).
    DATA(stream_factory) = ixml->create_stream_factory( ).
    DATA(istream) = stream_factory->create_istream_xstring( i_xml ).
    DATA(document) = ixml->create_document( ).
    DATA(parser) = ixml->create_parser( stream_factory = stream_factory
                                    istream        = istream
                                    document       = document ).
    IF parser->parse( ) NE 0.
      IF parser->num_errors( ) NE 0.
*      Raise error
      ENDIF.
      EXIT.
    ENDIF.

    CALL METHOD istream->close( ).
    CLEAR istream.

    xml_processing_get_element( EXPORTING i_node = document CHANGING c_xmltable = r_xmltable ).

  ENDMETHOD.
ENDCLASS.
