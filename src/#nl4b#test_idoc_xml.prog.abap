*&---------------------------------------------------------------------*
*& Report /nl4b/test_idoc_xml
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT /nl4b/test_idoc_xml.

  data hdl type ref to /NL4B/CL_IDOCXML2JSON.
  hdl = new #( ).

  data(idocxml) = hdl->convert_idoc_to_idoc_xml( EXPORTING i_docnum = '000000000001' ).
data(idocjson) = hdl->convert_idoc_xml_to_json( EXPORTING i_idoc_xml = idocxml i_include_control_record = abap_true ).
