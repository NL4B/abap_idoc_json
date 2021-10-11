class ltcl_idocxml2json definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      first_test for testing raising cx_static_check.
endclass.


class ltcl_idocxml2json implementation.

  method first_test.

  data hdl type ref to /NL4B/CL_IDOCXML2JSON.
  hdl = new #( ).

  data(idocxml) = hdl->convert_idoc_to_idoc_xml( EXPORTING i_docnum = '000000000001' ).
data(idocjson) = hdl->convert_idoc_xml_to_json( EXPORTING i_idoc_xml = idocxml i_include_control_record = abap_true ).

    cl_abap_unit_assert=>fail( 'Implement your first test here' ).
  endmethod.

endclass.
