************************************************************************
* Pacote
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
REPORT z_pacote_tester.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS:
    p_name TYPE string LOWER CASE OBLIGATORY DEFAULT 'mbtools-demo',
    p_vers TYPE string LOWER CASE,
    p_pack RADIOBUTTON GROUP g1 DEFAULT 'X',
    p_mani RADIOBUTTON GROUP g1,
    p_abbr RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
  PARAMETERS:
    p_reg TYPE string LOWER CASE OBLIGATORY DEFAULT 'https://registry.abappm.com'.
SELECTION-SCREEN END OF BLOCK b2.

DATA:
  gx_error  TYPE REF TO cx_root,
  gv_result TYPE string.

START-OF-SELECTION.

  TRY.
      CASE abap_true.
        WHEN p_pack.
          gv_result = zcl_pacote=>factory(
            iv_registry = p_reg
            iv_name     = p_name )->packument( ).
        WHEN p_mani.
          gv_result = zcl_pacote=>factory(
            iv_registry = p_reg
            iv_name     = p_name )->manifest( p_vers ).
        WHEN p_abbr.
          gv_result = zcl_pacote=>factory(
            iv_registry = p_reg
            iv_name     = p_name )->manifest(
              iv_version     = p_vers
              iv_abbreviated = abap_true ).
      ENDCASE.
    CATCH cx_root INTO gx_error.
      cl_abap_browser=>show_html( html_string = gx_error->get_text( ) ).
  ENDTRY.

  IF gv_result IS NOT INITIAL.
    cl_abap_browser=>show_html( html_string = gv_result ).
  ENDIF.
