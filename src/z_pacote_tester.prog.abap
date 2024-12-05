************************************************************************
* Pacote
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
REPORT z_pacote_tester.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS:
    p_name TYPE string LOWER CASE OBLIGATORY DEFAULT 'emoji',
    p_vers TYPE string LOWER CASE,
    p_pack RADIOBUTTON GROUP g1 DEFAULT 'X',
    p_mani RADIOBUTTON GROUP g1,
    p_abbr RADIOBUTTON GROUP g1,
    p_tgz  RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
  PARAMETERS:
    p_reg TYPE string LOWER CASE OBLIGATORY DEFAULT 'https://playground.abappm.com'.
SELECTION-SCREEN END OF BLOCK b2.

DATA:
  gx_error     TYPE REF TO cx_root,
  gi_pacote    TYPE REF TO zif_pacote,
  gs_packument TYPE zif_pacote=>ty_packument,
  gv_result    TYPE string,
  gv_tarball   TYPE xstring.

START-OF-SELECTION.

  TRY.
      gi_pacote = zcl_pacote=>factory(
        iv_registry = p_reg
        iv_name     = p_name ).

      CASE abap_true.
        WHEN p_pack.
          gv_result = gi_pacote->packument( ).
          gs_packument = gi_pacote->get( ).
        WHEN p_mani.
          gv_result = gi_pacote->manifest( p_vers ).
        WHEN p_abbr.
          gv_result = gi_pacote->manifest(
                        iv_version     = p_vers
                        iv_abbreviated = abap_true ).
        WHEN p_tgz.
          gv_tarball = gi_pacote->tarball( p_vers ).
      ENDCASE.
    CATCH cx_root INTO gx_error.
      cl_abap_browser=>show_html( html_string = gx_error->get_text( ) ).
  ENDTRY.

  IF gv_result IS NOT INITIAL.
    cl_abap_browser=>show_html( html_string = gv_result ).
  ELSEIF gv_tarball IS NOT INITIAL.
    BREAK-POINT.
  ENDIF.
