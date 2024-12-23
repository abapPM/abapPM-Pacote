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

DATA result TYPE string.

START-OF-SELECTION.

  TRY.
      DATA(pacote) = zcl_pacote=>factory(
        registry = p_reg
        name     = p_name ).

      CASE abap_true.
        WHEN p_pack.
          result = pacote->packument( ).

          DATA(packument) = pacote->get( ).
        WHEN p_mani.
          result = pacote->manifest( p_vers ).
        WHEN p_abbr.
          result = pacote->manifest(
            version     = p_vers
            abbreviated = abap_true ).
        WHEN p_tgz.
          DATA(tarball) = pacote->tarball( p_vers ).
      ENDCASE.
    CATCH cx_root INTO DATA(error).
      cl_abap_browser=>show_html( html_string = error->get_text( ) ).
  ENDTRY.

  IF result IS NOT INITIAL.
    cl_abap_browser=>show_html( html_string = result ).
  ELSEIF tarball IS NOT INITIAL.
    BREAK-POINT.
  ENDIF.
