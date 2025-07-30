INTERFACE /apmg/if_pacote PUBLIC.


************************************************************************
* Pacote
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************

  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

  TYPES:
    BEGIN OF ty_pacote,
      key       TYPE /apmg/if_persist_apm=>ty_key,
      name      TYPE string,
      json      TYPE string,
      packument TYPE /apmg/if_types=>ty_packument,
      instance  TYPE REF TO /apmg/if_pacote,
    END OF ty_pacote,
    ty_pacotes TYPE STANDARD TABLE OF ty_pacote WITH KEY key ##NEEDED.

  METHODS get
    RETURNING
      VALUE(result) TYPE /apmg/if_types=>ty_packument.

  METHODS get_json
    RETURNING
      VALUE(result) TYPE string.

  METHODS get_version
    IMPORTING
      !version      TYPE string
    RETURNING
      VALUE(result) TYPE /apmg/if_types=>ty_version.

  METHODS set
    IMPORTING
      !packument    TYPE /apmg/if_types=>ty_packument
    RETURNING
      VALUE(result) TYPE REF TO /apmg/if_pacote
    RAISING
      /apmg/cx_error.

  METHODS set_json
    IMPORTING
      !json         TYPE string
    RETURNING
      VALUE(result) TYPE REF TO /apmg/if_pacote
    RAISING
      /apmg/cx_error.

  METHODS exists
    RETURNING
      VALUE(result) TYPE abap_bool.

  METHODS load
    RETURNING
      VALUE(result) TYPE REF TO /apmg/if_pacote
    RAISING
      /apmg/cx_error.

  METHODS save
    RAISING
      /apmg/cx_error.

  METHODS delete
    RAISING
      /apmg/cx_error.

  METHODS manifest
    IMPORTING
      version       TYPE string
      abbreviated   TYPE abap_bool DEFAULT abap_false
      write         TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result) TYPE string
    RAISING
      /apmg/cx_error.

  METHODS packument
    IMPORTING
      write         TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result) TYPE string
    RAISING
      /apmg/cx_error.

  METHODS tarball
    IMPORTING
      filename      TYPE string
    RETURNING
      VALUE(result) TYPE xstring
    RAISING
      /apmg/cx_error.

ENDINTERFACE.
