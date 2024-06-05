INTERFACE zif_pacote PUBLIC.


************************************************************************
* Pacote
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

  TYPES:
    BEGIN OF ty_version,
      key     TYPE string,
      version TYPE zif_package_json_types=>ty_package_json,
    END OF ty_version.

  TYPES:
    " Full packument (as fetched from registry)
    " Some fields are hoisted from latest version to root
    " TODO: Check if this needs to happen in client or if its provided by registry already
    BEGIN OF ty_packument,
      _id         TYPE string,
      _rev        TYPE string,
      name        TYPE string,
      description TYPE string,
      dist_tags   TYPE STANDARD TABLE OF zif_package_json_types=>ty_generic WITH KEY key,
      time        TYPE STANDARD TABLE OF zif_package_json_types=>ty_generic WITH KEY key,
      versions    TYPE STANDARD TABLE OF ty_version WITH KEY key,
      maintainers TYPE STANDARD TABLE OF zif_package_json_types=>ty_person WITH KEY name,
      readme      TYPE string,
      users       TYPE STANDARD TABLE OF zif_package_json_types=>ty_user WITH KEY name,
      homepage    TYPE string,
      BEGIN OF bugs,
        url   TYPE zif_package_json_types=>ty_uri,
        email TYPE zif_package_json_types=>ty_email,
      END OF bugs,
      license     TYPE string,
      keywords    TYPE string_table,
      author      TYPE zif_package_json_types=>ty_person,
      BEGIN OF repository,
        type      TYPE string,
        url       TYPE zif_package_json_types=>ty_uri,
        directory TYPE string,
      END OF repository,
    END OF ty_packument.

  TYPES:
    BEGIN OF ty_pacote,
      key       TYPE zif_persist_apm=>ty_key,
      name      TYPE string,
      packument TYPE string,
      instance  TYPE REF TO zif_pacote,
    END OF ty_pacote.
  TYPES:
    ty_pacotes TYPE STANDARD TABLE OF ty_pacote WITH KEY key.

  METHODS get
    RETURNING
      VALUE(result) TYPE string.

  METHODS set
    IMPORTING
      !iv_packument TYPE string
    RETURNING
      VALUE(result) TYPE REF TO zif_pacote
    RAISING
      zcx_pacote.

  METHODS exists
    RETURNING
      VALUE(result) TYPE abap_bool.

  METHODS load
    RETURNING
      VALUE(result) TYPE REF TO zif_pacote
    RAISING
      zcx_pacote.

  METHODS save
    RAISING
      zcx_pacote.

  METHODS delete
    RAISING
      zcx_pacote.

  METHODS manifest
    IMPORTING
      iv_version     TYPE string
      iv_abbreviated TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result)  TYPE string
    RAISING
      zcx_pacote.

  METHODS packument
    RETURNING
      VALUE(result) TYPE string
    RAISING
      zcx_pacote.

  METHODS tarball
    IMPORTING
      iv_version    TYPE string
    RETURNING
      VALUE(result) TYPE string
    RAISING
      zcx_pacote.

ENDINTERFACE.
