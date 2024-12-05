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
      version TYPE zif_package_json_types=>ty_manifest,
    END OF ty_version.

  TYPES:
    BEGIN OF ty_attachment,
      key TYPE string,
      BEGIN OF tarball,
        content_type TYPE string,
        data         TYPE string,
        length       TYPE i,
      END OF tarball,
    END OF ty_attachment.

  TYPES:
    " Full packument (as fetched from registry)
    " Some fields are hoisted from latest version to root
    BEGIN OF ty_packument,
      name         TYPE string,
      description  TYPE string,
      dist_tags    TYPE STANDARD TABLE OF zif_package_json_types=>ty_generic WITH KEY key,
      time         TYPE STANDARD TABLE OF zif_package_json_types=>ty_time WITH KEY key,
      versions     TYPE STANDARD TABLE OF ty_version WITH KEY key,
      maintainers  TYPE STANDARD TABLE OF zif_package_json_types=>ty_person WITH KEY name,
      readme       TYPE string,
      users        TYPE STANDARD TABLE OF zif_package_json_types=>ty_user WITH KEY name,
      homepage     TYPE string,
      BEGIN OF bugs,
        url   TYPE zif_package_json_types=>ty_uri,
        email TYPE zif_package_json_types=>ty_email,
      END OF bugs,
      license      TYPE string,
      keywords     TYPE string_table,
      author       TYPE zif_package_json_types=>ty_person,
      BEGIN OF repository,
        type      TYPE string,
        url       TYPE zif_package_json_types=>ty_uri,
        directory TYPE string,
      END OF repository,
      _id          TYPE string,
      _rev         TYPE string,
      _attachments TYPE STANDARD TABLE OF ty_attachment WITH KEY key,
      access       TYPE string,
    END OF ty_packument.

  TYPES:
    BEGIN OF ty_pacote,
      key       TYPE zif_persist_apm=>ty_key,
      name      TYPE string,
      json      TYPE string,
      packument TYPE ty_packument,
      instance  TYPE REF TO zif_pacote,
    END OF ty_pacote.
  TYPES:
    ty_pacotes TYPE STANDARD TABLE OF ty_pacote WITH KEY key.

  METHODS get
    RETURNING
      VALUE(result) TYPE ty_packument.

  METHODS get_json
    RETURNING
      VALUE(result) TYPE string.

  METHODS set
    IMPORTING
      !is_packument TYPE ty_packument
    RETURNING
      VALUE(result) TYPE REF TO zif_pacote
    RAISING
      zcx_error.

  METHODS set_json
    IMPORTING
      !iv_json      TYPE string
    RETURNING
      VALUE(result) TYPE REF TO zif_pacote
    RAISING
      zcx_error.

  METHODS exists
    RETURNING
      VALUE(result) TYPE abap_bool.

  METHODS load
    RETURNING
      VALUE(result) TYPE REF TO zif_pacote
    RAISING
      zcx_error.

  METHODS save
    RAISING
      zcx_error.

  METHODS delete
    RAISING
      zcx_error.

  METHODS manifest
    IMPORTING
      iv_version     TYPE string
      iv_abbreviated TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result)  TYPE string
    RAISING
      zcx_error.

  METHODS packument
    RETURNING
      VALUE(result) TYPE string
    RAISING
      zcx_error.

  METHODS tarball
    IMPORTING
      iv_filename   TYPE string
    RETURNING
      VALUE(result) TYPE xstring
    RAISING
      zcx_error.

ENDINTERFACE.
