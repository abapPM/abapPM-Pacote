CLASS ltcl_pacote DEFINITION FOR TESTING RISK LEVEL HARMLESS
  DURATION SHORT FINAL.

  PRIVATE SECTION.

    DATA:
      cut            TYPE REF TO /apmg/if_package_json ##NEEDED,
      test_packument TYPE /apmg/if_types=>ty_packument,
      test_json      TYPE string.

    METHODS setup.

    METHODS prepare_string
      IMPORTING
        input         TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS:
      convert_json_to_packument FOR TESTING RAISING /apmg/cx_error,
      convert_packument_to_json FOR TESTING RAISING /apmg/cx_error.

ENDCLASS.

CLASS /apmg/cl_pacote DEFINITION LOCAL FRIENDS ltcl_pacote.

CLASS ltcl_pacote IMPLEMENTATION.

  METHOD setup.

    test_json = `{`
      && `|  "name": "@registrytest/package",`
      && `|  "description": "This is an example package",`
      && `|  "dist-tags": {`
      && `|    "latest": "1.0.0"`
      && `|  },`
      && `|  "time": {`
      && `|    "1.0.0": "2024-05-02T12:17:47.805Z",`
      && `|    "created": "2024-05-02T17:59:06.251Z",`
      && `|    "modified": "2024-11-10T19:51:04.774Z"`
      && `|  },`
      && `|  "versions": {`
      && `|    "1.0.0": {`
      && `|      "name": "@registrytest/package",`
      && `|      "version": "1.0.0",`
      && `|      "description": "This is an example package",`
      && `|      "type": "module",`
      && `|      "keywords": [`
      && `|        "example",`
      && `|        "package",`
      && `|        "test"`
      && `|      ],`
      && `|      "homepage": "https://registrytest.org",`
      && `|      "icon": "registrytest.svg",`
      && `|      "bugs": {`
      && `|        "url": "https://github.com/registrytest/package/issues",`
      && `|        "email": "bugs@registrytest.org"`
      && `|      },`
      && `|      "license": "MIT",`
      && `|      "author": {`
      && `|        "name": "Olivia Ortega",`
      && `|        "url": "https://github.com/rt-owner",`
      && `|        "email": "owner@registrytest.org",`
      && `|        "avatar": "data:image/svg+xml;"`
      && `|      },`
      && `|      "contributors": [`
      && `|        {`
      && `|          "name": "Cindy Collins",`
      && `|          "url": "https://github.com/rt-contributors",`
      && `|          "email": "contributor@registrytest.org"`
      && `|        },`
      && `|        {`
      && `|          "name": "Olivia Ortega",`
      && `|          "url": "https://github.com/rt-owner",`
      && `|          "email": "owner@registrytest.org"`
      && `|        }`
      && `|      ],`
      && `|      "maintainers": [`
      && `|        {`
      && `|          "name": "mbtools",`
      && `|          "email": "marc@marcbernardtools.com"`
      && `|        },`
      && `|        {`
      && `|          "name": "rt-contributor",`
      && `|          "email": "contributor@registrytest.org"`
      && `|        },`
      && `|        {`
      && `|          "name": "rt-maintainer",`
      && `|          "email": "maintainer@registrytest.org"`
      && `|        },`
      && `|        {`
      && `|          "name": "rt-owner",`
      && `|          "email": "owner@registrytest.org"`
      && `|        }`
      && `|      ],`
      && `|      "main": "index.js",`
      && `|      "man": [`
      && `|        "manual.md"`
      && `|      ],`
      && `|      "repository": {`
      && `|        "type": "git",`
      && `|        "url": "git+https://github.com/registrytest/package.git"`
      && `|      },`
      && `|      "funding": {`
      && `|        "type": "github",`
      && `|        "url": "https://github.com/registrytest"`
      && `|      },`
      && `|      "dependencies": {`
      && `|        "express": "^4.19.2",`
      && `|        "lodash": "^4.17.21",`
      && `|        "semver": "~7.6.0"`
      && `|      },`
      && `|      "devDependencies": {`
      && `|        "eslint": "^9.1.1",`
      && `|        "jest": "^29.7.0",`
      && `|        "prettier": "^3.2.5"`
      && `|      },`
      && `|      "optionalDependencies": {`
      && `|        "tap": "18.7.2"`
      && `|      },`
      && `|      "peerDependencies": {`
      && `|        "lodash": "^4.17.21",`
      && `|        "react": ">=17 <19.0.0"`
      && `|      },`
      && `|      "bundleDependencies": [`
      && `|        "semver"`
      && `|      ],`
      && `|      "engines": {`
      && `|        "abap": ">=7.50",`
      && `|        "apm": ">=1"`
      && `|      },`
      && `|      "os": [`
      && `|        "linux"`
      && `|      ],`
      && `|      "cpu": [`
      && `|        "x86-64"`
      && `|      ],`
      && `|      "db": [`
      && `|        "hdb"`
      && `|      ],`
      && `|      "private": true,`
      && `|      "readme": "# A Package for Registry Test",`
      && `|      "dist": {`
      && `|        "fileCount": 6,`
      && `|        "shasum": "c689ad02174b9a45ee0f2350f74a0b503a109389",`
      && `|        "tarball": "https://registry.npmjs.org/@registrytest/package/-/package-1.0.0.tgz",`
      && `|        "unpackedSize": 3987,`
      && `|        "integrity": "sha512-Z5UFg8g3X4eT/1wl1HCVENNAvNh9z5By7z3OL7XZQnsBh1SQ1dtUu1InlT3b2D`
      && `kpOOxwagDAVTX16hS25TU/rw==",`
      && `|        "signatures": [`
      && `|          {`
      && `|            "keyid": "SHA256:jl3bwswu80PjjokCgh0o2w5c2U4LhQAE57gj9cz1kzA",`
      && `|            "sig": "MEUCIQDGjADf3mDXMzryyGvffT0/s1IqISZC00BF8WctcYzq3gIgHCzxFPNAuwe616Fcvr4`
      && `tOM2FRrP6MKfhtIpuRBI2pjg="`
      && `|          }`
      && `|        ]`
      && `|      },`
      && `|      "deprecated": true,`
      && `|      "_id": "@registrytest/package@1.0.0",`
      && `|      "_abapVersion": "7.54.0",`
      && `|      "_apmVersion": "1.0.0"`
      && `|    }`
      && `|  },`
      && `|  "maintainers": [`
      && `|    {`
      && `|      "name": "rt-maintainer",`
      && `|      "email": "maintainer@registrytest.org"`
      && `|    },`
      && `|    {`
      && `|      "name": "rt-owner",`
      && `|      "email": "owner@registrytest.org"`
      && `|    }`
      && `|  ],`
      && `|  "readme": "# A Package for Registry Test",`
      && `|  "users": {`
      && `|    "be": 4,`
      && `|    "edge": 5`
      && `|  },`
      && `|  "homepage": "https://registrytest.org",`
      && `|  "icon": "registrytest.svg",`
      && `|  "bugs": {`
      && `|    "url": "https://github.com/registrytest/package/issues",`
      && `|    "email": "bugs@registrytest.org"`
      && `|  },`
      && `|  "license": "MIT",`
      && `|  "keywords": [`
      && `|    "example",`
      && `|    "package",`
      && `|    "test"`
      && `|  ],`
      && `|  "author": {`
      && `|    "name": "Olivia Ortega",`
      && `|    "url": "https://github.com/rt-owner",`
      && `|    "email": "owner@registrytest.org",`
      && `|    "avatar": "data:image/svg+xml;"`
      && `|  },`
      && `|  "repository": {`
      && `|    "type": "git",`
      && `|    "url": "git+https://github.com/registrytest/package.git",`
      && `|    "directory": "/src"`
      && `|  },`
      && `|  "_id": "@registrytest/package",`
      && `|  "_rev": "5-c21daa8aefda6b83f0f0cfd7302e0f81",`
      && `|  "_attachments": {`
      && `|    "package-1.0.0.tgz": {`
      && `|      "content_type": "tgz",`
      && `|      "data": "a1b2c3",`
      && `|      "length": 123`
      && `|    }`
      && `|  },`
      && `|  "access": "test"`
      && `|}`.

    test_json = prepare_string( test_json ).

    test_packument                   = VALUE #(
      name                           = `@registrytest/package`
      description                    = `This is an example package`
      dist_tags                      = VALUE #(
        (
          key                        = `latest`
          value                      = `1.0.0`
        )
      )
      time                           = VALUE #(
        (
          key                        = `1.0.0`
          timestamp                  = '20240502121747.805'
        )
        (
          key                        = `created`
          timestamp                  = '20240502175906.251'
        )
        (
          key                        = `modified`
          timestamp                  = '20241110195104.774'
        )
      )
      versions                       = VALUE #(
        (
          key                        = `1.0.0`
          manifest                    = VALUE #(
            name                     = `@registrytest/package`
            version                  = `1.0.0`
            description              = `This is an example package`
            type                     = `module`
            keywords                 = VALUE #(
              (
                                       `example`
              )
              (
                                       `package`
              )
              (
                                       `test`
              )
            )
            homepage                 = `https://registrytest.org`
            icon                     = `registrytest.svg`
            bugs                     = VALUE #(
              url                    = `https://github.com/registrytest/package/issues`
              email                  = `bugs@registrytest.org`
            )
            license                  = `MIT`
            author                   = VALUE #(
              name                   = `Olivia Ortega`
              url                    = `https://github.com/rt-owner`
              email                  = `owner@registrytest.org`
              avatar                 = `data:image/svg+xml;`
            )
            contributors             = VALUE #(
              (
                name                 = `Cindy Collins`
                url                  = `https://github.com/rt-contributors`
                email                = `contributor@registrytest.org`
                avatar               = ``
              )
              (
                name                 = `Olivia Ortega`
                url                  = `https://github.com/rt-owner`
                email                = `owner@registrytest.org`
                avatar               = ``
              )
            )
            maintainers              = VALUE #(
              (
                name                 = `mbtools`
                url                  = ``
                email                = `marc@marcbernardtools.com`
                avatar               = ``
              )
              (
                name                 = `rt-contributor`
                url                  = ``
                email                = `contributor@registrytest.org`
                avatar               = ``
              )
              (
                name                 = `rt-maintainer`
                url                  = ``
                email                = `maintainer@registrytest.org`
                avatar               = ``
              )
              (
                name                 = `rt-owner`
                url                  = ``
                email                = `owner@registrytest.org`
                avatar               = ``
              )
            )
            main                     = `index.js`
            man                      = VALUE #(
              (
                                       `manual.md`
              )
            )
            repository               = VALUE #(
              type                   = `git`
              url                    = `git+https://github.com/registrytest/package.git`
              directory              = ``
            )
            funding                  = VALUE #(
              type                   = `github`
              url                    = `https://github.com/registrytest`
            )
            dependencies             = VALUE #(
              (
                key                  = `express`
                range                = `^4.19.2`
              )
              (
                key                  = `lodash`
                range                = `^4.17.21`
              )
              (
                key                  = `semver`
                range                = `~7.6.0`
              )
            )
            dev_dependencies         = VALUE #(
              (
                key                  = `eslint`
                range                = `^9.1.1`
              )
              (
                key                  = `jest`
                range                = `^29.7.0`
              )
              (
                key                  = `prettier`
                range                = `^3.2.5`
              )
            )
            optional_dependencies    = VALUE #(
              (
                key                  = `tap`
                range                = `18.7.2`
              )
            )
            peer_dependencies        = VALUE #(
              (
                key                  = `lodash`
                range                = `^4.17.21`
              )
              (
                key                  = `react`
                range                = `>=17 <19.0.0`
              )
            )
            bundle_dependencies      = VALUE #(
              (
                                       `semver`
              )
            )
            engines                  = VALUE #(
              (
                key                  = `abap`
                range                = `>=7.50`
              )
              (
                key                  = `apm`
                range                = `>=1`
              )
            )
            os                       = VALUE #(
              (
                                       `linux`
              )
            )
            cpu                      = VALUE #(
              (
                                       `x86-64`
              )
            )
            db                       = VALUE #(
              (
                                       `hdb`
              )
            )
            private                  = abap_true
            readme                   = `# A Package for Registry Test`
            dist                     = VALUE #(
              file_count             = 6
              shasum                 = `c689ad02174b9a45ee0f2350f74a0b503a109389`
              tarball                = `https://registry.npmjs.org/@registrytest/package/-/package-1.0.0.tgz`
              unpacked_size          = 3987
              integrity              =
              `sha512-Z5UFg8g3X4eT/1wl1HCVENNAvNh9z5By7z3OL7XZQnsBh1SQ1dtUu1InlT3b2DkpOOxwagDAVTX16hS25TU/rw==`
              signatures             = VALUE #(
                (
                  keyid              = `SHA256:jl3bwswu80PjjokCgh0o2w5c2U4LhQAE57gj9cz1kzA`
                  sig                =
                  `MEUCIQDGjADf3mDXMzryyGvffT0/s1IqISZC00BF8WctcYzq3gIgHCzxFPNAuwe616Fcvr4tOM2FRrP6MKfhtIpuRBI2pjg=`
                )
              )
            )
            deprecated               = abap_true
            _id                      = `@registrytest/package@1.0.0`
            _abap_version            = `7.54.0`
            _apm_version             = `1.0.0`
          )
        )
      )
      maintainers                    = VALUE #(
        (
          name                       = `rt-maintainer`
          url                        = ``
          email                      = `maintainer@registrytest.org`
          avatar                     = ``
        )
        (
          name                       = `rt-owner`
          url                        = ``
          email                      = `owner@registrytest.org`
          avatar                     = ``
        )
      )
      readme                         = `# A Package for Registry Test`
      users                          = VALUE #(
        (
          name                       = `be`
          stars                      = 4
        )
        (
          name                       = `edge`
          stars                      = 5
        )
      )
      homepage                       = `https://registrytest.org`
      icon                           = `registrytest.svg`
      bugs                           = VALUE #(
        url                          = `https://github.com/registrytest/package/issues`
        email                        = `bugs@registrytest.org`
      )
      license                        = `MIT`
      keywords                       = VALUE #(
        (
                                       `example`
        )
        (
                                       `package`
        )
        (
                                       `test`
        )
      )
      author                         = VALUE #(
        name                         = `Olivia Ortega`
        url                          = `https://github.com/rt-owner`
        email                        = `owner@registrytest.org`
        avatar                       = `data:image/svg+xml;`
      )
      repository                     = VALUE #(
        type                         = `git`
        url                          = `git+https://github.com/registrytest/package.git`
        directory                    = `/src`
      )
      _id                            = `@registrytest/package`
      _rev                           = `5-c21daa8aefda6b83f0f0cfd7302e0f81`
      _attachments                   = VALUE #(
        (
          key                        = `package-1.0.0.tgz`
          tarball                    = VALUE #(
            content_type             = `tgz`
            data                     = `a1b2c3`
            length                   = 123
          )
        )
      )
      access                         = `test` ).

  ENDMETHOD.

  METHOD prepare_string.
    result = replace(
      val  = input
      sub  = '|'
      with = |\n|
      occ  = 0 ).
  ENDMETHOD.

  METHOD convert_json_to_packument.

    DATA(packument) = /apmg/cl_pacote=>convert_json_to_packument( test_json ).

    cl_abap_unit_assert=>assert_equals(
      act = packument
      exp = test_packument ).

  ENDMETHOD.

  METHOD convert_packument_to_json.

    DATA(json) = /apmg/cl_pacote=>convert_packument_to_json( test_packument ).

    cl_abap_unit_assert=>assert_equals(
      act = json
      exp = test_json ).

  ENDMETHOD.

ENDCLASS.
