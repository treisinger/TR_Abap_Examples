class YCL_TEST_DB_LAYER definition
  public
  create public .

public section.

  methods SAY_WHO_YOU_ARE .
  methods CONSTRUCTOR
    importing
      !IO_LOGGER type ref to YCL_TEST_LOGGER optional
      !ID_VALID_ON type SY-DATUM .
protected section.
private section.

  data MO_LOGGER type ref to YCL_TEST_LOGGER .
  data MD_VALID_ON type SY-DATUM .
ENDCLASS.



CLASS YCL_TEST_DB_LAYER IMPLEMENTATION.


METHOD constructor.

  IF io_logger IS SUPPLIED.
    mo_logger = io_logger.
  ELSE.
    CREATE OBJECT mo_logger.
  ENDIF.

  md_valid_on = id_valid_on.

ENDMETHOD.


METHOD say_who_you_are.

  WRITE:/ 'I am the base test DB access class'.

  WRITE:/ 'I am valid on ', md_valid_on+6(2), md_valid_on+4(2), md_valid_on(4).

  WRITE:/ 'The DB access classes logger is going to say hello'.

  mo_logger->say_who_you_are( ).

ENDMETHOD.
ENDCLASS.
