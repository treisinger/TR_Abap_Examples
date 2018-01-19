class YCL_TEST_SIMULATOR definition
  public
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !IO_LOGGER type ref to YCL_TEST_LOGGER optional
      !IO_DB_LAYER type ref to YCL_TEST_DB_LAYER optional
      !ID_PLANT_ID type WERKS_D .
  methods SAY_WHO_YOU_ARE .
protected section.
private section.

  data MO_LOGGER type ref to YCL_TEST_LOGGER .
  data MO_DB_LAYER type ref to YCL_TEST_DB_LAYER .
  data MD_PLANT type WERKS_D .
ENDCLASS.



CLASS YCL_TEST_SIMULATOR IMPLEMENTATION.


METHOD constructor.

  IF io_logger IS SUPPLIED.
    mo_logger = io_logger.
  ELSE.
    CREATE OBJECT mo_logger.
  ENDIF.

  IF io_db_layer IS SUPPLIED.
    mo_db_layer = io_db_layer.
  ELSE.
    CREATE OBJECT mo_db_layer
      EXPORTING
        io_logger   = mo_logger
        id_valid_on = sy-datum.
  ENDIF.

  md_plant = id_plant_id.

ENDMETHOD.


METHOD say_who_you_are.

  WRITE:/ 'I am the test simulator base class'.

  WRITE: 'My plant is ',md_plant.

  WRITE:/ 'The simulators logger is going to say hello'.

  mo_logger->say_who_you_are( ).

  WRITE:/ 'The simulators DB access layer is going to say hello'.

  mo_db_layer->say_who_you_are( ).

ENDMETHOD.
ENDCLASS.
