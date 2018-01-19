*&---------------------------------------------------------------------*
*& Report  Y_INJECTION_TEST
*&
*&---------------------------------------------------------------------*
* Show two ways to create linked objects, one using dependency injection
*--------------------------------------------------------------------*
REPORT  y_injection_test.

PARAMETERS : p_valid TYPE sy-datum,
             p_werks TYPE werks_d,
             p_test  AS CHECKBOX.

INITIALIZATION.
  p_valid = sy-datum.
  p_werks = '3116'.

START-OF-SELECTION.
*  PERFORM do_it_the_long_way.
  PERFORM do_it_the_short_way.

*&---------------------------------------------------------------------*
*&      Form  DO_IT_THE_LONG_WAY
*&---------------------------------------------------------------------*
* Normal way of doing things
*----------------------------------------------------------------------*
FORM do_it_the_long_way .
  DATA: lo_logger        TYPE REF TO ycl_test_logger.
  DATA: lo_db_layer      TYPE REF TO ycl_test_db_layer.
  DATA: lo_mock_db_layer TYPE REF TO ycl_test_mock_db_layer.
  DATA: lo_simulator     TYPE REF TO ycl_test_simulator.

  CREATE OBJECT lo_logger.

  IF p_test = abap_true.

    CREATE OBJECT lo_mock_db_layer
      EXPORTING
        io_logger   = lo_logger
        id_valid_on = p_valid.

    CREATE OBJECT lo_simulator
      EXPORTING
        id_plant_id   = p_werks
        io_db_layer   = lo_mock_db_layer
        io_logger     = lo_logger.

  ELSE.

    CREATE OBJECT lo_db_layer
      EXPORTING
        io_logger   = lo_logger
        id_valid_on = p_valid.

    CREATE OBJECT lo_simulator
      EXPORTING
        id_plant_id   = p_werks
        io_db_layer   = lo_db_layer
        io_logger     = lo_logger.

  ENDIF.

  lo_simulator->say_who_you_are( ).

  SKIP.

ENDFORM.                    " DO_IT_THE_LONG_WAY
*&---------------------------------------------------------------------*
*&      Form  DO_IT_THE_SHORT_WAY
*&---------------------------------------------------------------------*
*  Using Constructor Injection
*----------------------------------------------------------------------*
FORM do_it_the_short_way .
* Local Variables
  DATA: lo_simulator  TYPE REF TO ycl_test_simulator.

  zcl_bc_injector=>during_construction( :
    for_parameter = 'ID_PLANT_ID' use_value = p_werks ),
    for_parameter = 'ID_VALID_ON' use_value = p_valid ).

  IF p_test = abap_true.
    "We want to use a test double for the database object
    zcl_bc_injector=>instead_of( using_main_class = 'YCL_TEST_DB_LAYER'
                                 use_sub_class    = 'YCL_TEST_MOCK_DB_LAYER' ).
  ENDIF.

  zcl_bc_injector=>create_via_injection( CHANGING co_object = lo_simulator ).

  lo_simulator->say_who_you_are( ).

ENDFORM.                    " DO_IT_THE_SHORT_WAY
