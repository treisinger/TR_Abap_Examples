*&---------------------------------------------------------------------*
*&  Report           Z_UNIT_TEST_02
*&---------------------------------------------------------------------*

report z_unit_test_02.

class lcl_instrument_calculator definition.
  public section.

    types:
      y_element type f.

    methods:
      first_moment_about
        importing
          i_point         type y_element
        returning
          value(r_result) type y_element,

      add_element
        importing
          i_element type numeric.

  private section.
    data:
      elements type standard table of y_element.

endclass.

class lcl_instrument_calculator implementation.
  method first_moment_about.

    data:
      element type y_element.

    loop at me->elements into element.
      element  = element + element.
      element  = element - i_point.
      r_result = r_result + element.
    endloop.

    r_result = r_result / sy-tabix.

  endmethod.
  method add_element.

    append i_element to me->elements.

  endmethod.

endclass.

* ltc_    lokale Testklasse
* ltd_    lokale Testattrape (Testdouble)
* lth_    lokale Testhilfsklasse

class ltc_instrument_calculator definition for testing
  risk level harmless
  duration   short.
*  before 7.02
*  "#AU Risk_Level Harmless
*  "#AU Duration   Short

  private section.
    methods:
      test_first_moment for testing.

endclass.

class ltc_instrument_calculator implementation.

  method test_first_moment.

    data:
      expected_result      type f,
      point                type f,
      instrument_calulator type ref to lcl_instrument_calculator.

    create object instrument_calulator.

    instrument_calulator->add_element( 1 ).
    instrument_calulator->add_element( 2 ).

    point           = 2.
    expected_result = 1.

    cl_abap_unit_assert=>assert_equals_float(
        act              = instrument_calulator->first_moment_about( i_point = point )
        exp              = expected_result
*        rtol             = CL_ABAP_UNIT_ASSERT=>RTOL_DEFAULT
        msg              = 'Failure'
*        level            = IF_AUNIT_CONSTANTS=>CRITICAL
*        quit             = IF_AUNIT_CONSTANTS=>METHOD
           ).

  endmethod.
endclass.
