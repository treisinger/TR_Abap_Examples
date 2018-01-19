*&---------------------------------------------------------------------*
*&  Report           Z_UNIT_TEST_01
*&---------------------------------------------------------------------*
report Z_UNIT_TEST_01.

CLASS lcl_sum DEFINITION.
  PUBLIC SECTION.
    METHODS: SUM IMPORTING iv_1 TYPE i
                 RETURNING VALUE(rv_sum) TYPE i.
ENDCLASS.                    "lcl_sum DEFINITION
*
START-OF-SELECTION.
* Nothing here yet
*
*
CLASS lcl_sum IMPLEMENTATION.
  METHOD SUM.
    rv_sum = iv_1 + iv_1.
  ENDMETHOD.                    "sum
ENDCLASS.                    "lcl_sum IMPLEMENTATION

CLASS lcl_test DEFINITION FOR TESTING
  "#AU Risk_Level Harmless
  "#AU Duration   Short
.
  PUBLIC SECTION.
    METHODS: m_sum FOR TESTING.
ENDCLASS.                    "lcl_test DEFINITION
*
CLASS lcl_test IMPLEMENTATION.
  METHOD m_sum.
    DATA: o_cut TYPE REF TO lcl_sum.
    DATA: lv_result TYPE i.
*
    CREATE OBJECT o_cut.
    lv_result = o_cut->sum( 3 ).
*
    cl_aunit_assert=>assert_equals(
        EXP                  = 6
        act                  = lv_result
        msg                  = 'something wrong'
           ).
  ENDMETHOD.                    "m_sum
ENDCLASS.                    "lcl_test IMPLEMENTATION
