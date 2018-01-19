*&---------------------------------------------------------------------*
*& Report  Z_DP_FACTORY_ABSTRACT_02
*&
*&---------------------------------------------------------------------*
*&
*&  Exmple from zevolving site
*&---------------------------------------------------------------------*

REPORT znp_dp_abs_factory.

*=== .....
CLASS abs_data DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS: read_data ABSTRACT.
ENDCLASS.                    "abs_data DEFINITION
*===
CLASS data_from_file DEFINITION INHERITING FROM abs_data.
  PUBLIC SECTION.
    METHODS: read_data REDEFINITION.
ENDCLASS.                    "data_from_file DEFINITION
*
CLASS data_from_file IMPLEMENTATION.
  METHOD read_data.
    WRITE: / 'Reading data from File'.
  ENDMETHOD.                    "read_data
ENDCLASS.                    "Data_from_file IMPLEMENTATION
*===
CLASS data_from_db DEFINITION INHERITING FROM abs_data.
  PUBLIC SECTION.
    METHODS: read_data REDEFINITION.
ENDCLASS.                    "data_from_db DEFINITION
*
CLASS data_from_db IMPLEMENTATION.
  METHOD read_data.
    WRITE: / 'Reading data from DATABASE TABLE'.
  ENDMETHOD.                    "read_data
ENDCLASS.                    "Data_from_db IMPLEMENTATION

*===....
CLASS abs_print DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS: write_data ABSTRACT.
ENDCLASS.                    "abs_print DEFINITION
*===
CLASS print_alv DEFINITION INHERITING FROM abs_print.
  PUBLIC SECTION.
    METHODS: write_data REDEFINITION.
ENDCLASS.                    "print_alv DEFINITION
*
CLASS print_alv IMPLEMENTATION.
  METHOD write_data.
    WRITE: / 'Writing data into ALV'.
  ENDMETHOD.                    "write_data
ENDCLASS.                    "print_alv IMPLEMENTATION
*===
CLASS print_simple DEFINITION INHERITING FROM abs_print.
  PUBLIC SECTION.
    METHODS: write_data REDEFINITION.
ENDCLASS.                    "print_simple DEFINITION
*
CLASS print_simple IMPLEMENTATION.
  METHOD write_data.
    WRITE: / 'Writing data in classic - This is actually classic'.
  ENDMETHOD.                    "write_data
ENDCLASS.                    "print_simple IMPLEMENTATION

*=== ....
CLASS report DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS: get_data ABSTRACT,
             print_data ABSTRACT.
ENDCLASS.                    "absfactory DEFINITION
*===
CLASS simplereport DEFINITION INHERITING FROM report.
  PUBLIC SECTION.
    METHODS: get_data REDEFINITION.
    METHODS: print_data REDEFINITION.
ENDCLASS.                    "simplereport DEFINITION
*
CLASS simplereport IMPLEMENTATION.
  METHOD get_data.
    DATA: lo_data TYPE REF TO data_from_file.
    CREATE OBJECT lo_data.
    lo_data->read_data( ).
  ENDMETHOD.                    "get_Data
  METHOD print_data.
    DATA: lo_print TYPE REF TO print_simple.
    CREATE OBJECT lo_print.
    lo_print->write_data( ).
  ENDMETHOD.                    "print_data
ENDCLASS.                    "simplereport IMPLEMENTATION

*===
CLASS complexreport DEFINITION INHERITING FROM report.
  PUBLIC SECTION.
    METHODS: get_data REDEFINITION.
    METHODS: print_data REDEFINITION.
ENDCLASS.                    "complexreport DEFINITION
*
CLASS complexreport IMPLEMENTATION.
  METHOD get_data.
    DATA: lo_data TYPE REF TO data_from_db.
    CREATE OBJECT lo_data.
    lo_data->read_data( ).
  ENDMETHOD.                    "get_data
  METHOD print_data.
    DATA: lo_print TYPE REF TO print_alv.
    CREATE OBJECT lo_print.
    lo_print->write_data( ).
  ENDMETHOD.                    "print_data
ENDCLASS.                    "complexreport IMPLEMENTATION

*===
CLASS lcl_main_app DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: run.
ENDCLASS.                    "lcl_main_app DEFINITION
*
CLASS lcl_main_app IMPLEMENTATION.
  METHOD run.
    DATA: lo_report TYPE REF TO report.

*   Simple report (FILE + write)
    CREATE OBJECT lo_report TYPE simplereport.
    lo_report->get_data( ).
    lo_report->print_data( ).


*   report for DB in ALV
    CREATE OBJECT lo_report TYPE complexreport.
    lo_report->get_data( ).
    lo_report->print_data( ).

  ENDMETHOD.                    "run

ENDCLASS.                    "lcl_main_app IMPLEMENTATION

START-OF-SELECTION.
  lcl_main_app=>run( ).
