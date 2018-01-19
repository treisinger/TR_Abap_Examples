REPORT z_dp_decorator_01.
*&---------------------------------------------------------------------*
*& Purpose: Decorator Design Pattern Demo
*& Author : Naimesh Patel
*&---------------------------------------------------------------------*

* ===
CLASS output DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      process_output ABSTRACT.
ENDCLASS.                    "output DEFINITION

* ====
CLASS alvoutput DEFINITION INHERITING FROM output.
  PUBLIC SECTION.
    METHODS:
      process_output REDEFINITION.
ENDCLASS.                    "alvoutput DEFINITION

*
CLASS alvoutput IMPLEMENTATION.
  METHOD process_output.
    WRITE: / 'Standard ALV output'.
  ENDMETHOD.                    "process_output
ENDCLASS.                    "alvoutput IMPLEMENTATION

* ====
CLASS opdecorator DEFINITION INHERITING FROM output.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING io_decorator TYPE REF TO output,
      process_output REDEFINITION.
  PRIVATE SECTION.
    DATA: o_decorator TYPE REF TO output.
ENDCLASS.                    "opdecorator DEFINITION

*
CLASS opdecorator IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->o_decorator = io_decorator.
  ENDMETHOD.                    "constructor
  METHOD process_output.
    CHECK o_decorator IS BOUND.
    o_decorator->process_output( ).
  ENDMETHOD.                    "process_output
ENDCLASS.                    "opdecorator IMPLEMENTATION

* =====
CLASS op_pdf DEFINITION INHERITING FROM opdecorator.
  PUBLIC SECTION.
    METHODS: process_output REDEFINITION.
ENDCLASS.                    "op_pdf DEFINITION

*
CLASS op_pdf IMPLEMENTATION.
  METHOD process_output.
    super->process_output( ).
    WRITE: /(10) space, 'Generating PDF'.
  ENDMETHOD.                    "process_output
ENDCLASS.                    "op_pdf IMPLEMENTATION

* ======
CLASS op_xls DEFINITION INHERITING FROM opdecorator.
  PUBLIC SECTION.
    METHODS: process_output REDEFINITION.
ENDCLASS.                    "op_xls DEFINITION

*
CLASS op_xls IMPLEMENTATION.
  METHOD process_output.
    super->process_output( ).
    WRITE: /(10) space, 'Generating Excel'.
  ENDMETHOD.                    "process_output
ENDCLASS.                    "op_xls IMPLEMENTATION

* =====
CLASS op_email DEFINITION INHERITING FROM opdecorator.
  PUBLIC SECTION.
    METHODS: process_output REDEFINITION.
ENDCLASS.                    "op_email DEFINITION

*
CLASS op_email  IMPLEMENTATION.
  METHOD process_output.
    super->process_output( ).
    WRITE: /(10) space, 'Sending Email'.
  ENDMETHOD.                    "process_output
ENDCLASS.                    "op_email IMPLEMENTATION

* ====
CLASS op_alv DEFINITION INHERITING FROM opdecorator.
  PUBLIC SECTION.
    METHODS: process_output REDEFINITION.
ENDCLASS.                    "op_alv DEFINITION

*
CLASS op_alv IMPLEMENTATION.
  METHOD process_output.
    super->process_output( ).
    WRITE: /(10) space, 'Generating ALV'.
  ENDMETHOD.                    "process_output
ENDCLASS.                    "op_alv IMPLEMENTATION

* ====
CLASS mainapp DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      run IMPORTING
        iv_pdf   TYPE flag
        iv_email TYPE flag
        iv_xls   TYPE flag.
ENDCLASS.                    "mainapp DEFINITION

*
CLASS mainapp IMPLEMENTATION.
  METHOD run.
    DATA: lo_decorator TYPE REF TO output,
          lo_pre TYPE REF TO output.          " Helper Variable

* .... Setup objects
*   standarad object
    CREATE OBJECT lo_decorator TYPE alvoutput.
    lo_pre = lo_decorator.

*   testing Decorator
    IF iv_pdf IS NOT INITIAL.
      CREATE OBJECT lo_decorator
        TYPE
          op_pdf
        EXPORTING
          io_decorator = lo_pre.
      lo_pre = lo_decorator.
    ENDIF.
    IF iv_email IS NOT INITIAL.
      CREATE OBJECT lo_decorator
        TYPE
          op_email
        EXPORTING
          io_decorator = lo_pre.
      lo_pre = lo_decorator.
    ENDIF.
    IF iv_xls IS NOT INITIAL.
      CREATE OBJECT lo_decorator
        TYPE
          op_xls
        EXPORTING
          io_decorator = lo_pre.
      lo_pre  = lo_decorator.
    ENDIF.

    lo_decorator->process_output( ).

  ENDMETHOD.                    "run
ENDCLASS.                    "mainapp IMPLEMENTATION

PARAMETERS: p_pdf AS CHECKBOX,
            p_email AS CHECKBOX,
            p_xls AS CHECKBOX.

START-OF-SELECTION.
  mainapp=>run( iv_pdf = p_pdf
                iv_email = p_email
                iv_xls   = p_xls
                 ).
