report znp_dp_decorator.
*&---------------------------------------------------------------------*
*& Purpose : Decorator Design Pattern Demo
*& Author : Naimesh Patel
*& Co-Author: Enno Wulff (Adding the costs of each output)
*&---------------------------------------------------------------------*
* OUTPUT:
* Standard ALV output 1.000,00
* Generating ALV 100,00
* Generating PDF 10,00
* Sending Email 50,00
* ===========
* Total Cost: 1.160,00
*
*----------------------------------------------------------------------*

* ===
class output definition abstract.
  public section.
    data deco_cost type wertv8.
    data cost type wertv8.
    methods:
      process_output abstract,
      calc_cost abstract returning value(rv_cost) type wertv8.

endclass. "output DEFINITION

* ====
class alvoutput definition inheriting from output.
  public section.
    methods:
      process_output redefinition
      , calc_cost redefinition.
    .

endclass. "alvoutput DEFINITION

*
class alvoutput implementation.
  method process_output.
    deco_cost = 1000.
    write: / 'Standard ALV output', 30 deco_cost.
    cost = calc_cost( ).
  endmethod. "process_output
  method calc_cost.
    rv_cost = deco_cost.
  endmethod. "calc_cost

endclass. "alvoutput IMPLEMENTATION

* ====
class opdecorator definition inheriting from output.
  public section.
    methods:
      constructor
        importing io_decorator type ref to output,
      process_output redefinition,
      calc_cost redefinition.

  protected section.
    data: o_decorator type ref to output.
endclass. "opdecorator DEFINITION

*
class opdecorator implementation.
  method constructor.
    super->constructor( ).
    me->o_decorator = io_decorator.
  endmethod. "constructor
  method process_output.
    check o_decorator is bound.
    o_decorator->process_output( ).
  endmethod. "process_output
  method calc_cost.
    rv_cost = o_decorator->cost + deco_cost.
  endmethod. "calc_cost
endclass. "opdecorator IMPLEMENTATION

* =====
class op_pdf definition inheriting from opdecorator.
  public section.
    methods: process_output redefinition.

endclass. "op_pdf DEFINITION

*
class op_pdf implementation.
  method process_output.
    super->process_output( ).
    deco_cost = 10.
    write: /(10) space, 'Generating PDF', 30 deco_cost.
    cost = calc_cost( ).
  endmethod. "process_output
endclass. "op_pdf IMPLEMENTATION

* ======
class op_xls definition inheriting from opdecorator.
  public section.
    methods: process_output redefinition.
endclass. "op_xls DEFINITION

*
class op_xls implementation.
  method process_output.
    super->process_output( ).
    deco_cost = 20.
    write: /(10) space, 'Generating Excel', 30 deco_cost.
    cost = calc_cost( ).
  endmethod. "process_output

endclass. "op_xls IMPLEMENTATION

* =====
class op_email definition inheriting from opdecorator.
  public section.
    methods: process_output redefinition.
endclass. "op_email DEFINITION

*
class op_email implementation.
  method process_output.
    super->process_output( ).
    deco_cost = 50.
    write: /(10) space, 'Sending Email', 30 deco_cost.
    cost = calc_cost( ).

  endmethod. "process_output

endclass. "op_email IMPLEMENTATION

* ====
class op_alv definition inheriting from opdecorator.
  public section.
    methods: process_output redefinition.
endclass. "op_alv DEFINITION

*
class op_alv implementation.
  method process_output.
    super->process_output( ).
    deco_cost = 100.
    write: /(10) space, 'Generating ALV', 30 deco_cost.
    cost = calc_cost( ).
  endmethod. "process_output

endclass. "op_alv IMPLEMENTATION

* ====
class mainapp definition.
  public section.
    class-data l_cost type wertv8.
    class-methods:
      run importing
            iv_pdf   type flag
            iv_email type flag
            iv_xls   type flag.
endclass. "mainapp DEFINITION

*
class mainapp implementation.
  method run.
    data: lo_decorator type ref to output,
          lo_pre       type ref to output. " Helper Variable

* .... Setup objects
* standard object
    create object lo_decorator type alvoutput.

*==> Call Main output: ALV
    lo_pre = lo_decorator.
    create object lo_decorator
      type
      op_alv
      exporting
        io_decorator = lo_pre.

    if iv_pdf is not initial.
*==> Decorating: PDF
      lo_pre = lo_decorator.
      create object lo_decorator
        type
        op_pdf
        exporting
          io_decorator = lo_pre.
    endif.
    if iv_email is not initial.
*==> Decorating: PDF
      lo_pre = lo_decorator.
      create object lo_decorator
        type
        op_email
        exporting
          io_decorator = lo_pre.
    endif.
    if iv_xls is not initial.
*==> Decorating: PDF
      lo_pre = lo_decorator.
      create object lo_decorator
        type
        op_xls
        exporting
          io_decorator = lo_pre.
      lo_pre = lo_decorator.
    endif.

*== Process output
    lo_decorator->process_output( ).
*== Calculate costs
    l_cost = lo_decorator->calc_cost( ).
    write: /40 '==========='.
    write: / 'Total Cost:', 30 l_cost.

  endmethod. "run
endclass. "mainapp IMPLEMENTATION

parameters: p_pdf   as checkbox default 'X',
            p_email as checkbox default 'X',
            p_xls   as checkbox.

start-of-selection.
  mainapp=>run( iv_pdf = p_pdf
  iv_email = p_email
  iv_xls = p_xls
  ).
