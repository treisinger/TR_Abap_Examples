*&amp;-------------------------------------------------------*
*&amp; Author  : Naimesh Patel
*&amp; Purpose : Open-Closed Principle (Business Scenario)
*&amp;-------------------------------------------------------*

report  ztest_np_ocp.

*--------------------------------------------------------*
*       CLASS lcx_exc  DEFINITION
*--------------------------------------------------------*
*       Local Exception Class
*--------------------------------------------------------*
class lcx_exc definition
              inheriting from cx_static_check.

  public section.
    data: wf_text type string.

    methods:
      constructor
        importing if_text type string.

endclass.                    "lcx_exc  DEFINITIO

*--------------------------------------------------------*
*       INTERFACE lif_subobj
*--------------------------------------------------------*
interface lif_subobj.

  data: f_key      type string.

  methods:
    is_valid
      returning
        value(rf_valid) type boolean.

endinterface.                    "lif_subobj

*--------------------------------------------------------*
*       CLASS lcl_matnr DEFINITION
*--------------------------------------------------------*
class lcl_matnr definition.
  public section.

    interfaces: lif_subobj.

    methods:
      constructor
        importing
          if_matnr type mara-matnr.

endclass.                    "lcl_matnr DEFINITION

*--------------------------------------------------------*
*       CLASS lcl_kunnr DEFINITION
*--------------------------------------------------------*
*
*--------------------------------------------------------*
class lcl_kunnr definition.

  public section.

    interfaces: lif_subobj.

    methods:
      constructor
        importing
          if_kunnr type kna1-kunnr.

endclass.                    "lcl_kunnr DEFINITION

*--------------------------------------------------------*
*       CLASS lcl_so DEFINITION
*--------------------------------------------------------*
class lcl_so definition.

  public section.

    types: lty_t_obj type standard table of ref to lif_subobj.

    data: la_header type vbak,
          lt_item   type standard table of vbap,
          lt_obj    type lty_t_obj.

    methods:
      validate_keys
        raising
          lcx_exc.

endclass.                    "lcl_so DEFINITION

*--------------------------------------------------------*
*       Selection Screen Place holder
*--------------------------------------------------------*
parameters: p_test type flag.

*--------------------------------------------------------*
*       Start of selection
*--------------------------------------------------------*
start-of-selection.
  data: lo_so  type ref to lcl_so,
        lo_obj type ref to lif_subobj,
        la_obj like line of lo_so->lt_obj,
        lo_exc type ref to lcx_exc.

  create object lo_so.

* Customer
  create object lo_obj type lcl_kunnr
    exporting
      if_kunnr = '0001000072'.
  append la_obj to lo_so->lt_obj.

* Materials
  create object lo_obj type lcl_matnr
    exporting
      if_matnr = 'TEST1'.
  append la_obj to lo_so->lt_obj.

* Sales order processing
  try.
      lo_so->validate_keys( ).
    catch lcx_exc into lo_exc.
      message lo_exc->wf_text type 'I'.
  endtry.

*--------------------------------------------------------*
*       CLASS lcx_exc IMPLEMENTATION
*--------------------------------------------------------*
class lcx_exc implementation.

  method constructor.

    super->constructor( ).
    me->wf_text = if_text.

  endmethod.                    "constructor

endclass.                    "lcx_exc IMPLEMENTATION

*--------------------------------------------------------*
*       CLASS lcl_so IMPLEMENTATION
*--------------------------------------------------------*
class lcl_so implementation.

  method validate_keys.
* This method is closed for the changes but open for
*   adding a new code
* We can add any new method call or do something with
*   the objects from the table, but we don't need to
*   change the code which calls IS_VALID method as long
*   as the object reference is avaliable object table

    data: la_obj    like line of me->lt_obj,
          lo_subobj type ref to lif_subobj,
          lf_valid  type boolean.

    loop at me->lt_obj into la_obj.
      lo_subobj ?= la_obj.
      lf_valid = lo_subobj->is_valid( ).
      if lf_valid is initial.
        raise exception type lcx_exc
          exporting
            if_text = 'Not Valid'.
      endif.
    endloop.

  endmethod.                    "validate_keys


endclass.                    "lcl_so IMPLEMENTATION

*--------------------------------------------------------*
*       CLASS lcl_matnr IMPLEMENTATION
*--------------------------------------------------------*
class lcl_matnr implementation.

  method constructor.

*   set the key
    me->lif_subobj~f_key = if_matnr.

  endmethod.                    "constructor


  method lif_subobj~is_valid.

    data: lf_matnr type matnr.

*   check if it is valid
    select single matnr
           into lf_matnr
           from mara
           where matnr = me->lif_subobj~f_key.
    if sy-subrc = 0.
      rf_valid = 'X'.
    endif.

  endmethod.                    "lif_subobj~is_valid

endclass.                    "lcl_matnr IMPLEMENTATION

*--------------------------------------------------------*
*       CLASS lcl_kunnr IMPLEMENTATION
*--------------------------------------------------------*
*       Customer object
*--------------------------------------------------------*
class lcl_kunnr implementation.

  method constructor.

*   set the key
    me->lif_subobj~f_key = if_kunnr.

  endmethod.                    "constructor

  method lif_subobj~is_valid.

    data: lf_kunnr type kna1-kunnr.

*   check if it is valid
    select single kunnr
           into lf_kunnr
           from kna1
           where kunnr = me->lif_subobj~f_key.
    if sy-subrc = 0.
      rf_valid = 'X'.
    endif.

  endmethod.                    "lif_subobj~is_valid

endclass.                    "lcl_kunnr IMPLEMENTATION
