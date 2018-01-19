*&---------------------------------------------------------------------*
*& Report  Z_DP_STRATEGY_01
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report z_dp_strategy_01.

interface lif_payment.
  methods pay changing c_val type p.
endinterface.

class lcl_payment_1 definition.
  public section.
    interfaces lif_payment.
    aliases pay for lif_payment~pay.
endclass.

class lcl_payment_2 definition.
  public section.
    interfaces lif_payment.
    aliases pay for lif_payment~pay.
endclass.

class lcl_payment_1 implementation.
  method pay.
    "do something with c_val i.e.
    c_val = c_val - 10.
  endmethod.
endclass.

class lcl_payment_2 implementation.
  method pay.
    "do something else with c_val i.e.
    c_val = c_val + 10.
  endmethod.
endclass.

class lcl_main definition.
  public section.
    "during main object creation you pass which payment you want to use for this object
    methods constructor importing ir_payment type ref to lif_payment.

    "later on you can change this dynamicaly
    methods set_payment importing ir_payment type ref to lif_payment.
    methods show_payment_val.
    methods pay.

  private section.
    data payment_value type p.
    "reference to your interface which you will be working with polimorphically
    data mr_payment type ref to lif_payment.
endclass.

class lcl_main implementation.
  method constructor.
    if ir_payment is bound.
      me->mr_payment = ir_payment.
    endif.
  endmethod.
  method set_payment.
    if ir_payment is bound.
      me->mr_payment = ir_payment.
    endif.
  endmethod.
  method show_payment_val.
    write /: 'Payment value is now ', me->payment_value.
  endmethod.
  "hide fact that you are using composition to access pay method
  method pay.
    mr_payment->pay( changing c_val = payment_value ).
  endmethod.
endclass.


parameters pa_pay type c. "1 - first payment, 2 - second

data gr_main type ref to lcl_main.
data gr_payment type ref to lif_payment.

start-of-selection.
  "client application (which uses stategy pattern)

  case pa_pay.
    when 1.
      "create first type of payment
      create object gr_payment type lcl_payment_1.
    when 2.
      "create second type of payment
      create object gr_payment type lcl_payment_2.
  endcase.
  "pass payment type to main object
  create object gr_main
    exporting
      ir_payment = gr_payment.

  gr_main->show_payment_val( ).
  "now client doesn't know which object it is working with
  gr_main->pay( ).

  gr_main->show_payment_val( ).

  "you can also use set_payment method to set payment type dynamically
  "client would see no change
  if pa_pay = 1.
    "now create different payment to set it dynamically
    CREATE OBJECT gr_payment TYPE lcl_payment_2.

    gr_main->set_payment( gr_payment ).
    gr_main->pay( ).
    gr_main->show_payment_val( ).
  endif.
