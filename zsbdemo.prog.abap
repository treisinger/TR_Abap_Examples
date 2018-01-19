*&---------------------------------------------------------------------*
*& Report  ZSBDEMO
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zsbdemo.

DATA: r_screen TYPE REF TO zcl_screen_breaker.

PARAMETERS: x_opt_1 RADIOBUTTON GROUP opt USER-COMMAND option_selected,
            p_carrid TYPE scarr-carrid,
            x_opt_2 RADIOBUTTON GROUP opt DEFAULT 'X'.
SELECT-OPTIONS: s_carrid FOR p_carrid.

INITIALIZATION.
  CREATE OBJECT r_screen.

AT SELECTION-SCREEN OUTPUT.
  IF x_opt_1 = 'X'.
    r_screen->set_input( 'P_CARRID' ).
    r_screen->set_output( 'S_CARRID-LOW' ).
    r_screen->set_output( 'S_CARRID-HIGH' ).
    r_screen->activate_screen( ).
  ENDIF.

  IF x_opt_2 = 'X'.
    r_screen->set_output( 'P_CARRID' ).
    r_screen->set_input( 'S_CARRID-LOW' ).
    r_screen->set_input( 'S_CARRID-HIGH' ).
    r_screen->activate_screen( ).
  ENDIF.

START-OF-SELECTION.
