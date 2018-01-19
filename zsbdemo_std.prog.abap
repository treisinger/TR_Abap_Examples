*&---------------------------------------------------------------------*
*& Report  ZSBDEMO_STD
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zsbdemo_std.

PARAMETERS: x_opt_1 RADIOBUTTON GROUP opt USER-COMMAND option_selected,
            p_carrid TYPE scarr-carrid,
            x_opt_2 RADIOBUTTON GROUP opt DEFAULT 'X'.
SELECT-OPTIONS: s_carrid FOR p_carrid.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'P_CARRID'.
        IF x_opt_1 = 'X'.
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
      WHEN 'S_CARRID-LOW'.
        IF x_opt_1 = 'X'.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      WHEN 'S_CARRID-HIGH'.
        IF x_opt_1 = 'X'.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.
