class ZCL_SCREEN_BREAKER definition
  public
  create public .

*"* public components of class ZCL_SCREEN_BREAKER
*"* do not include other source files here!!!
public section.

  methods ACTIVATE_SCREEN .
  methods CONSTRUCTOR .
  methods GET_OBJECT
    importing
      value(I_FIELD_NAME) type SCREEN-NAME
    returning
      value(ES_SCREEN) type SCREEN
    exceptions
      NOT_FOUND .
  methods RESTORE_DEFAULT .
  methods SET_AS_PASS
    importing
      value(I_FIELD_NAME) type SCREEN-NAME
    exceptions
      NOT_FOUND .
  methods SET_CURRENT_SCREEN_AS_DEFAULT .
  methods SET_INPUT
    importing
      value(I_FIELD_NAME) type SCREEN-NAME
    exceptions
      NOT_FOUND .
  methods SET_INVISIBLE
    importing
      value(I_FIELD_NAME) type SCREEN-NAME
    exceptions
      NOT_FOUND .
  methods SET_MANDATORY
    importing
      value(I_FIELD_NAME) type SCREEN-NAME
    exceptions
      NOT_FOUND .
  methods SET_OBJECT
    importing
      value(IS_SCREEN) type SCREEN
    exceptions
      NOT_FOUND .
  methods SET_OPTIONAL
    importing
      value(I_FIELD_NAME) type SCREEN-NAME
    exceptions
      NOT_FOUND .
  methods SET_OUTPUT
    importing
      value(I_FIELD_NAME) type SCREEN-NAME
    exceptions
      NOT_FOUND .
  methods SET_VISIBLE
    importing
      value(I_FIELD_NAME) type SCREEN-NAME
    exceptions
      NOT_FOUND .
  methods STORE_CURRENT_SCREEN .
protected section.
*"* protected components of class ZCL_SCREEN_BREAKER
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_SCREEN_BREAKER
*"* do not include other source files here!!!

  data:
    ascreen TYPE STANDARD TABLE OF screen WITH KEY name .
  data:
    dscreen TYPE STANDARD TABLE OF screen WITH KEY name .
ENDCLASS.



CLASS ZCL_SCREEN_BREAKER IMPLEMENTATION.


method ACTIVATE_SCREEN.

    FIELD-SYMBOLS <fs_screen> TYPE screen.

    LOOP AT SCREEN.
      READ TABLE me->ascreen ASSIGNING <fs_screen>
        WITH TABLE KEY name = screen-name.
      IF sy-subrc IS INITIAL.
        MODIFY SCREEN FROM <fs_screen>.
      ENDIF.
    ENDLOOP.

endmethod.


method CONSTRUCTOR.

    store_current_screen( ).

endmethod.


method GET_OBJECT.

    READ TABLE me->ascreen INTO es_screen
      WITH TABLE KEY name = i_field_name.

    IF sy-subrc <> 0.
      RAISE not_found.
    ENDIF.

endmethod.


method RESTORE_DEFAULT.

    me->ascreen = me->dscreen.

endmethod.


method SET_AS_PASS.

    FIELD-SYMBOLS: <fs_screen> TYPE screen.

    READ TABLE me->ascreen ASSIGNING <fs_screen>
      WITH TABLE KEY name = i_field_name.
    IF <fs_screen> IS ASSIGNED.
      <fs_screen>-invisible = 1.
    ELSE.
      RAISE not_found.
    ENDIF.

endmethod.


method SET_CURRENT_SCREEN_AS_DEFAULT.

    CLEAR me->dscreen.
    LOOP AT SCREEN.
      APPEND screen TO me->dscreen.
    ENDLOOP.

endmethod.


method SET_INPUT.

    FIELD-SYMBOLS: <fs_screen> TYPE screen.

    READ TABLE me->ascreen ASSIGNING <fs_screen>
      WITH TABLE KEY name = i_field_name.
    IF <fs_screen> IS ASSIGNED.
      <fs_screen>-input      = 1.
      <fs_screen>-output     = 1.
      <fs_screen>-display_3d = 1.
      <fs_screen>-invisible  = 0.
      <fs_screen>-active     = 1.
    ELSE.
      RAISE not_found.
    ENDIF.

endmethod.


method SET_INVISIBLE.

    FIELD-SYMBOLS: <fs_screen> TYPE screen.

    READ TABLE me->ascreen ASSIGNING <fs_screen>
      WITH TABLE KEY name = i_field_name.
    IF <fs_screen> IS ASSIGNED.
      <fs_screen>-active = 0.
    ELSE.
      RAISE not_found.
    ENDIF.

endmethod.


method SET_MANDATORY.

    FIELD-SYMBOLS: <fs_screen> TYPE screen.

    READ TABLE me->ascreen ASSIGNING <fs_screen>
      WITH TABLE KEY name = i_field_name.
    IF <fs_screen> IS ASSIGNED.
      <fs_screen>-active   = 1.
      <fs_screen>-input    = 1.
      <fs_screen>-required = 1.
    ELSE.
      RAISE not_found.
    ENDIF.

endmethod.


method SET_OBJECT.

    FIELD-SYMBOLS: <fs_screen> TYPE screen.

    READ TABLE me->ascreen ASSIGNING <fs_screen>
      WITH TABLE KEY name = screen-name.

    IF sy-subrc <> 0.
      RAISE not_found.
    ENDIF.

    <fs_screen> = is_screen.

endmethod.


method SET_OPTIONAL.

    FIELD-SYMBOLS: <fs_screen> TYPE screen.

    READ TABLE me->ascreen ASSIGNING <fs_screen>
      WITH TABLE KEY name = i_field_name.
    IF <fs_screen> IS ASSIGNED.
      <fs_screen>-required = 0.
    ELSE.
      RAISE not_found.
    ENDIF.

endmethod.


method SET_OUTPUT.

    FIELD-SYMBOLS: <fs_screen> TYPE screen.

    READ TABLE me->ascreen ASSIGNING <fs_screen>
      WITH TABLE KEY name = i_field_name.
    IF <fs_screen> IS ASSIGNED.
      <fs_screen>-input      = 0.
      <fs_screen>-output     = 1.
      <fs_screen>-display_3d = 1.
      <fs_screen>-invisible  = 0.
      <fs_screen>-active     = 1.
    ELSE.
      RAISE not_found.
    ENDIF.

endmethod.


method SET_VISIBLE.

    FIELD-SYMBOLS: <fs_screen> TYPE screen.

    READ TABLE me->ascreen ASSIGNING <fs_screen>
      WITH TABLE KEY name = i_field_name.
    IF <fs_screen> IS ASSIGNED.
      <fs_screen>-active = 1.
    ELSE.
      RAISE not_found.
    ENDIF.

endmethod.


method STORE_CURRENT_SCREEN.

    CLEAR me->ascreen.
    LOOP AT SCREEN.
      APPEND screen TO me->ascreen.
    ENDLOOP.

    me->dscreen = me->ascreen.

endmethod.
ENDCLASS.
