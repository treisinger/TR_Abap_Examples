*&---------------------------------------------------------------------*
*& Report  Z_DP_COMMAND_01
*&
*&---------------------------------------------------------------------*
*& This an example of the command pattern from the book
*& Head First Design Patterns
*&
*&---------------------------------------------------------------------*
report  z_dp_command_01_hfdp1.
*&---------------------------------------------------------------------*
*&       Interface LIF_COMMAND
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
interface lif_command.

  methods:
    execute.

endinterface.       "LIF_COMMAND

*&---------------------------------------------------------------------*
*&       Class LCL_LIGHT
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_light definition.

  public section.

    methods:
      on,
      off.

endclass.               "LCL_LIGHT

*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_LIGHT
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_light implementation.

  method on.
    write : / 'Light is on'.
  endmethod.                    "LCL_LIGHT

  method off.
    write : / 'Light is off'.
  endmethod.                    "LCL_LIGHT

endclass.               "LCL_LIGHT

*&---------------------------------------------------------------------*
*&       Class LCL_GARAGE
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_garage definition.

  public section.

    methods:
      up,
      down.

endclass.               "LCL_GARAGE

*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_GARAGE
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_garage implementation.

  method up.
    write : / 'Garage door is up'.
  endmethod.                    "LCL_GARAGE

  method down.
    write : / 'Garage door is down'.
  endmethod.                    "LCL_GARAGE

endclass.               "LCL_GARAGE

*&---------------------------------------------------------------------*
*&       Class LCL_LIGHT_ON_COMMAND
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_light_on_command definition.

  public section.

    interfaces: lif_command.

    data:
      mo_light type ref to lcl_light.

    methods:
      constructor importing io_light type ref to lcl_light.

endclass.               "LCL_LIGHT_ON_COMMAND
*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_LIGHT_ON_COMMAND
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_light_on_command implementation.

  method constructor.
    mo_light = io_light.
  endmethod.                    "constructor

  method lif_command~execute.
    mo_light->on( ).
  endmethod.                    "lif_command~execute

endclass.               "LCL_LIGHT_ON_COMMAND

*&---------------------------------------------------------------------*
*&       Class LCL_GARAGE_OPEN_COMMAND
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_garage_open_command definition.

  public section.

    interfaces: lif_command.

    data:
      mo_garage type ref to lcl_garage.

    methods:
      constructor importing io_garage type ref to lcl_garage.

endclass.               "LCL_GARAGE_OPEN_COMMAND
*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_GARAGE_OPEN_COMMAND
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_garage_open_command implementation.

  method constructor.
    mo_garage = io_garage.
  endmethod.                    "constructor

  method lif_command~execute.
    mo_garage->up( ).
  endmethod.                    "lif_command~execute

endclass.               "LCL_GARAGE_OPEN_COMMAND


*&---------------------------------------------------------------------*
*&       Class LCL_SIMPLE_REMOTE_CONTROL
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_simple_remote_control definition.

  public section.
    data:
      mo_slot type ref to lif_command.

    methods:
      set_command importing io_command type ref to lif_command,
      button_was_pressed.

endclass.               "LCL_SIMPLE_REMOTE_CONTROL
*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_SIMPLE_REMOTE_CONTROL
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_simple_remote_control implementation.

  method set_command.
    mo_slot = io_command.
  endmethod.                    "set_command

  method button_was_pressed.
    mo_slot->execute( ).
  endmethod.                    "button_was_pressed

endclass.               "LCL_SIMPLE_REMOTE_CONTROL

*&---------------------------------------------------------------------*
*&       Class LCL_REMOTE_CONTROL_TEST
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_remote_control_test definition.

  public section.

    class-methods:
      main.

endclass.               "LCL_REMOTE_CONTROL_TEST
*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_REMOTE_CONTROL_TEST
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_remote_control_test implementation.

  method main.

    data:
      lo_remote    type ref to lcl_simple_remote_control,
      lo_light     type ref to lcl_light,
      lo_garage    type ref to lcl_garage,
      lo_light_on  type ref to lcl_light_on_command,
      lo_garage_up type ref to lcl_garage_open_command.

    create object lo_remote.
    create object lo_light.
    create object lo_garage.

    create object lo_light_on
      exporting
        io_light = lo_light.

    create object lo_garage_up
      exporting
        io_garage = lo_garage.

    lo_remote->set_command(
      exporting
        io_command = lo_light_on ).

    lo_remote->button_was_pressed( ).

    lo_remote->set_command(
      exporting
        io_command = lo_garage_up ).

    lo_remote->button_was_pressed( ).

  endmethod.                    "main

endclass.               "LCL_REMOTE_CONTROL_TEST


start-of-selection.

  lcl_remote_control_test=>main( ).
