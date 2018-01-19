*&---------------------------------------------------------------------*
*& Report  Y_COMMAND_I
*&
*&---------------------------------------------------------------------*
*& Transferred example into ABAP of the command pattern by Steven
*& Link: https://www.cuttingedge.it/blogs/steven/pivot/entry.php?id=91
*&
*&---------------------------------------------------------------------*
report y_command_i.

interface lif_cmd_handler.
  methods handle importing i_command type ref to object.
endinterface.


class lcl_move_customer_cmd definition.

  public section.
    data customer_id type i.
    data new_adress  type string.

endclass.
class lcl_move_customer_cmd implementation.

endclass.


class lcl_move_customer_cmd_handler definition.

  public section.
    interfaces lif_cmd_handler.

*  private section.
*    data repository type ref to object. "type ref to i_repository

endclass.
class lcl_move_customer_cmd_handler implementation.

  method lif_cmd_handler~handle.

    data cmd type ref to lcl_move_customer_cmd.

    cmd ?= i_command.

    cl_demo_output=>write( 'I handled the command.' ).
    cl_demo_output=>write( 'Customer: ' && cmd->customer_id ).
    cl_demo_output=>write( 'Adress: ' && cmd->new_adress ).

  endmethod.

endclass.


class lcl_controller definition.

  public section.
    methods constructor   importing i_handler type ref to lif_cmd_handler.
    methods move_customer importing i_customer_id type i i_new_adress type string.

  private section.
    data handler type ref to lif_cmd_handler.

endclass.

class lcl_controller implementation.
  method constructor.

    assert i_handler is bound.
    me->handler = i_handler.

  endmethod.

  method move_customer.

    data(cmd) = new lcl_move_customer_cmd( ).

    cmd->customer_id = i_customer_id.
    cmd->new_adress  = i_new_adress.

    me->handler->handle( cmd ).

    cl_demo_output=>display( ).

  endmethod.

endclass.


class lcl_main definition.

  public section.
    class-methods start.

endclass.
class lcl_main implementation.

  method start.

    data(controller) = new lcl_controller( new lcl_move_customer_cmd_handler( ) ).

    controller->move_customer( i_customer_id = '12345' i_new_adress = 'The new adress' ).

  endmethod.

endclass.

start-of-selection.

  lcl_main=>start( ).
