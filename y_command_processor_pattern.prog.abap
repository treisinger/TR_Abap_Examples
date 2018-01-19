*&---------------------------------------------------------------------*
*& Report  Y_COMMAND_PROCESSOR_PATTERN
*&
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report y_command_processor_pattern.

interface lif_command.
endinterface.


* Some useless comment

interface lif_cmd_handler.
  methods handle importing i_command type ref to lif_command.
endinterface.


class lcl_move_customer_cmd definition.

  public section.
    interfaces lif_command.

    data customer_id type i.
    data new_adress  type string.

endclass.
class lcl_move_customer_cmd implementation.
endclass.


class lcl_move_customer_cmd_handler definition.

  public section.
    interfaces lif_cmd_handler.

endclass.
class lcl_move_customer_cmd_handler implementation.
  method lif_cmd_handler~handle.

    data cmd type ref to lcl_move_customer_cmd.
    cmd ?= i_command.

    cl_demo_output=>write( 'I handled the command.' ).
    cl_demo_output=>write( 'Customer:' && ` ` && cmd->customer_id ).
    cl_demo_output=>write( 'Adress:' && ` ` && cmd->new_adress ).

  endmethod.
endclass.


class lcl_customer_cmd_decorator definition.
  public section.
    interfaces lif_cmd_handler.

    methods constructor importing i_decorated_handler type ref to lif_cmd_handler.

  private section.
    data decorated_handler type ref to lif_cmd_handler.

endclass.
class lcl_customer_cmd_decorator implementation.
  method constructor.

*   Constructor injection.
    assert i_decorated_handler is bound.
    me->decorated_handler = i_decorated_handler.

  endmethod.
  method lif_cmd_handler~handle.

    data cmd type ref to lcl_move_customer_cmd.

    cmd ?= i_command.

    cl_demo_output=>write( 'I decorated the command before.' ).
    cl_demo_output=>write( 'Customer ID validated.' ).

    me->decorated_handler->handle( cmd ).

    cl_demo_output=>write( 'I decorated the command after.' ).
    cl_demo_output=>write( 'Save Customer ID with new adress.' ).

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

*   Constructor injection. Assert ensures that controller is working correct
    assert i_handler is bound.
    me->handler = i_handler.

  endmethod.
  method move_customer.

    data(cmd) = new lcl_move_customer_cmd( ).

    cmd->customer_id = i_customer_id.
    cmd->new_adress  = i_new_adress.

*   Passing the data object to the handler
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

    data(handler) = new lcl_customer_cmd_decorator( new lcl_move_customer_cmd_handler( ) ).
    data(controller) = new lcl_controller( handler ).

    controller->move_customer( i_customer_id = '12345' i_new_adress = 'The new adress' ).

  endmethod.
endclass.


start-of-selection.
  lcl_main=>start( ).
