*&---------------------------------------------------------------------*
*& Report  Z_DP_COMMAND_02
*&
*&---------------------------------------------------------------------*
*& Example for the command design pattern
*&
*&---------------------------------------------------------------------*
report  z_dp_command_02.

*----------------------------------------------------------------------*
* CLASS receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class receiver definition.
  public section.
    methods:
      action.
endclass. "receiver DEFINITION
*----------------------------------------------------------------------*
* CLASS receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class receiver implementation.
  method action.
    write: / 'Console.WriteLine(Called Receiver.Action()'.
  endmethod. "action

endclass. "receiver IMPLEMENTATION


*----------------------------------------------------------------------*
* CLASS command DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class command definition abstract.
  public section.
    methods:
    constructor importing receiver type ref to receiver,
    execute abstract.
  protected section.
    data: receiver type ref to receiver.
endclass. "command DEFINITION
*----------------------------------------------------------------------*
* CLASS command IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class command implementation.
  method constructor.
    me->receiver = receiver.
  endmethod. "constructor
endclass. "command IMPLEMENTATION


*----------------------------------------------------------------------*
* CLASS concretecommand DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class concretecommand definition inheriting from command.
  public section.
    methods:
    execute redefinition.
endclass. "concretecommand DEFINITION


*----------------------------------------------------------------------*
* CLASS concretecommand IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class concretecommand implementation.
  method execute.
    me->receiver->action( ).
  endmethod. "execute
endclass. "concretecommand IMPLEMENTATION


*----------------------------------------------------------------------*
* CLASS invoker DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class invoker definition.
  public section.
    methods:
    setcommand importing command type ref to command,
    executecommand .
  private section.
    data: command type ref to command.
endclass. "invoker DEFINITION
*----------------------------------------------------------------------*
* CLASS invoker IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class invoker implementation.
  method setcommand.
    me->command = command.
  endmethod. "setcommand
  method executecommand.
    me->command->execute( ).
  endmethod. "executecommand
endclass. "invoker IMPLEMENTATION


*----------------------------------------------------------
* CLASS lcl_application DEFINITION
*----------------------------------------------------------
class lcl_application definition create private.
  public section.
    class-methods: run.
    methods: constructor.
  private section.
    class-data: so_application type ref to lcl_application.
endclass. "lcl_application DEFINITION
*----------------------------------------------------------
* IMPLEMENTATION
*----------------------------------------------------------
class lcl_application implementation.
*----------------------------------------------------------
* LCL_APPLICATION->RUN().
*----------------------------------------------------------
  method run.
    data:
    exc_ref type ref to cx_root,
    exc_text type string.
    if lcl_application=>so_application is initial.
      try.
          create object lcl_application=>so_application.
        catch cx_sy_create_object_error into exc_ref.
          exc_text = exc_ref->get_text( ).
          message exc_text type 'I'.
      endtry.
    endif.
  endmethod. "run
*----------------------------------------------------------
* LCL_APPLICATION->CONSTRUCTOR().
* Use the constructor for instantiating internal objects,
* fields, tables and events.
*----------------------------------------------------------
  method constructor.
    data:
    receiver type ref to receiver,
    command type ref to concretecommand,
    invoker type ref to invoker.

    create object receiver.
    create object command
      exporting
        receiver = receiver.
    create object invoker.
* set and execute command
    invoker->setcommand( command ).
    invoker->executecommand( ).
  endmethod. "constructor
endclass. "lcl_application IMPLEMENTATION

start-of-selection.
  lcl_application=>run( ).
