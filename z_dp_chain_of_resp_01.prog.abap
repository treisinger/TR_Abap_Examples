*&---------------------------------------------------------------------*
*& Report  Z_DP_CHAIN_OF_RESP_01
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report z_dp_chain_of_resp_01.

class cl_abap_typedescr definition load.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
data: moff type i
 ,slen type i
 ,mlen type i.
*---------------------------------------------------------------------*
* Macro ?get_class_name returns the class name of the class
* Importing &1 Any Class
* Exporting &2 The name of the Class &1
*----------------------------------------------------------------------*
define ?get_class_name.
  &2 = cl_abap_classdescr=>get_class_name( &1 ).
  find regex 'CLASS=' in &2 match offset moff match length mlen.
  slen = moff + mlen.
  shift &2 by slen places left.
end-of-definition.
*----------------------------------------------------------------------*
* CLASS handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class handler definition abstract.
  public section.
    methods:
      setsuccessor importing successor type ref to handler
      ,handlerequest abstract importing request type i.

   protected section.
    data:
    successor type ref to handler.

endclass. "handler DEFINITION
*----------------------------------------------------------------------*
* CLASS handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class handler implementation.
  method setsuccessor.
    me->successor = successor.
  endmethod. "setsuccessor

endclass. "handler IMPLEMENTATION
*----------------------------------------------------------------------*
* CLASS concretehandler1 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class concretehandler1 definition inheriting from handler.
  public section.
    methods:
      handlerequest redefinition.

endclass. "concretehandler1 DEFINITION
*----------------------------------------------------------------------*
* CLASS concretehandler1 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class concretehandler1 implementation.
  method handlerequest.

    data: class_name type abap_abstypename.
    ?get_class_name me class_name.

    if ( request >= 0 and request < 10 ).
      write: / class_name, 'Handled request', request.
    elseif ( not successor is initial ).
      me->successor->handlerequest( request ).
    endif.

  endmethod. "handlerequest
endclass. "concretehandler1 IMPLEMENTATION
*----------------------------------------------------------------------*
* CLASS concretehandler2 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class concretehandler2 definition inheriting from handler.

  public section.

    methods:

      handlerequest redefinition.

endclass. "concretehandler2 DEFINITION
*----------------------------------------------------------------------*
* CLASS concretehandler2 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class concretehandler2 implementation.
  method handlerequest.

    data: class_name type abap_abstypename.

    ?get_class_name me class_name.

    if ( request >= 10 and request < 20 ).
      write: / class_name, 'Handled request', request.
    elseif ( not successor is initial ).
      me->successor->handlerequest( request ).
    endif.

  endmethod. "handlerequest
endclass. "concretehandler2 IMPLEMENTATION
*----------------------------------------------------------------------*
* CLASS concretehandler3 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class concretehandler3 definition inheriting from handler.
  public section.
    methods:
      handlerequest redefinition.

endclass. "concretehandler3 DEFINITION
*----------------------------------------------------------------------*
* CLASS concretehandler3 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class concretehandler3 implementation.

  method handlerequest.

    data: class_name type abap_abstypename.

    ?get_class_name me class_name.

    if ( request >= 20 and request < 30 ).
      write: / class_name, 'Handled request', request.
    elseif ( not successor is initial ).
      me->successor->handlerequest( request ).
    endif.

  endmethod. "handlerequest
endclass. "concretehandler3 IMPLEMENTATION
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
      exc_ref  type ref to cx_root
      ,exc_text type string.

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
    h1 type ref to concretehandler1
    ,h2 type ref to concretehandler2
    ,h3 type ref to concretehandler3
    ,t_request type table of i
    ,request type i
    .

* create objects
    create object h1.
    create object h2.
    create object h3.

    h1->setsuccessor( h2 ).
    h2->setsuccessor( h3 ).

    insert 2 into table t_request.
    insert 5 into table t_request.
    insert 13 into table t_request.
    insert 22 into table t_request.
    insert 18 into table t_request.
    insert 3 into table t_request.
    insert 27 into table t_request.
    insert 20 into table t_request.

    loop at t_request into request.
      h1->handlerequest( request ).
    endloop.

  endmethod. "constructor

endclass. "lcl_application IMPLEMENTATION

start-of-selection.
  lcl_application=>run( ).
