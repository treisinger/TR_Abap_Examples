*&---------------------------------------------------------------------*
*& Report  Y_QUERY_I
*&
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report y_query_i.

*interface lif_query.
*
*endinterface.

interface lif_query_handler.
  methods handle importing i_query type ref to object returning value(r_result) type ref to data.

endinterface.

class lcl_find_msgs_by_area_query definition.
  public section.
    data message_area type arbgb.

endclass.
class lcl_find_msgs_by_area_query implementation.
endclass.


class lcl_find_msgs_by_area_handler definition.

  public section.
    interfaces lif_query_handler.

endclass.
class lcl_find_msgs_by_area_handler implementation.

  method lif_query_handler~handle.

    data result type ref to data.
    field-symbols <result> type any table.
    data query type ref to lcl_find_msgs_by_area_query.

    query ?= i_query.

    create data result type standard table of t100 with non-unique default key.

    assign result->* to <result>.

    select * from t100 into table <result> up to 10 rows
      where sprsl = sy-langu and arbgb = query->message_area.

    if sy-subrc = 0.
      cl_demo_output=>write( 'I handled the query' ).
    endif.

    r_result = result.

  endmethod.

endclass.

class lcl_controller definition.

  public section.
    methods constructor   importing i_handler type ref to lif_query_handler.
    methods find_messages importing i_message_area type arbgb.

  private section.
    data handler type ref to lif_query_handler.

endclass.
class lcl_controller implementation.
  method constructor.

    assert i_handler is bound.
    me->handler = i_handler.

  endmethod.
  method find_messages.

    types y_messages type standard table of t100 with non-unique default key.

*   create data reference
    data messages type ref to data.
    messages  = new y_messages( ).

*   create query instance
    data(query) = new lcl_find_msgs_by_area_query( ).

*   set query attribute
    query->message_area = i_message_area.

*   process and return messages
    messages = me->handler->handle( query ).

    field-symbols <messages> type any.
    assign messages->* to <messages>.

    cl_demo_output=>write( <messages> ).
    cl_demo_output=>display( ).

  endmethod.
endclass.

class lcl_main definition.

  public section.
    class-methods start.

endclass.
class lcl_main implementation.

  method start.

    data(handler) = new lcl_find_msgs_by_area_handler( ).

    data(controller) = new lcl_controller( handler ).

    controller->find_messages( i_message_area = 'CK' ).

  endmethod.

endclass.

start-of-selection.

  lcl_main=>start( ).
