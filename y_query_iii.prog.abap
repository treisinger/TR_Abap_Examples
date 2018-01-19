*&---------------------------------------------------------------------*
*& Report  Y_QUERY_III
*&
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report y_query_iii.

interface lif_query_handler.
  methods handle importing i_query type ref to object exporting e_result type any.

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

    types y_messages type standard table of t100 with non-unique default key.

    data result type ref to data.
    field-symbols <result> type any table.

    data query type ref to lcl_find_msgs_by_area_query.

    query ?= i_query.


*    create data result type standard table of t100 with non-unique default key.
    result = new y_messages( ).

    assign result->* to <result>.

    select * from t100 into table <result> up to 10 rows
      where sprsl = sy-langu and arbgb = query->message_area.

    if sy-subrc = 0.
      cl_demo_output=>write( 'I handled the query' ).
    endif.

    e_result = <result>.

  endmethod.
endclass.

class lcl_msgs_by_area_handler_deco definition.
  public section.
    interfaces lif_query_handler.
    methods constructor importing i_decorated_handler type ref to lif_query_handler.

  private section.
    data decorated_handler type ref to lif_query_handler.

endclass.
class lcl_msgs_by_area_handler_deco implementation.
  method constructor.

    assert i_decorated_handler is bound.
    me->decorated_handler = i_decorated_handler.

  endmethod.

  method lif_query_handler~handle.

    data query type ref to lcl_find_msgs_by_area_query.

    cl_demo_output=>write( 'I decorated the query before.' ).
    cl_demo_output=>write( 'Check if buffered data is available' ).

    me->decorated_handler->handle( exporting i_query = i_query importing e_result = e_result ).

    cl_demo_output=>write( 'I decorated the query after.' ).
    cl_demo_output=>write( 'Write success message into the Log' ).


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

*   create query instance
    data(query) = new lcl_find_msgs_by_area_query( ).

*   set query attribute
    query->message_area = i_message_area.

*   process and return messages
    data messages type standard table of t100 with non-unique default key.
    me->handler->handle( exporting i_query = query importing e_result = messages ).

    cl_demo_output=>write( messages ).
    cl_demo_output=>display( ).

  endmethod.
endclass.

class lcl_main definition.

  public section.
    class-methods start.

endclass.
class lcl_main implementation.

  method start.

    data handler type ref to lif_query_handler.

    handler = new lcl_msgs_by_area_handler_deco( new lcl_find_msgs_by_area_handler( ) ).

    data(controller) = new lcl_controller( handler ).

    controller->find_messages( i_message_area = 'CK' ).

  endmethod.

endclass.

start-of-selection.

  lcl_main=>start( ).
