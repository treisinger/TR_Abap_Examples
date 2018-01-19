program zkata_restaurant.

class restaurant definition.
  public section.
    methods:
      constructor importing id   type string
                            name type string,
      id          returning value(return) type string,
      name        returning value(return) type string.
  private section.
    data:
      _id   type string,
      _name type string.
endclass.
class restaurant implementation.
  method constructor.
    _id   = id.
    _name = name.
  endmethod.
  method id.
    return = _id.
  endmethod.
  method name.
    return = _name.
  endmethod.
endclass.

interface restaurant_builder.
  methods:
    with_name importing name          type string
              returning value(return) type ref to restaurant_builder,
    build     returning value(return) type ref to restaurant.
endinterface.

class in_memory_restaurant_builder definition for testing.
  public section.
    interfaces:
      restaurant_builder.
    methods:
      constructor importing id type string.
  private section.
    data:
      id   type string,
      name type string.
endclass.
class in_memory_restaurant_builder implementation.
  method constructor.
    me->id = id.
  endmethod.
  method restaurant_builder~build.
    create object return
      exporting
        id   = id
        name = name.
  endmethod.
  method restaurant_builder~with_name.
    me->name = name.
    return   = me.
  endmethod.
endclass.

interface restaurant_store.
  methods:
    create     returning value(return) type ref to restaurant_builder,
    save       importing restaurant    type ref to restaurant,
    find_by_id importing id            type string
               returning value(return) type ref to restaurant.
endinterface.

class in_memory_restaurant_store definition for testing.
  public section.
    interfaces:
      restaurant_store.
  private section.
    types:
      begin of map_entry_type,
        id     type string,
        object type ref to restaurant,
      end of map_entry_type,
      map_type type hashed table of map_entry_type with unique key id.
    data:
      incremental_id type i,
      map            type map_type.
endclass.
class in_memory_restaurant_store implementation.
  method restaurant_store~create.
    add 1 to incremental_id.
    create object return type in_memory_restaurant_builder
      exporting
        id = |{ incremental_id }|.
  endmethod.
  method restaurant_store~find_by_id.
    data entry like line of map.
    read table map into entry with key id = id.
    return = entry-object.
  endmethod.
  method restaurant_store~save.
    data entry like line of map.
    entry-id = restaurant->id( ).
    entry-object = restaurant.
    delete map where id = entry-id.
    insert entry into table map.
  endmethod.
endclass.

interface create_restaurant_request.
  methods:
    name returning value(return) type string.
endinterface.

class create_restaurant_request_stub definition for testing.
  public section.
    interfaces:
      create_restaurant_request.
    methods:
      with_name importing name type string.
  private section.
    data:
      name type string.
endclass.
class create_restaurant_request_stub implementation.
  method create_restaurant_request~name.
    return = name.
  endmethod.
  method with_name.
    me->name = name.
  endmethod.
endclass.

interface create_restaurant_receiver.
  methods:
    created_with_id importing id type string,
    send_message    importing id type string.
endinterface.

class create_restaurant_receiver_spy definition for testing.
  public section.
    interfaces:
      create_restaurant_receiver.
    methods:
      id                    returning value(return) type string,
      has_received_messages importing message_ids   type string_table optional
                            returning value(return) type abap_bool.
  private section.
    data:
      restaurant_id type string,
      message_ids   type string_table.
endclass.
class create_restaurant_receiver_spy implementation.
  method create_restaurant_receiver~created_with_id.
    restaurant_id = id.
  endmethod.
  method create_restaurant_receiver~send_message.
    append id to message_ids.
  endmethod.
  method id.
    return = restaurant_id.
  endmethod.
  method has_received_messages.
    if message_ids is supplied.
      return = boolc( message_ids = me->message_ids ).
    else.
      return = boolc( lines( me->message_ids ) > 0 ).
    endif.
  endmethod.
endclass.

interface command.
  methods:
    execute.
endinterface.

class create_restaurant_command definition.
  public section.
    interfaces:
      command.
    methods:
      constructor importing store    type ref to restaurant_store
                            request  type ref to create_restaurant_request
                            receiver type ref to create_restaurant_receiver.
  private section.
    data:
      store    type ref to restaurant_store,
      request  type ref to create_restaurant_request,
      receiver type ref to create_restaurant_receiver.
endclass.
class create_restaurant_command implementation.
  method constructor.
    me->store = store.
    me->request = request.
    me->receiver = receiver.
  endmethod.
  method command~execute.
    if request->name( ) is initial.
      receiver->send_message( 'EMPTY_NAME' ).
    else.
      data restaurant type ref to restaurant.
      restaurant = store->create( )->with_name( request->name( ) )->build( ).
      store->save( restaurant ).
      receiver->created_with_id( restaurant->id( ) ).
    endif.
  endmethod.
endclass.

class restaurant_test definition for testing risk level harmless duration short create private final.
  private section.
    data:
      store        type ref to restaurant_store,
      request_stub type ref to create_restaurant_request_stub,
      receiver_spy type ref to create_restaurant_receiver_spy,
      command      type ref to command.
    methods:
      setup,
      simple_creation          for testing,
      creating_mutiple         for testing,
      creating_with_empty_name for testing.
endclass.

class restaurant_test implementation.
  method setup.
    create object:
      store type in_memory_restaurant_store,
      request_stub,
      receiver_spy,
      command type create_restaurant_command
        exporting
          store    = store
          request  = request_stub
          receiver = receiver_spy.
  endmethod.
  method simple_creation.
    request_stub->with_name( 'Restaurant name' ).
    command->execute( ).

    cl_abap_unit_assert=>assert_equals( exp = 'Restaurant name'
                                        act = store->find_by_id( receiver_spy->id( ) )->name( ) ).
    cl_abap_unit_assert=>assert_false( receiver_spy->has_received_messages( ) ).
  endmethod.
  method creating_mutiple.
    request_stub->with_name( 'First name' ).
    command->execute( ).

    request_stub->with_name( 'Restaurant name' ).
    command->execute( ).
    data restaurant_id type string.
    restaurant_id = receiver_spy->id( ).

    request_stub->with_name( 'Second name' ).
    command->execute( ).

    cl_abap_unit_assert=>assert_equals( exp = 'Restaurant name'
                                        act = store->find_by_id( restaurant_id )->name( ) ).

  endmethod.
  method creating_with_empty_name.
    request_stub->with_name( '' ).
    command->execute( ).

    cl_abap_unit_assert=>assert_not_bound( store->find_by_id( receiver_spy->id( ) ) ).

    data message_ids type string_table.
    append 'EMPTY_NAME' to message_ids.
    cl_abap_unit_assert=>assert_true( receiver_spy->has_received_messages( message_ids ) ).
  endmethod.
endclass.
