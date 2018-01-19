*&---------------------------------------------------------------------*
*& Report  Z_DP_FACTORY_METHOD_03
*&
*&---------------------------------------------------------------------*
*& This is the transformed example of the Head First Design Pattern
*& Book.
*& Main characteristic for factory method is that the creation is done
*& by the subclass with inheritance.
*&
*&---------------------------------------------------------------------*
report z_dp_factory_method_03.

class lcl_pizza definition abstract.

  public section.

    methods prepare.
    methods bake.
    methods cut.
    methods box.
    methods get_name returning value(r_name) type string.

  protected section.
    data name     type string.
    data dough    type string.
    data sauce    type string.
    data toppings type standard table of string with non-unique default key.


endclass.               "lcl_pizza
class lcl_pizza implementation.

  method prepare.

    field-symbols <topping> type string.

    cl_demo_output=>write( 'Preparing ' && me->name ).
    cl_demo_output=>write( 'Tossing dough...' ).
    cl_demo_output=>write( 'Adding sauce...' ).
    cl_demo_output=>write( 'Adding toppings:' ).

    loop at me->toppings assigning <topping>.
      cl_demo_output=>write( <topping> ).
    endloop.

  endmethod.

  method bake.
    cl_demo_output=>write( 'Bake for 5 minutes at 350 degree celsius' ).
  endmethod.

  method cut.
    cl_demo_output=>write( 'Cutting pizza into diagonal slices ' ).
  endmethod.

  method box.
    cl_demo_output=>write( 'Place pizza in official pizza store box' ).
  endmethod.

  method get_name.
    r_name = me->name.
  endmethod.

endclass.               "lcl_pizza

class lcl_pizza_store definition abstract.

  public section.

*   This method uses in this case the concrete lcl_pizza (product).
    methods order_pizza importing i_pizza_type   type string
                        returning value(r_pizza) type ref to lcl_pizza.

  protected section.

*   This is the factory method. It is defined as abstract, so the sublass of
*   lcl_pizza_store defines how the concrete lcl_pizza is created
    methods create_pizza abstract importing i_pizza_type   type string
                                  returning value(r_pizza) type ref to lcl_pizza.


endclass.               "lcl_pizza_store
class lcl_pizza_store implementation.

  method order_pizza.

    data pizza type ref to lcl_pizza.

*   Here the concrete pizza store will call the redefined create pizza method which knows
*   how to create the concrete pizza
    pizza = me->create_pizza( exporting i_pizza_type = i_pizza_type ).

    pizza->prepare( ).
    pizza->bake( ).
    pizza->cut( ).
    pizza->box( ).

    r_pizza = pizza.

  endmethod.

endclass.               "lcl_pizza_store

class lcl_newyork_cheese_pizza definition inheriting from lcl_pizza.

  public section.

    methods constructor.

endclass.               "lcl_newyork_cheese_pizza
class lcl_newyork_cheese_pizza implementation.

  method constructor.

    data topping type string.

    super->constructor( ).

    me->name  = 'New York style sauce and cheese pizza'.
    me->dough = 'Thin crust Dough'.
    me->sauce = 'Marinara sauce'.

    topping = 'Grated regiano cheese'.
    append topping to me->toppings.

  endmethod.

endclass.               "lcl_newyork_cheese_pizza

class lcl_newyork_veggie_pizza definition inheriting from lcl_pizza.

  public section.
    methods constructor.

endclass.               "lcl_newyork_veggi_pizza
class lcl_newyork_veggie_pizza implementation.

  method constructor.

    data topping type string.

    super->constructor( ).

    me->name  = 'New York style veggie pizza'.
    me->dough = 'Thin crust Dough'.
    me->sauce = 'Marinara sauce'.

    topping = 'Extra onion rings'.
    append topping to me->toppings.

  endmethod.

endclass.               "lcl_newyork_veggi_pizza

class lcl_newyork_pizza_store definition inheriting from lcl_pizza_store.

  protected section.
    methods create_pizza redefinition.

endclass.               "lcl_newyork_pizza_store
class lcl_newyork_pizza_store implementation.

  method create_pizza.

    data pizza type ref to lcl_pizza.

    case i_pizza_type.
      when 'cheese'.
        create object pizza type lcl_newyork_cheese_pizza.
      when 'veggie'.
        create object pizza type lcl_newyork_veggie_pizza.
      when others.
    endcase.

    r_pizza = pizza.

  endmethod.

endclass.               "lcl_newyork_pizza_store

class lcl_chicago_cheese_pizza definition inheriting from lcl_pizza.

  public section.

    methods constructor.
    methods cut redefinition.

endclass.               "lcl_newyork_cheese_pizza
class lcl_chicago_cheese_pizza implementation.

  method constructor.

    data topping type string.

    super->constructor( ).

    me->name  = 'Chicago style deep dish cheese pizza'.
    me->dough = 'Extra thick crust Dough'.
    me->sauce = 'Plum tomato sauce'.

    topping = 'Shredded mozarella cheese'.
    append topping to me->toppings.
    topping = 'A Lot of Tabasco'.
    append topping to me->toppings.

  endmethod.

  method cut.

    cl_demo_output=>write( 'Cutting pizza to square slices' ).

  endmethod.

endclass.               "lcl_newyork_cheese_pizza

class lcl_chicago_veggie_pizza definition inheriting from lcl_pizza.

  public section.
    methods constructor.

endclass.               "lcl_newyork_veggi_pizza
class lcl_chicago_veggie_pizza implementation.

  method constructor.

    data topping type string.

    super->constructor( ).

    me->name  = 'Chicago style veggie pizza'.
    me->dough = 'Extra thick crust Dough'.
    me->sauce = 'Plum tomato sauce'.

    topping = 'Extra onion rings'.
    append topping to me->toppings.
    topping = 'Extra spicey Jalapenous'.
    append topping to me->toppings.

  endmethod.

endclass.               "lcl_newyork_veggi_pizza

class lcl_chicago_pizza_store definition inheriting from lcl_pizza_store.

  protected section.
    methods create_pizza redefinition.

endclass.               "lcl_newyork_pizza_store
class lcl_chicago_pizza_store implementation.

  method create_pizza.

    data pizza type ref to lcl_pizza.

    case i_pizza_type.
      when 'cheese'.
        create object pizza type lcl_chicago_cheese_pizza.
      when 'veggie'.
        create object pizza type lcl_chicago_veggie_pizza.
      when others.
    endcase.

    r_pizza = pizza.

  endmethod.

endclass.               "lcl_newyork_pizza_store


*&---------------------------------------------------------------------*
*&       Class PizzaTestDrive
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_pizza_test_drive definition.

  public section.

    class-methods main.

endclass.               "main

*&---------------------------------------------------------------------*
*&       Class (Implementation)  PizzaTestDrive
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
class lcl_pizza_test_drive implementation.

  method main.

    data ny_pizza_store type ref to lcl_pizza_store.
    data chicago_pizza_store TYPE REF TO lcl_pizza_store.
    data pizza type ref to lcl_pizza.

    create object ny_pizza_store type lcl_newyork_pizza_store.
    pizza = ny_pizza_store->order_pizza( exporting i_pizza_type = 'cheese' ).
    cl_demo_output=>write( 'Ethan ordered a ' && pizza->get_name( ) ).
    cl_demo_output=>display( ).

    pizza = ny_pizza_store->order_pizza( exporting i_pizza_type = 'veggie' ).
    cl_demo_output=>write( 'Rebecca ordered a ' && pizza->get_name( ) ).
    cl_demo_output=>display( ).

    create object chicago_pizza_store type lcl_chicago_pizza_store.
    pizza = chicago_pizza_store->order_pizza( exporting i_pizza_type = 'cheese' ).
    cl_demo_output=>write( 'Jeron ordered a ' && pizza->get_name( ) ).
    cl_demo_output=>display( ).

    pizza = chicago_pizza_store->order_pizza( exporting i_pizza_type = 'veggie' ).
    cl_demo_output=>write( 'Maik ordered a ' && pizza->get_name( ) ).
    cl_demo_output=>display( ).

  endmethod.

endclass.               "main

start-of-selection.

  lcl_pizza_test_drive=>main( ).
