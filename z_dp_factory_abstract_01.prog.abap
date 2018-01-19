*&---------------------------------------------------------------------*
*& Report  Z_DP_FACTORY_METHOD_03
*&
*&---------------------------------------------------------------------*
*& This is the transformed example of the Head First Design Pattern
*& Book.
*& Main characteristic for abstarct factory method is that the creation
*& is done by the composition of an ingrdient factory.
*&
*&---------------------------------------------------------------------*
report z_dp_factory_abstract_01.

interface lif_dough.
  methods get returning value(r_dough) type string.
endinterface.
interface lif_sauce.
  methods get returning value(r_sauce) type string.
endinterface.
interface lif_cheese.
  methods get returning value(r_cheese) type string.
endinterface.

class lcl_thin_crust_dough definition.
  public section.
    interfaces lif_dough.
endclass.
class lcl_thin_crust_dough implementation.
  method lif_dough~get.
    r_dough = 'Thin crust dough'.
  endmethod.
endclass.

class lcl_thick_crust_dough definition.
  public section.
    interfaces lif_dough.
endclass.
class lcl_thick_crust_dough implementation.
  method lif_dough~get.
    r_dough = 'Thick crust dough'.
  endmethod.
endclass.

class lcl_marinara_sauce definition.
  public section.
    interfaces lif_sauce.
endclass.
class lcl_marinara_sauce implementation.
  method lif_sauce~get.
    r_sauce = 'Delicious marinara sauce'.
  endmethod.
endclass.

class lcl_plum_tomato_sauce definition.
  public section.
    interfaces lif_sauce.
endclass.
class lcl_plum_tomato_sauce implementation.
  method lif_sauce~get.
    r_sauce = 'Plum tomato sauce'.
  endmethod.
endclass.

class lcl_mozarella_cheese definition.
  public section.
    interfaces lif_cheese.
endclass.
class lcl_mozarella_cheese implementation.
  method lif_cheese~get.
    r_cheese = 'Shredded mozarella cheese'.
  endmethod.
endclass.

class lcl_parmesan_cheese definition.
  public section.
    interfaces lif_cheese.
endclass.
class lcl_parmesan_cheese implementation.
  method lif_cheese~get.
    r_cheese = 'Parmesan cheese'.
  endmethod.
endclass.

interface lif_ingredient_factory.
*  types y_veggies type standard table of string with non-unique default key.
*
*  data dough     type ref to lif_dough.
*  data sauce     type ref to lif_sauce.
*  data cheese    type ref to lif_cheese.
*  data veggies   type y_veggies.
*  data pepperoni type string.
*  data clams     type string.


* The "create"-Methods in the ingredient factory interface is abstract, which
* means that the conctrete product which is created by the implementing class
* uses a "factory" method. So "factory" methods are use within abstract factories

  methods create_dough returning value(r_dough) type string.
  methods create_sauce returning value(r_sauce) type string.
  methods create_cheese returning value(r_cheese) type string.
*  methods create_veggies returning value(r_veggies) type y_veggies.
*  methods create_pepperoni returning value(r_pepperoni) type string.
*  methods create_clams returning value(r_clams) type string.
endinterface.

class lcl_ny_ingredient_factory definition.

  public section.
    interfaces lif_ingredient_factory.

endclass.
class lcl_ny_ingredient_factory implementation.

  method lif_ingredient_factory~create_dough.
    data dough type ref to lif_dough.
    create object dough type lcl_thin_crust_dough.
    r_dough = dough->get( ).
  endmethod.

  method lif_ingredient_factory~create_sauce.
    data sauce type ref to lif_sauce.
    create object sauce type lcl_marinara_sauce.
    r_sauce = sauce->get( ).
  endmethod.

  method lif_ingredient_factory~create_cheese.
    data cheese type ref to lif_cheese.
    create object cheese type lcl_mozarella_cheese.
    r_cheese = cheese->get( ).
  endmethod.

endclass.

class lcl_ch_ingredient_factory definition.

  public section.
    interfaces lif_ingredient_factory.

endclass.
class lcl_ch_ingredient_factory implementation.

  method lif_ingredient_factory~create_dough.
    data dough type ref to lif_dough.
    create object dough type lcl_thick_crust_dough.
    r_dough = dough->get( ).
  endmethod.

  method lif_ingredient_factory~create_sauce.
    data sauce type ref to lif_sauce.
    create object sauce type lcl_plum_tomato_sauce.
    r_sauce = sauce->get( ).
  endmethod.

  method lif_ingredient_factory~create_cheese.
    data cheese type ref to lif_cheese.
    create object cheese type lcl_parmesan_cheese.
    r_cheese = cheese->get( ).
  endmethod.

endclass.

class lcl_pizza definition abstract.

  public section.
    methods prepare abstract.
    methods bake.
    methods cut.
    methods box.
    methods get_name returning value(r_name) type string.
    methods set_name importing i_name type string.

  protected section.
    data name     type string.
    data dough    type string.
    data sauce    type string.
    data cheese   type string.


endclass.               "lcl_pizza

class lcl_pizza implementation.
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

  method set_name.
    me->name = i_name.
  endmethod.
endclass.               "lcl_pizza

class lcl_cheese_pizza definition inheriting from lcl_pizza.

  public section.
    data ingredient_factory type ref to lif_ingredient_factory.

    methods constructor importing i_ingredient_factory type ref to lif_ingredient_factory.

    methods prepare redefinition.
endclass.

class lcl_cheese_pizza implementation.
  method constructor.

    super->constructor( ).

    me->ingredient_factory = i_ingredient_factory.

  endmethod.

  method prepare.

    cl_demo_output=>write( 'Preparing ' && me->name ).
    me->dough  = me->ingredient_factory->create_dough( ).
    me->sauce  = me->ingredient_factory->create_sauce( ).
    me->cheese = me->ingredient_factory->create_cheese( ).

  endmethod.
endclass.

class lcl_veggie_pizza definition inheriting from lcl_pizza.

  public section.
    data ingredient_factory type ref to lif_ingredient_factory.

    methods constructor importing i_ingredient_factory type ref to lif_ingredient_factory.

    methods prepare redefinition.
endclass.

class lcl_veggie_pizza implementation.
  method constructor.

    super->constructor( ).

    me->ingredient_factory = i_ingredient_factory.

  endmethod.

  method prepare.

    cl_demo_output=>write( 'Preparing ' && me->name ).
    me->dough  = ingredient_factory->create_dough( ).
    me->sauce  = ingredient_factory->create_sauce( ).
    me->cheese = ingredient_factory->create_cheese( ).

  endmethod.
endclass.


class lcl_pizza_store definition abstract.

  public section.

    methods order_pizza
      importing i_pizza_type type string returning value(r_pizza) type ref to lcl_pizza.

  protected section.
    methods create_pizza abstract
      importing i_pizza_type type string returning value(r_pizza) type ref to lcl_pizza.

endclass.               "lcl_pizza_store
class lcl_pizza_store implementation.

  method order_pizza.

    data pizza type ref to lcl_pizza.

    pizza = me->create_pizza( exporting i_pizza_type = i_pizza_type ).

    pizza->bake( ).
    pizza->cut( ).
    pizza->box( ).

    r_pizza = pizza.

  endmethod.

endclass.               "lcl_pizza_store

class lcl_ny_pizza_store definition inheriting from lcl_pizza_store.

  protected section.
    methods create_pizza redefinition.

endclass.               "lcl_newyork_pizza_store
class lcl_ny_pizza_store implementation.

  method create_pizza.

    data pizza type ref to lcl_pizza.
    data ingredient_factory type ref to lif_ingredient_factory.

    create object ingredient_factory type lcl_ny_ingredient_factory.

    case i_pizza_type.
      when 'cheese'.
        create object pizza type lcl_cheese_pizza exporting i_ingredient_factory = ingredient_factory.
        pizza->set_name( exporting i_name = 'New York style cheese pizza' ).
      when 'veggie'.
        create object pizza type lcl_veggie_pizza exporting i_ingredient_factory = ingredient_factory.
        pizza->set_name( exporting i_name = 'New York style veggie pizza' ).
      when others.
    endcase.

    r_pizza = pizza.

  endmethod.

endclass.               "lcl_newyork_pizza_store

class lcl_ch_pizza_store definition inheriting from lcl_pizza_store.

  protected section.
    methods create_pizza redefinition.

endclass.               "lcl_chicago_pizza_store
class lcl_ch_pizza_store implementation.

  method create_pizza.

    data pizza type ref to lcl_pizza.
    data ingredient_factory type ref to lif_ingredient_factory.

    create object ingredient_factory type lcl_ch_ingredient_factory.

    case i_pizza_type.
      when 'cheese'.
        create object pizza type lcl_cheese_pizza exporting i_ingredient_factory = ingredient_factory.
        pizza->set_name( exporting i_name = 'Chicago style cheese pizza' ).
      when 'veggie'.
        create object pizza type lcl_veggie_pizza exporting i_ingredient_factory = ingredient_factory.
        pizza->set_name( exporting i_name = 'Chicago style veggie pizza' ).
      when others.
    endcase.

    r_pizza = pizza.

  endmethod.

endclass.               "lcl_chicago_pizza_store

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
    data chicago_pizza_store type ref to lcl_pizza_store.
    data pizza type ref to lcl_pizza.


    create object ny_pizza_store type lcl_ny_pizza_store.
    pizza = ny_pizza_store->order_pizza( exporting i_pizza_type = 'cheese' ).
    cl_demo_output=>write( 'Ethan ordered a ' && pizza->get_name( ) ).
    cl_demo_output=>display( ).

    pizza = ny_pizza_store->order_pizza( exporting i_pizza_type = 'veggie' ).
    cl_demo_output=>write( 'Rebecca ordered a ' && pizza->get_name( ) ).
    cl_demo_output=>display( ).

    create object chicago_pizza_store type lcl_ch_pizza_store.
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
