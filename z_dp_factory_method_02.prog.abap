*&---------------------------------------------------------------------*
*& Report  Z_DP_FACTORY_METHOD_02
*&
*&---------------------------------------------------------------------*
*& Example for factory method design pattern
*&
*&---------------------------------------------------------------------*

REPORT Z_DP_FACTORY_METHOD_02.

*----------------------------------------------------------------------*
* -> Abstract - CLASS cl_car_base DEFINITION
*----------------------------------------------------------------------*
class cl_car_base definition abstract.

  public section.

    methods:
      set_attributes
        importing
          iv_color type string
          iv_year  type c,

      get_cartype ABSTRACT
        returning
          value(rv_cartype) type string.

  private section.

    data:
      mv_color type string,
      mv_year  type c length 4.

endclass.                    "cl_car_base DEFINITION

*----------------------------------------------------------------------*
* -> Abstract - CLASS cl_car_base IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_car_base implementation.

  method set_attributes.

    me->mv_color = iv_color.
    me->mv_year  = iv_year.

  endmethod.                    "set_attributes

*  method get_cartype.
*
*  endmethod.                    "get_cartype

endclass.                    "cl_car_base IMPLEMENTATION

*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_vectra DEFINITION
*----------------------------------------------------------------------*
class cl_vectra definition inheriting from cl_car_base.

  public section.

    methods get_cartype REDEFINITION.

endclass.                    "cl_vectra DEFINITION

*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_vectra IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_vectra implementation.

  method get_cartype.

    rv_cartype = 'VECTRA'.

  endmethod.                    "get_cartype

endclass.                    "cl_vectra IMPLEMENTATION

*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_agile DEFINITION
*----------------------------------------------------------------------*
class cl_agile definition inheriting from cl_car_base.

  public section.

    methods get_cartype redefinition.

endclass.                    "cl_agile DEFINITION

*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_agile IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_agile implementation.

  method get_cartype.

    rv_cartype = 'AGILE'.

  endmethod.                    "get_cartype

endclass.                    "cl_agile IMPLEMENTATION

*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_prisma DEFINITION
*----------------------------------------------------------------------*
class cl_prisma definition inheriting from cl_car_base.

  public section.

    methods get_cartype redefinition.

endclass.                    "cl_prisma DEFINITION

*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_prisma IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_prisma implementation.

  method get_cartype.

    rv_cartype = 'PRISMA'.

  endmethod.                    "get_cartype

endclass.                    "cl_prisma IMPLEMENTATION

*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_fiesta DEFINITION
*----------------------------------------------------------------------*
class cl_fiesta definition inheriting from cl_car_base.

  public section.

    methods get_cartype redefinition.

endclass.                    "cl_fiesta DEFINITION

*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_fiesta IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_fiesta implementation.

  method get_cartype.

    rv_cartype = 'FIESTA'.

  endmethod.                    "get_cartype

endclass.                    "cl_fiesta IMPLEMENTATION

*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_focus DEFINITION
*----------------------------------------------------------------------*
class cl_focus definition inheriting from cl_car_base.

  public section.

    methods get_cartype redefinition.

endclass.                    "cl_focus DEFINITION

*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_focus IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_focus implementation.

  method get_cartype.

    rv_cartype = 'FOCUS'.

  endmethod.                    "get_cartype

endclass.                    "cl_focus IMPLEMENTATION

*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_ka DEFINITION
*----------------------------------------------------------------------*
class cl_ka definition inheriting from cl_car_base.

  public section.

    methods get_cartype redefinition.

endclass.                    "cl_ka DEFINITION

*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_ka IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_ka implementation.

  method get_cartype.

    rv_cartype = 'KA'.

  endmethod.                    "get_cartype

endclass.                    "cl_ka IMPLEMENTATION


************************************************************************
************************************************************************
*                                                                      *
*                       FACTORY CLASSES DEFINITION                     *
*                                                                      *
************************************************************************

*----------------------------------------------------------------------*
* -> Abstract - CLASS cl_car_factory DEFINITION
*----------------------------------------------------------------------*
class cl_car_factory definition abstract.

  public section.

    methods:
      create_car
        importing
          iv_cartype type n
        exporting
          er_carinstance type ref to cl_car_base.

endclass.                    "cl_car_factory DEFINITION

*----------------------------------------------------------------------*
* Abstract - CLASS cl_car_factory IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_car_factory implementation.

  method create_car.

  endmethod.                    "create_car

endclass.                    "cl_car_factory IMPLEMENTATION


*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_chevrolet_factory DEFINITION
*----------------------------------------------------------------------*
class cl_chevrolet_factory definition inheriting from cl_car_factory.

  public section.

    methods: create_car redefinition.

endclass.                    "cl_chevrolet_factory DEFINITION


*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_chevrolet_factory IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_chevrolet_factory implementation.

  method create_car.

    case iv_cartype.
      when '100'.
        create object er_carinstance type cl_vectra.
      when '200'.
        create object er_carinstance type cl_agile.
      when '300'.
        create object er_carinstance type cl_prisma.

    endcase.

  endmethod.                    "create_car

endclass.                    "cl_chevrolet_factory IMPLEMENTATION


*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_ford_factory DEFINITION
*----------------------------------------------------------------------*
class cl_ford_factory definition inheriting from cl_car_factory.

  public section.

    methods : create_car redefinition.

endclass.                    "cl_ford_factory DEFINITION

*----------------------------------------------------------------------*
* -> Concrete - CLASS cl_ford_factory IMPLEMENTATION
*----------------------------------------------------------------------*
class cl_ford_factory implementation.

  method create_car.

    case iv_cartype.
      when '100'.
        create object er_carinstance type cl_fiesta.
      when '200'.
        create object er_carinstance type cl_focus.
      when '300'.
        create object er_carinstance type cl_ka.

    endcase.

  endmethod.                    "create_car

endclass.                    "cl_ford_factory IMPLEMENTATION


************************************************************************
************************************************************************
*                                                                      *
*                           PROGRAM FLOW LOGIC                         *
*                                                                      *
************************************************************************
start-of-selection.

  parameters:
     p_type1 type n length 3 obligatory default '100',
     p_type2 type n length 3 obligatory default '100'.

  data:
     lr_chevrolet_factory type ref to cl_car_factory,
     lr_ford_factory      type ref to cl_car_factory.

  data:
    lr_car1 type ref to cl_car_base,
    lr_car2 type ref to cl_car_base.

  data:
    lv_type1 type string,
    lv_type2 type string.


  create object:
    lr_chevrolet_factory type cl_chevrolet_factory,
    lr_ford_factory      type cl_ford_factory.

  lr_chevrolet_factory->create_car(
    exporting
      iv_cartype     = p_type1
    importing
      er_carinstance = lr_car1 ).


  lr_ford_factory->create_car(
    exporting
      iv_cartype     = p_type2
    importing
      er_carinstance = lr_car2 ).


  if lr_car1 is bound.

    lr_car1->set_attributes(
      exporting
        iv_color = 'BLACK'
        iv_year  = '2010' ).

    lv_type1 = lr_car1->get_cartype( ).
    write : / 'Car Type : ', lv_type1.
    uline.

  endif.

  if lr_car2 is bound.

    lr_car2->set_attributes(
      exporting
        iv_color = 'WHITE'
        iv_year  = '2015' ).

    lv_type2 = lr_car2->get_cartype(  ).
    write : / 'Car Type : ', lv_type2.

  endif.
