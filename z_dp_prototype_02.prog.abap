*&---------------------------------------------------------------------*
*& Report  Z_DP_PROTOTYPE_02
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT Z_DP_PROTOTYPE_02.

*----------------------------------------------------------------------*
*       CLASS prototype DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS prototype DEFINITION ABSTRACT.
  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING id TYPE string
     ,get_id RETURNING value(id) TYPE string
     ,clone ABSTRACT RETURNING value(prototype) TYPE REF TO prototype
     .
  PRIVATE SECTION.
    DATA: id TYPE string.
ENDCLASS.                    "prototype DEFINITION

*----------------------------------------------------------------------*
*       CLASS prototype IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS prototype IMPLEMENTATION.
  METHOD constructor.
    me->id = id.
  ENDMETHOD.                    "constructor
  METHOD get_id.
    id = me->id.
      ENDMETHOD.                    "get_id
ENDCLASS.                    "prototype IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS concreteprototype1 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS concreteprototype1 DEFINITION INHERITING FROM prototype.
  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION.
ENDCLASS.                    "concreteprototype1 DEFINITION
*----------------------------------------------------------------------*
*       CLASS concreteprototype1 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS concreteprototype1 IMPLEMENTATION.
  METHOD clone.
    prototype = me.
  ENDMETHOD.                    "clone
ENDCLASS.                    "concreteprototype1 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS concreteprototype2 DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS concreteprototype2 DEFINITION INHERITING FROM prototype.
  PUBLIC SECTION.
    METHODS:
      clone REDEFINITION.
ENDCLASS.                    "concreteprototype2 DEFINITION
*----------------------------------------------------------------------*
*       CLASS concreteprototype2 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS concreteprototype2 IMPLEMENTATION.
  METHOD clone.
    prototype = me.
  ENDMETHOD.                    "clone
ENDCLASS.                    "concreteprototype2 IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS mainapp DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS mainapp DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      main.
ENDCLASS.                    "mainapp DEFINITION

*----------------------------------------------------------------------*
*       CLASS mainapp IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS mainapp IMPLEMENTATION.
  METHOD main.
    data: p1 type REF TO concreteprototype1
         ,c1 type REF TO concreteprototype1
         ,p2 TYPe REF TO concreteprototype2
         ,c2 type REF TO concreteprototype2
         ,id type string
         .

    field-SYMBOLS <fs> type REF TO prototype.

*   Create two instances and clone each
    create object p1 exporting id = 'I'.
    c1 ?= p1->clone( ).
    id = c1->get_id( ).
    write: / 'Cloned: {0}', id.

*   Down Casting uses assign operator ?= as it can not be checked
*   before runtine
    create object p2 exporting id = 'II'.
    c2 ?= p2->clone( ).
    id = c2->get_id( ).
    write: / 'Cloned: {0}', id.

  ENDMETHOD.                    "endmethod
ENDCLASS.                    "mainapp IMPLEMENTATION

START-OF-SELECTION.
  mainapp=>main( ).
