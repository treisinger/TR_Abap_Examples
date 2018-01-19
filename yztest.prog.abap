*&---------------------------------------------------------------------*
*& Report  YZTEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report yztest.

class obj1 definition.

  public section.
    data color type name.

    methods set_color importing i_color type name.
    methods get_color returning value(r_color) type name.

endclass.

class obj1 implementation.
  method set_color.

    color = i_color.

  endmethod.
  method get_color.

    r_color = color.

  endmethod.

endclass.

START-OF-SELECTION.

data l_obj1 type REF TO obj1.

create object l_obj1.

l_obj1->set_color( 'BLUE' ).
data(color) = l_obj1->get_color( ).
write:/ 'Color is:', color.
