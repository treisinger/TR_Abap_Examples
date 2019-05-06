class YCL_TEST_LOGGER definition
  public
  create public .

public section.

  methods SAY_WHO_YOU_ARE .
protected section.
private section.
ENDCLASS.



CLASS YCL_TEST_LOGGER IMPLEMENTATION.


METHOD say_who_you_are.

  WRITE:/ 'I am the base test logging class'.

ENDMETHOD.
ENDCLASS.
