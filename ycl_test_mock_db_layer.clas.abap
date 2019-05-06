class YCL_TEST_MOCK_DB_LAYER definition
  public
  inheriting from YCL_TEST_DB_LAYER
  create public .

public section.

  methods SAY_WHO_YOU_ARE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS YCL_TEST_MOCK_DB_LAYER IMPLEMENTATION.


method SAY_WHO_YOU_ARE.

  WRITE:/ 'I am a test double of the database access layer'.



endmethod.
ENDCLASS.
