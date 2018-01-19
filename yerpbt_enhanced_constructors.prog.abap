report ysuboops18.

class grandfather definition.

  public section.

    methods : constructor .

endclass.

class grandfather implementation.

  method constructor.

    write:/5 'I am grandfather'.

    skip.

  endmethod.

endclass.

class father definition inheriting from grandfather.

  public section.

    methods : constructor importing i_age type i optional.

endclass.

class father implementation.

  method constructor .

    call method super->constructor.

    write:/5 'I am father'.

    skip.

  endmethod.

endclass.

class son definition inheriting from father.

  public section.

    methods : constructor.

endclass.

class son implementation.

  method constructor .

    call method super->constructor.

    write:/5 'I am son'.

    skip.

  endmethod.

endclass.

start-of-selection.

  data: myson type ref to son.

 create object: myson.
