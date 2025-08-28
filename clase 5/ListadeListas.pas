program ListaDeListas;


uses 
  SysUtils;

  type// nodo para peliculas 
    PNodoPelicula = ^NodoPelicula;
    NodoPelicula = record
      nombre: string;
      siguiente: PNodoPelicula;
    end;

// Lista de peliculas (apunte a la cabeza de la lista y guardar su tamaño

    TñListaPeliculas = record
      cabeza: PNodoPelicula;
      tamaño: integer;
    end;

    /// Nodo para categorias 
    /// cada categoria tiene un nombre y una lista de peliculas
    PNodoCategoria = ^NodoCategoria;
    TNodoCategoria = record
      nombre: string;
      peliculas: TñListaPeliculas;
      siguiente: PNodoCategoria;
    end;

    ///lista de categorias 
    TListaCategorias = record 
    cabeza : PNodoCategoria;
    tamaño: integer;
    end;

    //// procedimientos

    /// inicializar una lista de peliculas vacia
    procedure InicializarListaPeliculas(var lista: TñListaPeliculas);
    begin
      lista.cabeza := nil; // no hay nodos al inicio
      lista.tamaño := 0; // tamño 0
    end;
    
    /// inciializar una lista de categorias vacia
    procedure InicializarListaCategorias(var lista: TListaCategorias);
    begin
      lista.cabeza := nil; // no hay nodos al inicio
      lista.tamaño := 0; // tamaño 0
    end;

    /// insertar una nueva categoria al inicio de la lista
    procedure InsertarCategoria(var lista: TListaCategorias; nombre: string);
    var
      nuevoNodo: PNodoCategoria;
      begin 
        New(nuevoNodo); // crear un nuevo nodo y reservar la memoria
        nuevoNodo^.nombre := nombre; // asignar el nombre
        // inicializar la lista de peliculas
        InicializarListaPeliculas(nuevoNodo^.peliculas);
        // inbsertar al inicio de la lista
        nuevoNodo^.siguiente := lista.cabeza; // apuntar al nodo actual
        lista.cabeza := nuevoNodo; // actualizar la cabeza de la lista
        Inc(lista.tamaño); // aumentar el tamaño de la lista
        end;
    // Buscar una categoria por nombre y devolver su nodo
    //function BuscarCategoria(var lista: TListaCategorias; nombre: string): PNodoCategoria;    
    procedure InsertarPelicula(var lista: TñListaPeliculas; nombre: string);
    var 
      nuevoNodo: PNodoPelicula;
    begin
      New(nuevoNodo); // crear un nuevo nodo y reservar la memoria
        nuevoNodo^.nombre := nombre; // asignar el nombre
        nuevoNodo^.siguiente := lista.cabeza; // apuntar al nodo actual
        lista.cabeza := nuevoNodo; // actualizar la cabeza de la lista
        Inc(lista.tamaño); // aumentar el tamaño de la lista
    end;
    
        //function BuscarCategoria(var lista: TListaCategorias; nombre: string): PNodoCategoria;    
    function BuscarCategoria(var lista: TListaCategorias; nombre: string): PNodoCategoria;
    var
      actual: PNodoCategoria;
    begin
      actual := lista.cabeza; // iniciar desde la cabeza de la lista
        while actual <> nil do // recorrer la lista
        begin 
            if actual^.nombre = nombre then // si el nombre coincide
            begin
                Result := actual; // devolver el nodo encontrado
                Exit; // salir de la funcion
                actual := actual^.siguiente; // avanzar al siguiente nodo
            end;
                Result := nil; // si no se encuentra, devolver nil
    
                end;


// mostar todo el catalogo (categorias y sus peliculas)
procedure MostrarCatalogo(var lista: TListaCategorias);
var 
    cat: PNodoCategoria;
    peli: PNodoPelicula;
begin
    if lista.tamaño = 0 then
        Writeln('El catalogo esta vacio.')
    else
    begin
        cat := lista.cabeza; // iniciar desde la cabeza de la lista de categorias
        while cat <> nil do // recorrer las categorias
        begin
            Writeln('Categoria: ', cat^.nombre); // mostrar el nombre de la categoria
            peli := cat^.peliculas.cabeza; // iniciar desde la cabeza de la lista de peliculas
            if peli = nil then
                Writeln('  No hay peliculas en esta categoria.')
            else
                while peli <> nil do // recorrer las peliculas
                begin
                    Writeln('  Pelicula: ', peli^.nombre); // mostrar el nombre de la pelicula
                    peli := peli^.siguiente; // avanzar al siguiente nodo de peliculas
                end;
            cat := cat^.siguiente; // avanzar a la siguiente categoria
        end;
    end;


    // programa principal

    var 
        listaCategorias: TListaCategorias;
        categoria: PNodoCategoria; // auxiliar para trabajar con categorias
    begin
        InicializarListaCategorias(listaCategorias); // inicializar la lista de categorias
    // insertar Categorias
        InsertarCategoria(listaCategorias, 'Accion');
        InsertarCategoria(listaCategorias, 'Comedia');
        InsertarCategoria(listaCategorias, 'Terror');

        // insertar peliculas en la categoria de Accion
        categoria := BuscarCategoria(listaCategorias, 'Accion');
        if categoria <> nil then
        begin
            InsertarPelicula(categoria^.peliculas, 'Duro de matar');
            InsertarPelicula(categoria^.peliculas, 'Rapidos y Furiosos');
        end;

        // insertar peliculas en la categoria de Comedia
        categoria := BuscarCategoria(listaCategorias, 'Comedia');
        if categoria <> nil then
        begin
            InsertarPelicula(categoria^.peliculas, 'La Máscara');
            InsertarPelicula(categoria^.peliculas, 'Supercool');
        end;


        // mostrar el catalogo completo
        MostrarCatalogo(listaCategorias);
        readln; // esperar a que el usuario presione Enter
    end.