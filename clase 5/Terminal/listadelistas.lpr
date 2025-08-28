program listadelistas;

uses
  SysUtils;

type
  // ------------------- NODOS Y LISTAS -------------------

  // Nodo para películas (cada película tiene un nombre y un puntero al siguiente nodo)
  PNodoPelicula = ^TNodoPelicula;
  TNodoPelicula = record
    nombre: string;
    siguiente: PNodoPelicula;
  end;

  // Lista de películas (apunta a la cabeza de la lista y guarda su tamaño)
  TListaPeliculas = record
    cabeza: PNodoPelicula;
    tamano: Integer;
  end;

  // Nodo para categorías (cada categoría tiene un nombre, su propia lista de películas y un puntero al siguiente nodo)
  PNodoCategoria = ^TNodoCategoria;
  TNodoCategoria = record
    nombre: string;
    peliculas: TListaPeliculas;
    siguiente: PNodoCategoria;
  end;

  // Lista de categorías (apunta a la cabeza y guarda tamaño)
  TListaCategorias = record
    cabeza: PNodoCategoria;
    tamano: Integer;
  end;

// ------------------- PROCEDIMIENTOS -------------------

// Inicializa una lista de películas vacía
procedure InicializarListaPeliculas(var lista: TListaPeliculas);
begin
  lista.cabeza := nil;   // No hay nodos al inicio
  lista.tamano := 0;     // Tamaño 0
end;

// Inicializa una lista de categorías vacía
procedure InicializarListaCategorias(var lista: TListaCategorias);
begin
  lista.cabeza := nil;
  lista.tamano := 0;
end;

// Inserta una nueva categoría en la lista de categorías
procedure InsertarCategoria(var lista: TListaCategorias; nombre: string);
var
  nuevo: PNodoCategoria;
begin
  new(nuevo);                          // Reservar memoria para el nuevo nodo
  nuevo^.nombre := nombre;             // Guardar nombre de la categoría
  InicializarListaPeliculas(nuevo^.peliculas); // Inicializar su lista de películas vacía
  nuevo^.siguiente := lista.cabeza;    // Insertar al inicio de la lista
  lista.cabeza := nuevo;               // Nuevo nodo es la nueva cabeza
  Inc(lista.tamano);                   // Aumentar tamaño
end;

// Inserta una nueva película en la lista de películas
procedure InsertarPelicula(var lista: TListaPeliculas; nombre: string);
var
  nuevo: PNodoPelicula;
begin
  new(nuevo);                          // Reservar memoria para el nodo
  nuevo^.nombre := nombre;             // Guardar nombre de la película
  nuevo^.siguiente := lista.cabeza;    // Insertar al inicio
  lista.cabeza := nuevo;               // Nuevo nodo es la nueva cabeza
  Inc(lista.tamano);                   // Aumentar tamaño
end;

// Busca una categoría por su nombre y devuelve el puntero a ella
function BuscarCategoria(lista: TListaCategorias; nombre: string): PNodoCategoria;
var
  actual: PNodoCategoria;
begin
  actual := lista.cabeza;              // Comenzar desde la cabeza
  while actual <> nil do               // Recorrer la lista
  begin
    if actual^.nombre = nombre then    // Si coincide el nombre, devolver nodo
      Exit(actual);
    actual := actual^.siguiente;       // Avanzar al siguiente
  end;
  Result := nil;                       // Si no se encontró, devolver nil
end;

// Muestra todo el catálogo (categorías y sus películas)
procedure MostrarCatalogo(lista: TListaCategorias);
var
  cat: PNodoCategoria;
  pel: PNodoPelicula;
begin
  if lista.tamano = 0 then
  begin
    writeln('No hay categorías registradas.');
    exit;
  end;

  cat := lista.cabeza;                 // Comenzar desde la primera categoría
  while cat <> nil do
  begin
    writeln('=== ', cat^.nombre, ' ===');  // Mostrar nombre de categoría

    if cat^.peliculas.tamano = 0 then
      writeln('  (No hay películas en esta categoría)')
    else
    begin
      pel := cat^.peliculas.cabeza;    // Recorrer la lista de películas
      while pel <> nil do
      begin
        writeln('  - ', pel^.nombre);  // Mostrar nombre de película
        pel := pel^.siguiente;         // Pasar a la siguiente
      end;
    end;

    writeln;                           // Línea en blanco
    cat := cat^.siguiente;             // Pasar a la siguiente categoría
  end;
end;

// ------------------- PROGRAMA PRINCIPAL -------------------

var
  listaCategorias: TListaCategorias;   // Lista principal de categorías
  cat: PNodoCategoria;                 // Variable auxiliar para trabajar con categorías
begin
  InicializarListaCategorias(listaCategorias);

  // Insertar categorías
  InsertarCategoria(listaCategorias, 'Acción');
  InsertarCategoria(listaCategorias, 'Comedia');
  InsertarCategoria(listaCategorias, 'Terror');

  // Insertar películas en la categoría "Acción"
  cat := BuscarCategoria(listaCategorias, 'Acción');
  if cat <> nil then
  begin
    InsertarPelicula(cat^.peliculas, 'Duro de Matar');
    InsertarPelicula(cat^.peliculas, 'Rápidos y Furiosos');
  end;

  // Insertar películas en la categoría "Comedia"
  cat := BuscarCategoria(listaCategorias, 'Comedia');
  if cat <> nil then
  begin
    InsertarPelicula(cat^.peliculas, 'La Máscara');
    InsertarPelicula(cat^.peliculas, 'Supercool');
  end;

  // Mostrar todo el catálogo
  MostrarCatalogo(listaCategorias);

  readln; // Espera a que el usuario presione Enter antes de cerrar
end.
