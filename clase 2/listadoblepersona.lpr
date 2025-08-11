program listadoblepersona;
uses crt; // Se utiliza para funciones de consola como ClrScr y ReadKey

// ---------- DEFINICIÓN DE TIPOS ----------
type
  // Registro para representar a una persona
  TPersona = record
    nombre: string;
    correo: string;
    edad: Integer;
  end;

  // Definición del puntero a nodo
  PNodo = ^TNodo;

  // Nodo de la lista doblemente enlazada
  TNodo = record
    dato: TPersona;    // Información de la persona
    anterior: PNodo;   // Apunta al nodo anterior
    siguiente: PNodo;  // Apunta al nodo siguiente
  end;

  // Estructura para la lista doblemente enlazada
  TListaDoble = record
    cabeza: PNodo;  // Primer nodo de la lista
    cola: PNodo;    // Último nodo de la lista
    tamano: Integer; // Cantidad de elementos en la lista
  end;

// ---------- OPERACIONES SOBRE LA LISTA ----------

// Inicializa la lista vacía
procedure InicializarLista(var lista: TListaDoble);
begin
  lista.cabeza := nil;
  lista.cola := nil;
  lista.tamano := 0;
end;

// Inserta un nodo al final de la lista
procedure InsertarAlFinal(var lista: TListaDoble; persona: TPersona);
var
  nuevo: PNodo;
begin
  New(nuevo); // Reserva memoria para el nuevo nodo
  nuevo^.dato := persona; // Asigna los datos de la persona
  nuevo^.siguiente := nil; // No hay nodo siguiente
  nuevo^.anterior := lista.cola; // El anterior es el actual último nodo

  if lista.cabeza = nil then // Si la lista está vacía
    lista.cabeza := nuevo // El nuevo nodo es también la cabeza
  else
    lista.cola^.siguiente := nuevo; // Enlaza el último nodo con el nuevo

  lista.cola := nuevo; // Actualiza la cola al nuevo nodo
  Inc(lista.tamano); // Incrementa el tamaño de la lista
end;

// Recorre y muestra todos los elementos de la lista
procedure MostrarLista(lista: TListaDoble);
var
  actual: PNodo;
begin
  actual := lista.cabeza;
  Writeln('Contenido de la lista:');
  while actual <> nil do
  begin
    Writeln('Nombre : ', actual^.dato.nombre);
    Writeln('Correo : ', actual^.dato.correo);
    Writeln('Edad   : ', actual^.dato.edad);
    Writeln('---------------------------');
    actual := actual^.siguiente;
  end;
  Writeln('Total de personas: ', lista.tamano);
end;

// Busca un nodo por correo electrónico
function BuscarPorCorreo(var lista: TListaDoble; correo: string): PNodo;
var
  actual: PNodo;
begin
  actual := lista.cabeza;
  while actual <> nil do
  begin
    if actual^.dato.correo = correo then
    begin
      BuscarPorCorreo := actual; // Devuelve el nodo si lo encuentra
      Exit;
    end;
    actual := actual^.siguiente; // Continúa buscando
  end;
  BuscarPorCorreo := nil; // Si no lo encuentra, devuelve nil
end;

// Edita el nombre y edad de una persona en el nodo encontrado
procedure EditarPersona(nodo: PNodo; nuevoNombre: string; nuevaEdad: Integer);
begin
  if nodo <> nil then
  begin
    nodo^.dato.nombre := nuevoNombre;
    nodo^.dato.edad := nuevaEdad;
  end;
end;

// Libera la memoria ocupada por la lista
procedure LiberarLista(var lista: TListaDoble);
var
  actual, siguiente: PNodo;
begin
  actual := lista.cabeza;
  while actual <> nil do
  begin
    siguiente := actual^.siguiente; // Guarda el siguiente nodo
    Dispose(actual); // Libera el nodo actual
    actual := siguiente; // Pasa al siguiente nodo
  end;
  // Reinicializa la lista
  lista.cabeza := nil;
  lista.cola := nil;
  lista.tamano := 0;
end;

// ---------- PROGRAMA PRINCIPAL ----------
var
  lista: TListaDoble;       // Lista doblemente enlazada
  p: TPersona;              // Persona temporal para insertar
  nodoEncontrado: PNodo;    // Puntero al nodo que se busca
begin
  ClrScr; // Limpia la pantalla
  InicializarLista(lista); // Inicializa la lista

  // --- Insertar personas ---
  p.nombre := 'Ana';
  p.correo := 'ana@example.com';
  p.edad := 25;
  InsertarAlFinal(lista, p); // Inserta a Ana

  p.nombre := 'Luis';
  p.correo := 'luis@mail.com';
  p.edad := 30;
  InsertarAlFinal(lista, p); // Inserta a Luis

  p.nombre := 'Carla';
  p.correo := 'carla@mail.com';
  p.edad := 28;
  InsertarAlFinal(lista, p); // Inserta a Carla

  // --- Mostrar la lista original ---
  Writeln('--- Lista Original ---');
  MostrarLista(lista);

  // --- Buscar y editar ---
  nodoEncontrado := BuscarPorCorreo(lista, 'luis@mail.com');
  if nodoEncontrado <> nil then
  begin
    Writeln('Persona encontrada: ', nodoEncontrado^.dato.nombre);
    EditarPersona(nodoEncontrado, 'Luis Eduardo', 31);
    Writeln('Persona editada exitosamente.');
  end
  else
    Writeln('Correo no encontrado.');

  // --- Mostrar lista actualizada ---
  Writeln;
  Writeln('--- Lista Editada ---');
  MostrarLista(lista);

  // --- Liberar memoria antes de salir ---
  LiberarLista(lista);

  ReadKey; // Espera una tecla antes de salir
end.

